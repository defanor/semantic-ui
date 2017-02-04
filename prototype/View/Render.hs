module View.Render where

import SDL
import qualified SDL.Raw as Raw
import qualified SDL.Image as Image
import SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import Foreign.C.Types
import Linear.V2
import Control.Monad.State as S
import qualified Codec.Binary.UTF8.String as UTF8

import Types
import View.Common
import View.Rectangle


fpath = "/usr/share/fonts/dejavu/DejaVuSans.ttf"
fpathMono = "/usr/share/fonts/dejavu/DejaVuSansMono.ttf"
sectionPaddingLeft = 20
titleFontSize = 20
fontSize = 16
paddingBottom = 10
paddingLine = 3


textSurface :: MonadIO m => String -> Int -> String -> Raw.Color -> m Surface
textSurface str size font c = do
  font <- openFont font size
  let str' = if UTF8.isUTF8Encoded str then UTF8.decodeString str else str
  surf <- renderUTF8Shaded font str' c (clr 0x06 0x10 0x14 0)
  closeFont font
  pure surf

clr = Raw.Color

eBottom :: Integral a => [Element] -> a
eBottom = fromIntegral . foldl max 0 .
         concatMap (\(_, rs) -> map (\(r, _) -> rectY r + rectH r) rs)


-- | Turns ['Element'] into a texture
makeTexture :: Renderer
            -> [Int]
            -- ^ path to the active element
            -> Int
            -- ^ current Y offset
            -> Int
            -- ^ renderer viewpoint width
            -> Int
            -- ^ renderer viewpoint height
            -> [Element]
            -- ^ elements
            -> IO Texture
makeTexture renderer active y rw rh es = do
  bgSurf <- createRGBSurface (V2 (fromIntegral rw) (fromIntegral rh)) RGBA4444
  mapM_ (\(p, rs) ->
           mapM_ (\(r, s) -> case overlap (rect 0 (- y) rw rh) r of
                   Nothing -> pure ()
                   Just (r1, r2) -> do
                     when (active == p) $ do
                       surfaceFillRect bgSurf
                         (Just $ rect (rectX r1) (rectY r1 - 1) (rectW r1) 1)
                         (V4 0xA0 0xD0 0xB0 0xFF)
                       surfaceFillRect bgSurf
                         (Just $ rect (rectX r1) (rectY r1 + rectH r1) (rectW r1) 1)
                         (V4 0xA0 0xB0 0xD0 0xFF)
                       surfaceFillRect bgSurf
                         (Just $ fmap fromIntegral r1)
                         (V4 0x06 0x10 0x14 0)
                     surfaceBlit s
                       (Just $ fmap fromIntegral r2)
                       bgSurf
                       (Just (P (V2 (rectX r1) (rectY r1))))
                 )
           rs
        )
    es
  bgTexture <- createTextureFromSurface renderer bgSurf
  freeSurface bgSurf
  pure bgTexture

-- | [Re]renders a document
renderDoc :: Renderer
          -- ^ a renderer
          -> Int
          -- ^ current Y offset
          -> Int
          -- ^ renderer viewpoint width
          -> Int
          -- ^ renderer viewpoint height
          -> [Int]
          -- ^ path to the active element
          -> Bool
          -- ^ rerender is required
          -> Block
          -- ^ root block
          -> Maybe RenderCache
          -- ^ previous cache
          -> IO RenderCache
renderDoc renderer y rw rh active True b rcache = do
  elements <- case rcache of
    Just rc -> do
      destroyTexture $ rcTexture rc
      rElements . snd <$> runStateT (renderBlock (rcElements rc) b []) (RenderState [] 0 0 rw [])
    Nothing -> rElements . snd <$> runStateT (renderBlock [] b []) (RenderState [] 0 0 rw [])
  t <- makeTexture renderer active y rw rh elements
  pure $ RenderCache (V2 0 0) t elements
renderDoc renderer y rw rh active False b (Just rcache) = do
  destroyTexture $ rcTexture rcache
  t <- makeTexture renderer active y rw rh (rcElements rcache)
  pure $ rcache { rcTexture = t }
renderDoc renderer y rw rh active redraw b rcache = undefined -- should not happen

-- | Renders a block
renderBlock :: [Element] -> Block -> [Int] -> StateT RenderState IO ()
renderBlock old (BSection title blocks) path = do
  titleSurf <- case lookup path old of
    Just [(_, ts)] -> pure ts
    Nothing -> textSurface title (max fontSize (titleFontSize - length path)) fpath
      $ clr 0xA0 0xD0 0xB0 0
  titleSurfDim@(V2 tw th) <- surfaceDimensions titleSurf
  rs <- S.get
  put $ rs { rX = rX rs + sectionPaddingLeft,
             rY = rY rs + fromIntegral th + paddingBottom,
             rW = rW rs - sectionPaddingLeft }
  zipWithM (renderBlock old) blocks (map ((++) path . pure) [0..])
  rs' <- S.get
  put $ rs' { rX = rX rs,
              rW = rW rs,
              rElements =
                (path, [(rect
                          (rX rs)
                          (rY rs)
                          (rW rs)
                          (eBottom (rElements rs') - rY rs),
                         titleSurf)])
                : rElements rs'}
renderBlock old (BImage imgPath) path = do
  s <- case lookup path old of
    Just [(_, surf)] -> pure surf
    Nothing -> Image.load imgPath
  (V2 iw ih) <- surfaceDimensions s
  rs <- S.get
  put $ rs { rY = rY rs + fromIntegral ih + paddingBottom,
             rElements = (path, [(rect (rX rs) (rY rs) iw ih, s)]) : rElements rs }
renderBlock old (BParagraph inlines) path = do
  rs <- S.get
  zipWithM (renderInline old (rX rs)) inlines (map ((++) path . pure) [0..])
  rs' <- S.get
  put $ rs' { rX = rX rs,
              rW = rW rs,
              rY = eBottom (rElements rs') + paddingBottom,
              rElements = (path, []) : rElements rs'}
renderBlock old (BCode lang str) path = do
  rs <- S.get
  zipWithM (renderLine old) (lines str) (map ((++) path . pure) [0..])
  rs' <- S.get
  put $ rs' { rX = rX rs,
              rY = rY rs' + paddingBottom,
              rW = rW rs,
              rElements = (path, []) : rElements rs'}

renderLine :: [Element] -> String -> [Int] -> StateT RenderState IO ()
renderLine old str path = do
  surf <- case lookup path old of
    Just [(_, s)] -> pure s
    Nothing -> textSurface (if str == "" then " " else str) fontSize fpathMono (clr 0xD0 0xE0 0xE0 0)
  (V2 iw ih) <- surfaceDimensions surf
  rs <- S.get
  put $ rs { rY = rY rs + fromIntegral ih + paddingLine,
             rElements = (path, [(rect (rX rs) (rY rs) iw ih, surf)]) : rElements rs }

renderInline :: [Element] -> Int -> Inline -> [Int] -> StateT RenderState IO ()
renderInline old origX (IText txt) = renderInline' old fpath origX txt (clr 0xC0 0xC0 0xB0 0)
renderInline old origX (ILink txt _) = renderInline' old fpath origX txt (clr 0x80 0xC0 0xB0 0)
renderInline old origX (ICode _ txt) = renderInline' old fpathMono origX txt (clr 0xD0 0xE0 0xE0 0)

renderInline' :: [Element] -> String -> Int -> String -> Raw.Color -> [Int] -> StateT RenderState IO ()
renderInline' old font origX txt c path = do
  space <- textSurface " " fontSize font c
  sDim <- surfaceDimensions space
  freeSurface space
  ws <- case lookup path old of
    Just es -> pure $ map snd es
    Nothing -> mapM (\w -> textSurface w fontSize font c) (words txt)
  dims <- mapM surfaceDimensions ws
  coordinates <- mapM (placeWord origX sDim) dims
  rs <- S.get
  put $ rs { rElements = (path, zip coordinates ws) : rElements rs }

-- always prepends space or newline
placeWord :: Int -> V2 CInt -> V2 CInt -> StateT RenderState IO (Rectangle Int)
placeWord origX sDim wDim = do
  rs <- S.get
  let spaceWidth = vX sDim
      hsw = fromIntegral spaceWidth / 2
      x = rX rs + vX wDim
      (st, c) = if (x > rW rs)
                then (rs { rX = origX + vX wDim + spaceWidth, rY = rY rs + vY wDim + paddingLine},
                      rect origX (rY rs + vY wDim + paddingLine) (vX wDim) (vY wDim))
                else (rs { rX = x + spaceWidth},
                      rect (rX rs + round hsw) (rY rs) (vX wDim) (vY wDim))
  put st
  pure c
