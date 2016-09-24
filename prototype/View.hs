{-# LANGUAGE OverloadedStrings #-}

import SDL
import qualified SDL.Raw as Raw
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import Control.Monad (unless)
import SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import Data.Maybe
import Data.List as L
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import SDL.Image
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Traversable
import Data.Either
import Data.Word

import Control.Monad.State as S
import Linear.V2
import Foreign.C.Types
import GHC.Exts
import Control.Applicative
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL


import Types


type Element = ([Int], [(Rectangle Int, Surface)])

data RenderState = RenderState { rPath :: [Int]
                               , rX :: Int
                               , rY :: Int
                               , rW :: Int
                               , rElements :: [Element]
                               }

data RenderCache = RenderCache { rcSurfaceDim :: V2 CInt
                               , rcTexture :: Texture
                               -- ^ background and foreground textures
                               , rcElements :: [Element]
                               }


fpath = "/usr/share/fonts/dejavu/DejaVuSans.ttf"
fpathMono = "/usr/share/fonts/dejavu/DejaVuSansMono.ttf"
sectionPaddingLeft = 20
titleFontSize = 20
fontSize = 16
paddingBottom = 10
paddingLine = 3


main :: IO ()
main = do
  b <- parseBlock <$> BL.getContents
  case b of
    Left err -> putStrLn err
    Right block -> do
      initialize [InitEvents, InitVideo]
      window <- createWindow "( ﾉ ﾟｰﾟ)ﾉ ☀" defaultWindow {windowResizable = True}
      renderer <- createRenderer window (-1) defaultRenderer
      TTF.init
      appLoop block Nothing [] 0 renderer
      TTF.quit

appLoop :: Block
        -> Maybe RenderCache
        -> [Int]
        -> Int
        -> Renderer
        -> IO ()
appLoop doc rcache' selection' yOffset' renderer = do
  event <- waitEvent
  events <- pollEvents
  rvp <- SDL.get (rendererViewport renderer)

  let epls = map eventPayload (event:events)
      (rw, rh) = maybe (200, 200) (\x -> (rectW x, rectH x)) rvp
      elements' = maybe [] rcElements rcache'
      (selection, yOffset) =
        foldl (\(s, y) epl -> navigate doc elements' s y rh epl) (selection', yOffset') epls

  -- todo: add link selection there, update active element here
  mapM_ (handleLinkActions doc elements' selection yOffset) epls

  rcache <- let render = renderDoc renderer yOffset rw rh selection (any rerenderNeeded epls) doc in
    case (rcache', selection /= selection' || yOffset /= yOffset' || any rerenderNeeded epls) of
      (Just rc, True) -> render (Just rc)
      (Just rc, _) -> pure rc
      _ -> render Nothing

  when (any redrawNeeded epls
        || any rerenderNeeded epls
        || yOffset /= yOffset'
        || selection /= selection') $ do
    rendererDrawColor renderer $= V4 0x06 0x10 0x14 0
    clear renderer
    let (V2 w h) = rcSurfaceDim rcache
    copy renderer (rcTexture rcache) Nothing Nothing
    present renderer

  unless (any ((==) (Just "q") . textInput) epls) $
    appLoop doc (Just rcache) selection yOffset renderer


inRectangle :: (Integral a, Integral b) => Point V2 a -> Rectangle b -> Bool
inRectangle (P (V2 x y)) r = and [ x >= rectX r
                            , x <= rectX r + rectW r
                            , y >= rectY r
                            , y <= rectY r + rectH r]

atPoint :: Integral a => Point V2 a -> [Element] -> [[Int]]
atPoint p = map fst . filter (any (inRectangle p . fst) . snd)

getElem :: Block -> [Int] -> Maybe (Either Block Inline)
getElem x [] = Just $ Left x
getElem (BParagraph f) [n] = if n < length f
                             then Just $ Right $ f !! n
                             else Nothing
getElem (BSection _ bs) (p:ps) = if p < length bs
                                 then getElem (bs !! p) ps
                                 else Nothing
getElem _ _ = Nothing

getLink :: Block -> [Int] -> Maybe Inline
getLink b p = case getElem b p of
  Just (Right l@(ILink _ _)) -> Just l
  _ -> Nothing

getSection :: Block -> [Int] -> Maybe Block
getSection b p = case getElem b p of
  Just (Left s@(BSection _ _)) -> Just s
  _ -> Nothing


type Nav = (Block -> [Int] -> Maybe [Int])
type Nav' = (Block -> [Int] -> [Int])

tryNav :: Nav
tryNav b p = getElem b p >> pure p

sDown :: Nav
sDown b p = tryNav b (p ++ [0])

sUp :: Nav
sUp b [] = Nothing
sUp b p = Just $ L.init p

sNext :: Nav
sNext b [] = Nothing
sNext b p = tryNav b $ L.init p ++ [last p + 1]

sPrev :: Nav
sPrev b [] = Nothing
sPrev b p | last p > 0 = tryNav b $ L.init p ++ [last p - 1]
          | otherwise = Nothing

sLast :: Nav
sLast b p = maybe (tryNav b p) (sLast b) (sNext b p)

sFirst :: Nav
sFirst b p = maybe (tryNav b p) (sFirst b) (sPrev b p)

sPrevCycle :: Nav
sPrevCycle b p = sPrev b p <|> sLast b p

sNextCycle :: Nav
sNextCycle b p = sNext b p <|> sFirst b p

sDownNext :: Nav
sDownNext b p = sDown b p <|> sNext b p

sLastChild :: Nav
sLastChild b p = sDown b p >>= sLast b

sFirstChild :: Nav
sFirstChild b p = sDown b p >>= sFirst b

sVeryLastChild :: Nav
sVeryLastChild b p = maybe (tryNav b p) (sVeryLastChild b) (sLastChild b p)

sNextUp :: Nav
sNextUp b p = sNext b p <|> (sUp b p >>= sNextUp b)

-- | Info-style prev
sPrevInfo :: Nav
sPrevInfo b p = (sPrev b p >>= sVeryLastChild b)
             <|> sUp b p
             <|> sVeryLastChild b p

-- | Info-style next
sNextInfo :: Nav
sNextInfo b p = sDown b p
                <|> sNextUp b p
                <|> pure []

-- | Repeat a navigation action until we reach a section
tillSection :: Nav -> Nav
tillSection f b p = do
  p' <- f b p
  (getSection b p' >> pure p') <|> tillSection f b p'

-- | Repeat a navigation action until we reach a link
tillLink :: Nav -> Nav
tillLink f b p = do
  p' <- f b p
  (getLink b p' >> pure p') <|> tillLink f b p'

handleLinkActions :: Block -> [Element] -> [Int] -> Int -> EventPayload -> IO ()
handleLinkActions b es s y
  (MouseButtonEvent
    (MouseButtonEventData _ Pressed _ ButtonLeft 1 (P (V2 cx cy)))) = do
  case catMaybes $ map (getLink b) $ atPoint (P (V2 cx (cy - fromIntegral y))) es of
    [l@(ILink txt target)] -> do
      BL.putStrLn (printInline l)
      hFlush stdout
    _ -> pure ()
handleLinkActions b es s y (KeyboardEvent (KeyboardEventData _ Pressed _ ks))
  | unwrapKeycode (keysymKeycode ks) == 13 = case getElem b s of
      Just (Left b) -> BL.putStrLn (printBlock b) >> hFlush stdout
      Just (Right i) -> BL.putStrLn (printInline i) >> hFlush stdout
      Nothing -> pure ()
  | otherwise = pure ()
handleLinkActions b es s y epl = pure ()


-- todo: move selection on scroll
navigate :: Block -> [Element] -> [Int] -> Int -> Int -> EventPayload -> ([Int], Int)
navigate b es p y h (MouseWheelEvent (MouseWheelEventData _ _ (V2 _ i))) =
  (p, y + (div h 4) * fromIntegral i)
navigate b es p y h epl = maybe (p, y) id $ do
  navigateStruct <|> (navigatePos =<< textInput epl)
  where
    navigatePos :: Text.Text -> Maybe ([Int], Int)
    navigatePos c = do
      y' <- lookup c [ ("f", - (div h 4)),
                       ("b", div h 4),
                       (" ", - h)]
      pure (p, y + y')
    navigateStruct :: Maybe ([Int], Int)
    navigateStruct = do
      p <- case epl of
        (KeyboardEvent (KeyboardEventData _ Pressed _ ks)) -> do
          -- todo: check modifiers
          let ksm = keysymModifier ks
          f <- lookup (unwrapKeycode (keysymKeycode ks),
                       keyModifierLeftShift ksm || keyModifierRightShift ksm)
               [((9, False), sNextInfo),
                ((9, True), sPrevInfo)]
          tillLink f b p
        _ -> mempty
        <|> do
          ti <- textInput epl
          f <- lookup ti [ ("n", sNextCycle), ("p", sPrevCycle)
                    , ("u", sUp), ("d", sDown)
                    , ("]", sNextInfo), ("[", sPrevInfo)]
          tillSection f b p
      elemCoords <- map fst <$> lookup p es
      let viewTop = -y
          viewBottom = viewTop + h
          viewHeight = viewBottom - viewTop
          elemTop = top elemCoords
          elemBottom = bottom elemCoords
          elemHeight = elemBottom - elemTop
          newTop = case (viewTop <= elemTop,
                         viewBottom >= elemBottom,
                         viewHeight > elemHeight) of
            -- fits on the current screen
            (True, True, _) -> viewTop
            -- doesn't fit currently, but would feet in a screen
            (_, _, True) -> elemTop - round (fromIntegral (viewHeight - elemHeight) * 0.8)
            _ -> elemTop
      pure (p, min 0 (-newTop))

redrawNeeded :: EventPayload -> Bool
redrawNeeded (WindowShownEvent _) = True
redrawNeeded (WindowExposedEvent _) = True
redrawNeeded (WindowResizedEvent _) = True
redrawNeeded (MouseWheelEvent _) = True
redrawNeeded (KeyboardEvent _) = True
redrawNeeded _ = False

rerenderNeeded :: EventPayload -> Bool
rerenderNeeded (WindowResizedEvent _) = True
rerenderNeeded (WindowShownEvent _) = True
rerenderNeeded _ = False


textInput :: EventPayload -> Maybe Text.Text
textInput (TextInputEvent tid) = Just $ textInputEventText tid
textInput _ = Nothing


rect :: (Integral a, Integral b, Integral c, Integral d, Integral e) =>
        a -> b -> c -> d -> Rectangle e
rect a b c d = Rectangle (P (V2 (fromIntegral a) (fromIntegral b)))
               (V2 (fromIntegral c) (fromIntegral d))

rectX :: (Integral a, Integral b) => Rectangle a -> b
rectX (Rectangle (P (V2 x _)) (V2 _ _)) = fromIntegral x

rectY :: (Integral a, Integral b) => Rectangle a -> b
rectY (Rectangle (P (V2 _ y)) (V2 _ _)) = fromIntegral y

rectW :: (Integral a, Integral b) => Rectangle a -> b
rectW (Rectangle (P (V2 _ _)) (V2 w _)) = fromIntegral w

rectH :: (Integral a, Integral b) => Rectangle a -> b
rectH (Rectangle (P (V2 _ _)) (V2 _ h)) = fromIntegral h

rectP :: (Integral a, Integral b) => Rectangle a -> Point V2 b
rectP r = (P (V2 (rectX r) (rectY r)))

vX :: (Integral a, Integral b) => V2 a -> b
vX (V2 x _) = fromIntegral x

vY :: (Integral a, Integral b) => V2 a -> b
vY (V2 _ y) = fromIntegral y


textSurface :: MonadIO m => String -> Int -> String -> Raw.Color -> m Surface
textSurface str size font c = do
  font <- openFont font size
  surf <- renderUTF8Shaded font str c (clr 0x06 0x10 0x14 0)
  closeFont font
  pure surf

clr = Raw.Color



eBottom :: Integral a => [Element] -> a
eBottom = fromIntegral . foldl max 0 .
         concatMap (\(_, rs) -> map (\(r, _) -> rectY r + rectH r) rs)

top :: Integral a => [Rectangle a] -> a
top [] = 0
top (r:rs) = fromIntegral . foldl min (rectY r) $ map rectY rs

bottom :: Integral a => [Rectangle a] -> a
bottom [] = 0
bottom (r:rs) = fromIntegral . foldl min (rectY r + rectH r) $ map (\r -> rectY r + rectH r) rs



overlap :: Integral a => Rectangle a -> Rectangle a -> Maybe (Rectangle a, Rectangle a)
overlap (Rectangle (P (V2 x1 y1)) (V2 w1 h1)) (Rectangle (P (V2 x2 y2)) (V2 w2 h2)) =
  let x = max x1 x2
      y = max y1 y2
      w = min (x1 + w1) (x2 + w2) - x
      h = min (y1 + h1) (y2 + h2) - y
  in if w > 0 && h > 0
     then Just (rect (x - x1) (y - y1) w h, rect (x - x2) (y - y2) w h)
     else Nothing

makeTexture :: Renderer -> [Int] -> Int -> Int -> Int -> [Element] -> IO Texture
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

renderDoc :: Renderer -> Int -> Int -> Int -> [Int] -> Bool -> Block -> Maybe RenderCache -> IO RenderCache
renderDoc renderer y rw rh active True b rcache = do
  elements <- case rcache of
    Just rc -> do
      -- mapM_ (\(_, rs) -> mapM_ (\(_, s) -> freeSurface s) rs) $ rcElements rc
      destroyTexture $ rcTexture rc
      rElements . snd <$> runStateT (renderBlock (rcElements rc) b []) (RenderState [] 0 0 rw [])
    Nothing -> rElements . snd <$> runStateT (renderBlock [] b []) (RenderState [] 0 0 rw [])

  -- elements <- rElements . snd <$> runStateT (renderBlock b []) (RenderState [] 0 0 rw [])
  t <- makeTexture renderer active y rw rh elements
  pure $ RenderCache (V2 0 0) t elements
renderDoc renderer y rw rh active False b (Just rcache) = do
  destroyTexture $ rcTexture rcache
  t <- makeTexture renderer active y rw rh (rcElements rcache)
  pure $ rcache { rcTexture = t }
renderDoc renderer y rw rh active redraw b rcache = undefined -- should not happen

freeOldSurfaces :: MonadIO m => [Int] -> [Element] -> m ()
freeOldSurfaces p e = case lookup p e of
  Just surfaces -> mapM_ (freeSurface . snd) surfaces
  Nothing -> pure ()

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
    Nothing -> imgLoad imgPath
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
