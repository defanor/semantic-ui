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

import Types


type Element = ([Int], [(Rectangle Int, Surface)])

data RenderState = RenderState { rPath :: [Int]
                               , rX :: Int
                               , rY :: Int
                               , rW :: Int
                               , rElements :: [Element]
                               }


fpath = "/usr/share/fonts/dejavu/DejaVuSans.ttf"
sectionPaddingLeft = 20
titleFontSize = 20
fontSize = 16


main :: IO ()
main = do
  block <- readLn
  initialize [InitEvents, InitVideo]
  window <- createWindow "( ﾉ ﾟｰﾟ)ﾉ ☀" defaultWindow {windowResizable = True}
  renderer <- createRenderer window (-1) defaultRenderer
  TTF.init
  appLoop block Nothing [] [] 0 renderer
  TTF.quit


appLoop :: Block
        -> Maybe Surface
        -> [Element]
        -> [Int]
        -> Int
        -> Renderer
        -> IO ()
appLoop doc surf' elements' selection' yOffset' renderer = do
  event <- waitEvent
  rvp <- SDL.get (rendererViewport renderer)

  let epl = eventPayload event
      (rw, rh) = maybe (200, 200) (\x -> (rectW x, rectH x)) rvp
      (selection, yOffset) = navigate doc elements' selection' yOffset' epl

  -- todo: add link selection there, update active element here
  handleLinkActions doc elements' selection yOffset epl

  (surf, elements) <-
    let render = renderDoc selection elements' (rerenderNeeded epl) rw doc in
      case (surf', selection /= selection' || rerenderNeeded epl) of
        (Just s, True) -> freeSurface s >> render
        (Just s, _) -> pure (s, elements')
        _ -> render

  -- show
  when (redrawNeeded epl
        || rerenderNeeded epl
        || yOffset /= yOffset'
        || selection /= selection') $ do
    rendererDrawColor renderer $= V4 0x10 0x10 0x10 0
    clear renderer
    (V2 w h) <- surfaceDimensions surf
    -- todo: it's unnecessary to do this on every scroll, optimize
    texture <- createTextureFromSurface renderer surf
    copy renderer texture Nothing
      (Just (rect 0 yOffset w h))
    destroyTexture texture
    present renderer

  unless (textInput epl == Just "q") $
    appLoop doc (Just surf) elements selection yOffset renderer


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


handleLinkActions :: Block -> [Element] -> [Int] -> Int -> EventPayload -> IO ()
handleLinkActions b es s y
  (MouseButtonEvent
    (MouseButtonEventData _ Pressed _ ButtonLeft 1 (P (V2 cx cy)))) = do
  case catMaybes $ map (getLink b) $ atPoint (P (V2 cx (cy - fromIntegral y))) es of
    [ILink txt target] -> do
      putStrLn target
      hFlush stdout
    _ -> pure ()
handleLinkActions b es s y epl = pure ()


-- todo: move selection on scroll
navigate :: Block -> [Element] -> [Int] -> Int -> EventPayload -> ([Int], Int)
navigate b es p y (MouseWheelEvent (MouseWheelEventData _ _ (V2 _ i))) =
  (p, y + 50 * fromIntegral i)
navigate b es p y epl = maybe (p, y) id $ do
  ti <- textInput epl
  navigateStruct ti <|> navigatePos ti
  where
    navigatePos :: Text.Text -> Maybe ([Int], Int)
    navigatePos c = do
      f <- lookup c [ ("f", (flip (-) 50)), ("b", (+ 50)) ]
      pure (p, f y)
    navigateStruct :: Text.Text -> Maybe ([Int], Int)
    navigateStruct c = do
      f <- lookup c [ ("n", sNextCycle), ("p", sPrevCycle)
                    , ("u", sUp), ("d", sDown)
                    , ("]", sNextInfo), ("[", sPrevInfo)]
      p <- tillSection f b p
      t <- top . map fst <$> lookup p es
      pure (p, -t)

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
textSurface str size font clr = do
  font <- openFont fpath size
  surf <- renderUTF8Solid font str $ clr
  closeFont font
  pure surf

clr = Raw.Color



bottom :: Integral a => [Element] -> a
bottom = fromIntegral . foldl max 0 .
         concatMap (\(_, rs) -> map (\(r, _) -> rectY r + rectH r) rs)

top :: Integral a => [Rectangle a] -> a
top [] = 0
top (r:rs) = fromIntegral . foldl min (rectY r) $ map rectY rs

renderDoc :: [Int] -> [Element] -> Bool -> Int -> Block -> IO (Surface, [Element])
renderDoc active elems redraw w b = do
  -- todo: no need to blit all the elements all the time, optimize it
  es <- getElements redraw
  surf <- createRGBSurface (V2 (fromIntegral w) (bottom es)) RGBA4444
  mapM_ (\(p, rs) -> do
            when (active == p) $ surfaceFillRects
              surf
              (fromList $ map (fmap fromIntegral . fst) rs)
              (V4 0x0 0x0 0x0 0xFF)
            mapM_(\(r, s) -> surfaceBlit s Nothing surf (Just $ rectP r)) rs)
    es
  pure (surf, es)
  where
    getElements :: Bool -> IO [Element]
    getElements True = do
      mapM_ (\(_, rs) -> mapM_ (\(_, s) -> freeSurface s) rs) elems
      rElements . snd <$> runStateT (renderBlock b []) (RenderState [] 0 0 w [])
    getElements False = pure elems

renderBlock :: Block -> [Int] -> StateT RenderState IO ()
renderBlock (BSection title blocks) path = do
  titleSurf <- textSurface title titleFontSize fpath $ clr 0xb0 0x80 0x80 0
  titleSurfDim@(V2 tw th) <- surfaceDimensions titleSurf
  rs <- S.get
  put $ rs { rX = rX rs + sectionPaddingLeft,
             rY = rY rs + fromIntegral th,
             rW = rW rs - sectionPaddingLeft }
  zipWithM renderBlock blocks (map ((++) path . pure) [0..])
  rs' <- S.get
  put $ rs' { rX = rX rs,
              rW = rW rs,
              rElements =
                (path, [(rect
                          (rX rs)
                          (rY rs)
                          (rW rs)
                          (bottom (rElements rs') - rY rs),
                         titleSurf)])
                : rElements rs'}
renderBlock (BImage imgPath) path = do
  s <- imgLoad imgPath
  (V2 iw ih) <- surfaceDimensions s
  rs <- S.get
  put $ rs { rY = rY rs + fromIntegral ih,
             rElements = (path, [(rect (rX rs) (rY rs) iw ih, s)]) : rElements rs }
renderBlock (BParagraph inlines) path = do
  rs <- S.get
  zipWithM (renderInline (rX rs)) inlines (map ((++) path . pure) [0..])
  rs' <- S.get
  put $ rs' { rX = rX rs,
              rW = rW rs,
              rY = bottom (rElements rs'),
              rElements = (path, []) : rElements rs'}

renderInline :: Int -> Inline -> [Int] -> StateT RenderState IO ()
renderInline origX (IText txt) = renderInline' origX txt (clr 0x80 0x80 0x80 0)
renderInline origX (ILink txt _) = renderInline' origX txt (clr 0x80 0x80 0xa0 0)

renderInline' :: Int -> String -> Raw.Color -> [Int] -> StateT RenderState IO ()
renderInline' origX txt c path = do
  space <- textSurface " " fontSize fpath c
  sDim <- surfaceDimensions space
  freeSurface space
  ws <- mapM (\w -> textSurface w fontSize fpath c) (words txt)
  dims <- mapM surfaceDimensions ws
  coordinates <- mapM (placeWord origX sDim) dims
  rs <- S.get
  put $ rs { rElements = (path, zip coordinates ws) : rElements rs }

-- always prepends space or newline
placeWord :: Int -> V2 CInt -> V2 CInt -> StateT RenderState IO (Rectangle Int)
placeWord origX sDim wDim = do
  rs <- S.get
  let x = rX rs + vX sDim + vX wDim
      (st, c) = if (x > rW rs)
                then (rs { rX = origX + vX wDim, rY = rY rs + vY wDim },
                      rect origX (rY rs + vY wDim) (vX wDim) (vY wDim))
                else (rs { rX = x },
                      rect (rX rs + vX sDim) (rY rs) (vX wDim) (vY wDim))
  put st
  pure c
