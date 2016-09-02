{-# LANGUAGE OverloadedStrings #-}

import SDL
import qualified SDL.Raw as Raw
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import Control.Monad (unless)
import SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import SDL.Image
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Traversable
import Data.Either
import Data.Word

import Types
import BlockState


fpath = "/usr/share/fonts/dejavu/DejaVuSans.ttf"
sectionPaddingLeft = 20
titleFontSize = 20
fontSize = 16


main :: IO ()
main = do
  block <- readLn
  initializeAll
  window <- createWindow "( ﾉ ﾟｰﾟ)ﾉ ☀" defaultWindow {windowResizable = True}
  renderer <- createRenderer window (-1) defaultRenderer
  TTF.init
  appLoop block (makeState False block) 0 Nothing renderer
  TTF.quit


redrawNeeded :: EventPayload -> Bool
redrawNeeded (WindowShownEvent _) = True
redrawNeeded (WindowExposedEvent _) = True
redrawNeeded (WindowResizedEvent _) = True
redrawNeeded (MouseWheelEvent _) = True
redrawNeeded (KeyboardEvent _) = True
redrawNeeded _ = False

rerenderNeeded :: EventPayload -> Bool
rerenderNeeded (WindowResizedEvent _) = True
rerenderNeeded (KeyboardEvent _) = True
rerenderNeeded _ = False

adjustOffset :: Int -> EventPayload -> Int
adjustOffset cur (MouseWheelEvent (MouseWheelEventData _ _ (V2 _ o))) = cur + fromIntegral o * 50
adjustOffset cur _ = cur

textInput :: EventPayload -> Maybe Text.Text
textInput (TextInputEvent tid) = Just $ textInputEventText tid
textInput _ = Nothing

appLoop :: Block
        -> BlockState Bool
        -> Int
        -> Maybe (Surface, BlockState [Rectangle Int])
        -> Renderer
        -> IO ()
appLoop doc bsb yOffset surf' renderer = do
  event <- waitEvent
  let epl = eventPayload event
  rvp <- get (rendererViewport renderer)
  let (rw, rh) = case rvp of
        Just (Rectangle (P (V2 x y)) (V2 w' h')) -> (fromIntegral w', fromIntegral h')
        _ -> (200, 200)

  -- navigate
  let (bsb', yOffset') = case surf' of
        Just (surf, st) ->
          let nav f =
                let bsb' = f bsb
                    yOffset' = case activeCoord bsb' st of
                      Just [Rectangle (P (V2 _ y)) (V2 _ h)] ->
                        if -yOffset >= y || -yOffset + rh <= y + h
                        then -y
                        else yOffset
                      _ -> yOffset
                in (bsb', yOffset')
          in
            case (epl, textInput epl) of
              (_, Just "n") -> nav $ navigate 1
              (_, Just "p") -> nav $ navigate (-1)
              (_, Just "]") -> nav $ navigate' 1
              (_, Just "[") -> nav $ navigate' (-1)
              ((MouseWheelEvent (MouseWheelEventData _ _ (V2 _ i))), _) ->
                (bsb, yOffset + 50 * fromIntegral i)
              _ -> (bsb, yOffset)
        _ -> (bsb, yOffset)

  -- render
  (surf, st) <- case (surf', rerenderNeeded epl) of
                  (Just (s, bsc), False) -> pure (s, bsc)
                  (Just (s, bsc), True) -> do
                    freeSurface s
                    renderBlock 0 0 rw doc bsb'
                  (Nothing, _) -> renderBlock 0 0 rw doc bsb'

  -- show
  when (redrawNeeded epl) $ do
    putStrLn $ show $ activeElem bsb' doc
    rendererDrawColor renderer $= V4 0x10 0x10 0x10 0
    clear renderer
    
    (V2 w h) <- surfaceDimensions surf
    texture <- createTextureFromSurface renderer surf
    copy renderer texture Nothing
      (Just (rect 0 yOffset' w h))
    destroyTexture texture
    present renderer

  unless (textInput epl == Just "q") $
    appLoop doc bsb' yOffset' (Just (surf, st)) renderer



-- navigation



getBlock :: [Int] -> Block -> Maybe (Either Block Inline)
getBlock [] x = Just $ Left x
getBlock [n] (BParagraph f) = if n < length f
                              then Just $ Right $ f !! n
                              else Nothing
getBlock (p:ps) (BSection _ bs) = if p < length bs
                                  then getBlock ps (bs !! p)
                                  else Nothing
getBlock _ _ = Nothing


activeElem :: BlockState Bool -> Block -> Maybe (Either Block Inline)
activeElem bsb b = case findBS (== True) bsb of
  [path] -> getBlock path b
  _ -> Nothing


rect :: (Integral a, Integral b, Integral c, Integral d, Integral e) => a -> b -> c -> d -> Rectangle e
rect a b c d = Rectangle (P (V2 (fromIntegral a) (fromIntegral b)))
               (V2 (fromIntegral c) (fromIntegral d))


textSuraface :: String -> Int -> String -> Raw.Color -> IO Surface
textSuraface str size font clr = do
  font <- openFont fpath size
  surf <- renderUTF8Solid font str $ clr
  closeFont font
  pure surf

clr = Raw.Color

makeState :: a -> Block -> BlockState a
makeState x (BSection _ blocks) = BSRec x (map (makeState x) blocks)
makeState x (BImage _) = BSOne x
makeState x (BParagraph inl) = BSFew x $ replicate (length inl) x

renderBlock :: Int -> Int -> Int -> Block -> BlockState Bool -> IO (Surface, BlockState [Rectangle Int])
renderBlock ox oy w (BSection title blocks) (BSRec active sblocks) = do
  titleSurf <- textSuraface title titleFontSize fpath $ clr 0xb0 0x80 0x80 0
  titleSurfDim@(V2 tw th) <- surfaceDimensions titleSurf
  (oy', blockSurfs) <- foldM (\(oy', bs) (b, sb) -> do
                                 (bl, bi) <- renderBlock (ox + sectionPaddingLeft) oy' (w - sectionPaddingLeft) b sb
                                 let ah = case bi of
                                       (BSRec [Rectangle _ (V2 _ h')] _) -> h'
                                       (BSFew [Rectangle _ (V2 _ h')] _) -> h'
                                       (BSOne [Rectangle _ (V2 _ h')]) -> h'
                                 pure (oy' + ah, bs ++ [(bl, bi)]))
                       (oy + fromIntegral th, [])
                       (zip blocks sblocks)
  blocksSurf <- joinBlocks (w - sectionPaddingLeft) (map fst blockSurfs) Nothing
  blocksSurfDim@(V2 bw bh) <- surfaceDimensions blocksSurf
  let totalHeight = th + bh
  s <- createRGBSurface (V2 (fromIntegral w) totalHeight) RGBA4444
  when (active) $ do
    surfaceFillRect s Nothing (V4 0x00 0x00 0x00 0xa0)
  surfaceBlit titleSurf Nothing s (Just (P (V2 0 0)))
  surfaceBlit blocksSurf Nothing s
    (Just (P (V2 (fromIntegral sectionPaddingLeft) th)))
  mapM_ freeSurface [titleSurf, blocksSurf]
  pure (s, BSRec [rect ox oy w totalHeight] $ map snd blockSurfs)
renderBlock ox oy w (BImage path) (BSOne active) = do
  is <- imgLoad path
  (V2 iw ih) <- surfaceDimensions is
  s <- createRGBSurface (V2 iw ih) RGBA4444
  when active $ surfaceFillRect s Nothing (V4 0x00 0x00 0x00 0xa0)
  surfaceBlit is Nothing s (Just (P (V2 0 0)))
  freeSurface is
  pure (s, BSOne [rect ox oy iw ih])
renderBlock ox oy w (BParagraph inlines) (BSFew active as) = do
  (s, rects) <- renderInlines ox oy w inlines active as
  (V2 x y) <- surfaceDimensions s
  pure (s, BSFew [rect ox oy x y] rects)


renderInlines :: Int -> Int -> Int
              -> [Inline] -> Bool -> [Bool]
              -> IO (Surface, [[Rectangle Int]])
renderInlines ox oy w inl active as = do
  inlines <- prepareInlines ox ox oy w (zip inl as)
  let cX (Rectangle (P (V2 x y)) (V2 w' h')) = fromIntegral $ x + w'
      cY (Rectangle (P (V2 x y)) (V2 w' h')) = fromIntegral $ y + h'
      rectangles = map (map snd) inlines
      sw = (foldl max 0 $ map cX $ concat rectangles) - fromIntegral ox
      sh = (foldl max 0 $ map cY $ concat rectangles) - fromIntegral oy
  s <- createRGBSurface (V2 sw sh) RGBA4444
  when active $ surfaceFillRect s Nothing (V4 0x00 0x00 0x00 0xa0)
  let relBlit x y surf = do
        surfaceBlit surf Nothing s
          (Just (P (V2 (fromIntegral x - fromIntegral ox)
                     (fromIntegral y - fromIntegral oy))))
        freeSurface surf
  mapM_ (mapM_ (\(surf, (Rectangle (P (V2 x y)) _)) -> relBlit x y surf)) inlines
  pure (s, map (map snd) inlines)

prepareInlines :: Int -> Int -> Int -> Int
              -> [(Inline, Bool)]
              -> IO [[(Surface, Rectangle Int)]]
prepareInlines _ _ _ _ [] = pure []
prepareInlines oox ox oy w ((it, active) : xs) = do
  let (text, color) = inlineParams it
  let textw = unwords $ words text
  firstLine <- head <$> wordWrap fpath fontSize (w - ox) textw
  otherLines <- wordWrap fpath fontSize w $ drop (length firstLine) textw
  surfaces <- mapM
    (\l -> do
        ts <- textSuraface l fontSize fpath color
        (V2 iw ih) <- surfaceDimensions ts
        baseSurface <- createRGBSurface (V2 iw ih) RGBA4444
        when active $
          surfaceFillRect baseSurface Nothing (V4 0x00 0x00 0x00 0xa0)
        surfaceBlit ts Nothing baseSurface (Just (P (V2 0 0)))
        freeSurface ts
        pure baseSurface)
    (firstLine : otherLines)
  dimensions <- mapM surfaceDimensions surfaces
  -- compute rectangles from dimensions and offsets. it's different
  -- for the first and the last ones, while same for others
  let rectangles' = snd $ foldl
        (\(y, rs) (V2 w' h') ->
           (y + fromIntegral h',
            rs ++ [rect oox y w' h']))
        (oy, [])
        dimensions
      -- adjust the first one
      rectangles = case rectangles' of
        [] -> []
        (Rectangle (P (V2 x y)) (V2 w' h'):xs) -> rect ox y w' h' : xs
      -- get data from the last one (which may be the same as the first)
      (nox, noy) = case reverse rectangles of
        [] -> (ox, oy)
        (Rectangle (P (V2 x y)) (V2 w' h'):xs) -> (x + w', y)
  -- now we have new offsets, surfaces, and rectangles for the current
  -- inline.
  ((zip surfaces rectangles) :) <$> prepareInlines oox nox noy w xs


inlineText :: Inline -> String
inlineText (ILink s _) = s
inlineText (IText s) = s

inlineParams :: Inline -> (String, Raw.Color)
inlineParams (ILink s _) = (s, clr 0x80 0x80 0xa0 0)
inlineParams (IText s) = (s, clr 0x80 0x80 0x80 0)


joinBlocks :: Int -> [Surface] -> Maybe (V4 Word8) -> IO Surface
joinBlocks w surfs clr = do
  dims <- mapM surfaceDimensions surfs
  let totalHeight = foldl (\sum (V2 w h) -> sum + h) 0 dims
  s <- createRGBSurface (V2 (fromIntegral w) totalHeight) RGBA4444
  case clr of
    Nothing -> pure ()
    Just c -> surfaceFillRect s Nothing c
  foldM (\offset (surf, (V2 w h)) ->
           surfaceBlit surf Nothing s (Just (P (V2 0 offset)))
           >> pure (h + offset)) 0
    $ zip surfs dims
  mapM_ freeSurface surfs
  pure s


wordWrap' :: TTFFont -> Int -> [String] -> Int -> IO [[String]]
wordWrap' font w str pos
  | pos < length str = do
      szTxt@(w', h') <- sizeUTF8 font (unwords $ take (pos + 1) str)
      if w' > w
        then ((take pos str) :) <$> wordWrap' font w (drop pos str) 1
        else wordWrap' font w str (pos + 1)
  | length str == 0 = pure []
  | otherwise = pure [str]


wordWrap :: String -> Int -> Int -> String -> IO [String]
wordWrap fontPath size w str = do
  font <- openFont fontPath size
  wrapped <- map ((++) " " . unwords) <$> wordWrap' font w (words str) 1
  closeFont font
  pure wrapped
