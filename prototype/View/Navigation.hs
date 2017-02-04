{-# LANGUAGE OverloadedStrings #-}
module View.Navigation where

import Types
import SDL
import Data.List as L
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative
import System.IO
import Data.Maybe

import View.Rectangle
import View.Common


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

textInput :: EventPayload -> Maybe Text.Text
textInput (TextInputEvent tid) = Just $ textInputEventText tid
textInput _ = Nothing

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
