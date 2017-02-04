{-# LANGUAGE OverloadedStrings #-}

import SDL
import SDL.TTF as TTF
import Control.Monad.State as S
import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import View.Common
import View.Rectangle
import View.Navigation
import View.Render


main :: IO ()
main = do
  b <- parseBlock <$> BL.getContents
  case b of
    Left err -> putStrLn err
    Right block -> do
      initialize [InitEvents, InitVideo]
      window <- createWindow "view" defaultWindow {windowResizable = True}
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
