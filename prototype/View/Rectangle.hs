module View.Rectangle where

import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import SDL.Video.Renderer


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

inRectangle :: (Integral a, Integral b) => Point V2 a -> Rectangle b -> Bool
inRectangle (P (V2 x y)) r = and [ x >= rectX r
                            , x <= rectX r + rectW r
                            , y >= rectY r
                            , y <= rectY r + rectH r]
