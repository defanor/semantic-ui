module View.Common where

import SDL
import Foreign.C.Types

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


