-- Simulation.hs - physics goes here
module Simulation where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import Foreign
import Data.Array.Storable
import Data.List.Split (splitEvery)

import Physics.ODE.Types
import Physics.ODE.World as W
import Physics.ODE.Mass as M
import Physics.ODE.Body as B
import Codec.Image.PNG

title = "LambdaScape"

data Model = Model {
    terrain :: Terrain,
    robots :: [Robot],
    world :: World
}

data Terrain = Terrain {
    grid :: [[Vertex3 GLfloat]]
    --triangles :: [GeomClass]
}

data Robot = Robot {
    name :: String,
    geometry :: [GeomClass]
}

createModel :: IO Model
createModel = do
    epng <- loadPNGFile "height.png"
    let
        png = case epng of
            (Left msg) -> error msg
            (Right png') -> png'
        im = imageData png
        (w',h') = dimensions png
        (w,h) = (fromIntegral w', fromIntegral h')
        
        scaleHeight :: Word8 -> GLfloat
        scaleHeight z = (fromIntegral z) / 255 * 10
        
        takeEvery :: Int -> [a] -> [a]
        takeEvery i xs = map head $ splitEvery i xs
        
        rowM ptr = (map scaleHeight . takeEvery 3)
            `liftM` peekArray (3 * w * h) ptr
        
    pixels <- withStorableArray im rowM
    
    let
        xx = cycle [ -(fromIntegral w) / 2 .. (fromIntegral w) / 2 ]
        yy = concatMap (replicate w)
            [ -(fromIntegral h) / 2 .. (fromIntegral h) / 2 ]
        
        points :: [[Vertex3 GLfloat]]
        points = splitEvery w $ zipWith3 Vertex3 xx yy pixels
        
        --mesh :: [GeomClass]
        --mesh = 
    
    -- setGeomData
    world <- W.create
    W.setGravity world 0 (-9.81) 0
    
    mass <- M.create
    B.setMass mass 0.0
    
    return $ Model {
        terrain = Terrain { grid = points },
        robots = [],
        world = world
    }

initialize :: IO ()
initialize = do
    translate $ Vector3 0 0 ((-10) :: GLfloat)

keyboard :: IORef Model -> KeyboardMouseCallback
keyboard modelRef key keyState modifiers _ = do
    -- escape key terminates the main loop correctly for debugging in ghci,
    -- unlike all of the orange book translations
    when (key == Char '\27') leaveMainLoop

display :: IORef Model -> DisplayCallback
display modelRef = do
    clearColor $= Color4 0.8 0.65 0.6 1
    clear [ ColorBuffer, DepthBuffer ]
    
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    shadeModel $= Flat
    depthMask $= Enabled
    lighting $= Disabled
    
    model <- get modelRef
    
    --renderGrid $ grid $ terrain model
    
    swapBuffers
    postRedisplay Nothing

-- almost
renderGrid :: [[Vertex3 GLfloat]] -> IO ()
renderGrid grid = renderPrimitive TriangleStrip stripM where
    stripM = zipWithM_ quadM highScan lowScan
    
    highScan = drop 1 $ concat $ map doubleUp grid
    lowScan = drop 1 $ concat $ map doubleUp $ tail grid
    doubleUp xs = [head xs] ++ xs ++ [last xs]
    
    quadM :: Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
    quadM v1 v2 = pointM v1 >> pointM v2
    
    pointM :: Vertex3 GLfloat -> IO ()
    pointM v@(Vertex3 _ _ z) = color (Color4 c c c 1) >> vertex v
        where c = z / 10
