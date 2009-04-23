-- Simulation.hs - physics goes here
module Simulation where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import Foreign
import Data.Array.Storable
import Data.List.Split (splitEvery)

import Physics.ODE
import Codec.Image.PNG

title = "World"

data Model = Model {
    terrain :: Terrain,
    robots :: [Robot]
}

data Terrain = Terrain {
    tiles :: [Tile]
}

data Tile = Tile {
    nw, ne, se, sw :: Vertex3 GLfloat
} deriving Show

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
        
        rowM ptr = (splitEvery w . map scaleHeight . takeEvery 3)
            `liftM` peekArray (3 * w * h) ptr
    pixels <- withStorableArray im rowM
    
    let
        grid :: Int -> Int -> GLfloat
        grid x y = (pixels !! ym) !! xm where
            xm = x `mod` w
            ym = y `mod` h
        
        xx = [0 .. w - 1]
        yy = [0 .. h - 1]
        
        tx :: Int -> GLfloat
        tx x = (fromIntegral x) - (fromIntegral w) / 2
        
        ty :: Int -> GLfloat
        ty y = (fromIntegral y) - (fromIntegral h) / 2
        
        makeTile :: Int -> Int -> Tile
        makeTile x y = Tile nw ne se sw where
            nw = Vertex3 (tx x) (ty y) $ grid x y
            ne = Vertex3 (tx $ x + 1) (ty y) $ grid (x + 1) y
            se = Vertex3 (tx $ x + 1) (ty $ y - 1) $ grid (x + 1) (y - 1)
            sw = Vertex3 (tx x) (ty $ y - 1) $ grid x (y - 1)
        tileGrid = [ [ makeTile x y | x <- xx ] | y <- yy ]
        tiles = concat tileGrid
        
    return $ Model (Terrain tiles) []

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
    let
        (Terrain tiles) = terrain model
        texCoord2f :: GLfloat -> GLfloat -> IO ()
        texCoord2f x y = texCoord $ TexCoord2 x y
        quadM (Tile nw ne se sw) = do 
            let
                (Vertex3 _ _ zNW) = nw
                (Vertex3 _ _ zNE) = ne
                (Vertex3 _ _ zSE) = se
                (Vertex3 _ _ zSW) = sw
                z = (zNW + zNE + zSE + zSW) / 40
            color $ Color4 z z z (1 :: GLfloat)
            vertex nw
            vertex ne
            vertex se
            vertex sw
    renderPrimitive Quads $ mapM quadM tiles
    
    swapBuffers
    postRedisplay Nothing
