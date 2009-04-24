{- LambdaScape.hs -
    a testbed for autonomous robot navigation systems in a simulated virtual
    with simulated sensors
-}
module LambdaScape where

import Control.Monad
import List (intersperse, isPrefixOf)
import System.IO
import Foreign

import Physics.ODE
import Physics.ODE.Types
import Physics.ODE.Objects
import qualified Physics.ODE.World as W
import qualified Physics.ODE.Mass as M
import qualified Physics.ODE.Body as B

import Graphics.UI.GLUT
import Data.Word
import Codec.Image.PNG
import Data.Array.Storable
import Data.List.Split

-- this will return a GeomStruct for TriangleMesh data, however that works
loadTerrain :: String -> IO ()
loadTerrain file = do
    epng <- loadPNGFile file
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
    return () -- for now

-- for simulating LIDAR-type sensors
-- use builtin Ray type to check for intersections and then solve
distance :: World -> (Float,Float,Float) -> (Float,Float,Float) -> IO Float
distance world pos vec = error "Not implemented"

-- communicate with a client device on a line-buffered handle
handler :: World -> Handle -> IO ()
handler world handle = do
    mapM_ (hPutStrLn handle <=< parseCommand world)
        =<< lines `liftM` hGetContents handle

-- parse a command from the client, performing the appropriate response
-- and returning the 
parseCommand :: World -> String -> IO String
parseCommand world line = do
    let (cmd:params) = words line
    case cmd of
        -- query the distance
        -- should only need to pass in (vx,vy,vz) but this is easier for now
        "d" -> show `liftM` distance world (px,py,pz) (vx,vy,vz)
            where (px:py:pz:vx:vy:vz:_) = map read params
        -- spin the motors on the robot in the simulation
        "m" -> return "OK" -- however this will work

run :: IO ()
run = do
    -- broadcast world data here to a concurrent opengl viewer for rendering
    -- with a Control.Concurrent.Chan
    
    -- scene construction with heightmap and robot parts goes here
    -- in the meantime, enjoy the following:
    
    world <- W.create
    W.setGravity world 0 (-9.81) 0
    
    b <- B.create world
    m <- M.create
    -- probably this needs a Geom...
    --M.setSphere m 2500 0.05
    --M.mass m 1.0
    B.setMass b m
    
    -- this also goes in the Geom I bet
    --B.setPosition b 0 2 0
    B.addForce b 0 200 0
    
    let
        dt = 0.04
        physics t = do
            vel <- B.getLinearVel b
            pos <- B.getBodyPosition b
            putStrLn $ concat $ intersperse "; " $ [
                    "t=" ++ show t,
                    "pos=" ++ show pos,
                    "vel=" ++ show vel
                ]
            W.step world dt
        
    mapM_ physics [0, dt ..]
