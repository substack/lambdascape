-- Simulation.hs - connect to and observe the physics simulation
module Simulation where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Network
import System.IO
import Control.Concurrent (forkIO, yield)
import System

title = "LambdaScape"

data Model = Model {
    terrain :: Terrain,
    robots :: IORef [Robot],
    sock :: Handle,
    done :: Bool
}

type TriangleFace = (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)

data Terrain = Terrain {
    triangles :: [TriangleFace]
} deriving Show

data Robot = Robot {
    name :: String,
    position :: Vector3 GLfloat,
    rotation :: GLmatrix GLfloat
} deriving Show

createModel :: IO (IORef Model)
createModel = do
    args <- getArgs
    let port = fromIntegral $ read $ head args
    
    handle <- connectTo "localhost" (PortNumber port)
    hSetBuffering handle LineBuffering
    
    hPutStrLn handle "observer\nterrain" -- request the terrain as an observer
    -- terain is verticies
    let
        makeV3 :: (GLfloat, GLfloat, GLfloat) -> Vertex3 GLfloat
        makeV3 (x,y,z) = Vertex3 x y z
    verts <- (map makeV3 . read) `liftM` hGetLine handle
    -- and faces (triangles) that index the vertices
    faces <- read `liftM` hGetLine handle
    let -- trace the indicies
        tx :: [TriangleFace]
        tx = map interp faces
        interp (i,j,k) = (verts !! i, verts !! j, verts !! k)
    
    robotsRef <- newIORef []
    
    modelRef <- newIORef Model {
        terrain = Terrain tx,
        robots = robotsRef,
        sock = handle,
        done = False
    }
    forkIO (updateRobots modelRef =<< lines `liftM` hGetContents handle)
    return modelRef

initialize :: IO ()
initialize = do
    translate $ Vector3 0 5 ((-10) :: GLfloat)

keyboard :: IORef Model -> KeyboardMouseCallback
keyboard modelRef key keyState modifiers _ = do
    -- escape key terminates the main loop correctly for debugging in ghci,
    -- unlike all of the orange book translations
    when (key == Char '\27') $ do
        model <- get modelRef
        let handle = sock model
        modelRef $= model { done = True }
        hPutStrLn handle "quit"
        leaveMainLoop

display :: IORef Model -> DisplayCallback
display modelRef = do
    clearColor $= Color4 0.8 0.65 0.6 1
    clear [ ColorBuffer, DepthBuffer ]
    
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    shadeModel $= Flat
    depthMask $= Disabled
    lighting $= Disabled
    
    model <- get modelRef
    when (done model) $ leaveMainLoop
    
    renderGrid $ terrain model
    renderObject Solid $ Sphere' 1.0 24 24 
    
    swapBuffers
    postRedisplay Nothing

updateRobots :: IORef Model -> [String] -> IO ()
updateRobots modelRef [] = do
    model <- get modelRef
    modelRef $= model { done = True }

updateRobots modelRef (line:xs) = do
    (Model _ robotRef handle done) <- get modelRef
    when (not done) $ do
        putStrLn line
        --(name, (pos, rot)) <- read `liftM` hGetLine handle 
        {-
        putStrLn $ "name=" ++ name
        putStrLn $ "pos=" ++ pos
        putStrLn $ "rot=" ++ rot
        -}
        yield >> updateRobots modelRef xs

renderGrid :: Terrain -> IO ()
renderGrid (Terrain trx) = renderPrimitive Triangles $ mapM_ triM trx where
    triM (v1,v2,v3) = ptM v1 >> ptM v2 >> ptM v3
    ptM (Vertex3 x y z) = do
        color $ Color4 z z z 1
        vertex $ Vertex3 x y (z * 5)
