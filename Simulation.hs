-- Simulation.hs - connect to and observe the physics simulation
module Simulation where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Network
import System.IO
import Control.Concurrent (forkIO, yield, threadDelay)
import System
import qualified Data.Map as M
import qualified Data.Map ((!))

title = "LambdaScape"

data Model = Model {
    terrain :: Terrain,
    robotsRef :: IORef (M.Map String Robot),
    sock :: Handle,
    done :: Bool
}

type TriangleFace = (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)

data Terrain = Terrain {
    triangles :: [TriangleFace]
} deriving Show

data Robot = Robot {
    rRot :: GLmatrix GLfloat,
    rPos :: Vertex3 GLfloat
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
    
    robotsRef <- newIORef $ M.empty
    
    modelRef <- newIORef Model {
        terrain = Terrain tx,
        robotsRef = robotsRef,
        sock = handle,
        done = False
    }
    let updateM = updateRobots modelRef =<< lines `liftM` hGetContents handle
    -- wait until the display callback has probably been run
    forkIO (threadDelay 1000 >> updateM)
    return modelRef

updateRobots :: IORef Model -> [String] -> IO ()
updateRobots modelRef [] = do
    model <- get modelRef
    modelRef $= model { done = True }
updateRobots modelRef (line:xs) = do
    (Model _ robotsRef handle done) <- get modelRef
    when (not done) $ do
        let
            px,py,pz :: GLfloat
            rot :: [Float]
            (name, (px,py,pz), rot) = read line
        rotMat <- newMatrix RowMajor rot
        
        -- set the rotation and translation
        robotsRef $~ M.insert name (Robot {
            rPos = Vertex3 px py pz,
            rRot = rotMat
        })
        yield >> updateRobots modelRef xs

initialize :: IO ()
initialize = do
    --rotate 180 $ Vector3 1 0 (0 :: GLfloat)
    translate $ Vector3 0 0 (-100 :: GLfloat)

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
    depthMask $= Enabled
    depthFunc $= Just Lequal
    lighting $= Disabled
    
    model <- get modelRef
    when (done model) $ leaveMainLoop
    
    renderGrid $ terrain model
    rr <- M.elems `liftM` (get $ robotsRef model)
    mapM_ renderRobot =<< (M.elems `liftM` (get $ robotsRef model))
    
    swapBuffers
    postRedisplay Nothing

renderGrid :: Terrain -> IO ()
renderGrid (Terrain trx) = renderPrimitive Triangles $ mapM_ triM trx where
    triM (v1,v2,v3) = ptM v1 >> ptM v2 >> ptM v3
    ptM (Vertex3 x y z) = do
        color $ Color4 z z z 1
        vertex $ Vertex3 x y (z * 5)

renderRobot :: Robot -> IO ()
renderRobot robot = preservingMatrix $ do
    --translate $ rPos robot
    color $ Color4 1 0 0 (1 :: GLfloat)
    pointSize $= 20
    renderPrimitive Points $ vertex $ rPos robot
    --renderObject Solid $ Cube 1.0
