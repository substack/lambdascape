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

import Prelude hiding (catch)
import Control.Exception (catch)

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
    rMatrix :: GLmatrix GLfloat
} deriving Show

createModel :: IO (IORef Model)
createModel = do
    prog <- getProgName
    args <- getArgs
    when (length args < 1) $ do
        putStrLn $ error $ "\n\tUsage: " ++ prog ++ " [port]"
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
        -- grab the robot name and its row-major 4x4 matrix
        let parsed = reads line :: [((String, [Float]),String)]
        case parsed of
            [] -> do -- complain loudly about bad input without crashing
                putStrLn $ "Failed to parse\n"
                    ++ (concatMap ("    " ++) $ lines line)
            [((name, mat),_)] -> do -- set the rotation and translation
                rMat <- newMatrix RowMajor mat
                robotsRef $~ M.insert name (Robot {
                    rMatrix = rMat
                })
        yield >> updateRobots modelRef xs -- next line

initialize :: IO ()
initialize = do
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
    when (keyState == Up) $ case key of
        -- show the keybindings file
        SpecialKey KeyF1 -> putStr =<< readFile "KEYBINDINGS"
        _ -> return ()

-- main display monad
-- does some opengl housekeeping and then delegates to the render monads
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
    
    -- draw the terrain
    renderGrid $ terrain model
    -- render all the robots in the robot mapping
    mapM_ renderRobot =<< (M.elems `liftM` (get $ robotsRef model))
    
    swapBuffers
    postRedisplay Nothing

renderGrid :: Terrain -> IO ()
renderGrid (Terrain trx) = renderPrimitive Triangles $ mapM_ triM trx where
    triM (v1,v2,v3) = ptM v1 >> ptM v2 >> ptM v3
    ptM (Vertex3 x y z) = do
        let c = z / 4.0
        color $ Color4 c c c 1
        vertex $ Vertex3 x y (z * 5)

renderRobot :: Robot -> IO ()
renderRobot robot = preservingMatrix $ do
    color $ Color4 1 0 0 (1 :: GLfloat)
    multMatrix $ rMatrix robot
    renderObject Solid $ Cube 10.0
