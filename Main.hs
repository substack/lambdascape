-- Main.hs
module Main where

import qualified Simulation as S
import Shaders (buildShaders)
import qualified Navigate as N
import Navigate (keyboards, display)

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

main :: IO ()
main = do
    (_, argv) <- getArgsAndInitialize
    initWindow
    createWindow S.title
    initScene
    modelRef <- newIORef =<< S.createModel
    navRef <- newIORef =<< N.createNavigator S.initialize
    
    -- return from the main loop so ghci stays running
    actionOnWindowClose $= MainLoopReturns
    
    -- bind some callbacks then launch the main loop
    let keyM = keyboards [ N.keyboard navRef N.viBinding, S.keyboard modelRef ]
    keyboardMouseCallback $= Just keyM
    displayCallback $= display navRef (S.display modelRef)
    reshapeCallback $= Just reshape
    mainLoop

-- standard reshaping song and dance from the red book examples
reshape :: ReshapeCallback
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    let (w,h) = (512,512)
    matrixMode $= Projection
    loadIdentity
    let aspectRatio = (fromIntegral w) / (fromIntegral h)
    perspective 60 aspectRatio 0.1 1000
    matrixMode $= Modelview 0

-- setup the window
initWindow :: IO ()
initWindow = do
    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 512 512
    initialWindowPosition $= Position 0 0

-- setup the scene
initScene :: IO ()
initScene = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    shadeModel $= Flat
    depthMask $= Enabled
    pointSmooth $= Enabled
    lineSmooth $= Enabled
    lighting $= Disabled
    texture Texture2D $= Enabled
