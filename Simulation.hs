-- Simulation.hs - physics goes here
module Simulation where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import Foreign
import Data.Array.Storable
import Data.List.Split (splitEvery)

import Physics.ODE
import Physics.ODE.Types
import Physics.ODE.World as W
import Physics.ODE.Mass as M
import Physics.ODE.Body as B

import LambdaScape

title = "LambdaScape"

data Model = Model {
    terrain :: Terrain,
    robots :: [Robot]
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
    grid <- loadTerrain "height.png"
    
    return $ Model {
        terrain = Terrain grid,
        robots = []
    }

createCar :: IO ()
createCar = do
    return ()

{-
    void applyAntiSwayBarForces() {
        amt = 0;
        for(int i = 0; i < 4; i++) {                
            Vector3 anchor2 = wheels[i].Joint.Anchor2;
            Vector3 anchor1 = wheels[i].Joint.Anchor;
            Vector3 axis = wheels[i].Joint.Axis2;
     
            displacement = Vector3.Dot(anchor1-anchor2, axis);
     
            if(displacement > 0) {
                amt = displacement * swayForce;
                if(amt > swayForceLimit)
                    amt = swayForceLimit;
                wheels[i].Body.AddForce(-axis *amt); //downforce
                wheels[i^1].Body.AddForce(axis *amt); //upforce
            }
        }
    }           
-}

initialize :: IO ()
initialize = do
    translate $ Vector3 0 5 ((-10) :: GLfloat)

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
    depthMask $= Disabled
    lighting $= Disabled
    
    model <- get modelRef
    
    renderGrid $ grid $ terrain model
    renderObject Solid $ Sphere' 1.0 24 24 
    
    swapBuffers
    postRedisplay Nothing

-- almost
renderGrid :: [[Vertex3 GLfloat]] -> IO ()
renderGrid grid = renderPrimitive Quads stripM where
    ptM vx = do
        color $ Color4 1 0 0 (1 :: GLfloat)
        mapM_ vertex vx
    
    quads :: [Vertex3 GLfloat] -> [Vertex3 GLfloat] -> [[Vertex3 GLfloat]]
    quads row1 row2 =
    
    stripM = zipWithM_ rowM (init grid) (tail grid)
