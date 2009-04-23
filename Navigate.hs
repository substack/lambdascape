-- Navigate.hs - provide navigation conveniences for a main program
module Navigate where

import Graphics.UI.GLUT
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Control.Monad
import Data.IORef

data Navigator = Navigator {
    -- scene camera stuff
    cameraMatrix :: GLmatrix GLdouble,
    cameraDelta :: GLmatrix GLdouble,
    frameTimes :: [Float],
    keysDown :: Set.Set Key,
    -- unit-frames per second
    rotationalVelocity :: Float,
    translationVelocity :: Float
} deriving Show

-- create a new navigator data type
createNavigator :: IO () -> IO Navigator
createNavigator initM = preservingMatrix $ do
    loadIdentity
    identity <- get $ matrix Nothing
    initM
    camera <- get $ matrix Nothing
    
    return $ Navigator {
        cameraDelta = identity,
        cameraMatrix = camera,
        frameTimes = [],
        keysDown = Set.empty,
        -- reasonable defaults
        rotationalVelocity = 50,
        translationVelocity = 10
    }

-- calculate frames per second
getFPS :: Navigator -> Float
getFPS nav = if ft == [] then default_fps else fps
    where
        default_fps = 30 -- initial conditions are pesky
        ft = frameTimes nav
        fps = (fromIntegral $ length ft) / (sum ft)

keyboards :: [KeyboardMouseCallback] -> KeyboardMouseCallback
keyboards [] _ _ _ _ = return ()
keyboards (cb:cbx) key keyState modifiers pos = do
    cb key keyState modifiers pos
    keyboards cbx key keyState modifiers pos

-- build a new navigator object in response to keyboard input
-- wrapper around user-supplied keyboard callback
keyboard :: IORef Navigator -> KeyBinding -> KeyboardMouseCallback
keyboard navRef binding key keyState modifiers pos = do
    -- escape key terminates the main loop post haste!
    when (key == Char '\27') leaveMainLoop
    nav <- get navRef
    let
        fps = getFPS nav
        rotV = (rotationalVelocity nav) / fps
        transV = (translationVelocity nav) / fps
        
        newKeysDown = keyMod key $ keysDown nav
        keyMod = case keyState of
            Down -> Set.insert
            Up -> Set.delete
    
    delta <- preservingMatrix $ do
        loadIdentity
        let bound key = binding rotV transV key keyState modifiers pos
        mapM_ bound $ Set.toList $ newKeysDown
        get $ matrix Nothing
    
    navRef $= nav {
        keysDown = newKeysDown,
        cameraDelta = delta
    }
    
type KeyBinding = Float -> Float -> KeyboardMouseCallback

-- vi-style keybindings for camera control each key at a time \o/
-- these are pretty sweet if you use vi a lot
viBinding :: KeyBinding
viBinding rotV transV key keyState (Modifiers shift ctrl alt) pos
    | key == Char 'h' = -- move left
        translate $ vector3f transV 0 0
    | key == Char 'j' = -- move backward
        translate $ vector3f 0 0 (-transV)
    | key == Char 'k' = -- move forward
        translate $ vector3f 0 0 transV
    | key == Char 'l' = -- move right
        translate $ vector3f (-transV) 0 0
    | key == Char 'u' = -- move up
        translate $ vector3f 0 (-transV) 0
    | key == Char 'i' = -- move down
        translate $ vector3f 0 transV 0
    | key == Char 'w' = -- pan up
        rotate (-rotV) $ vector3f 1 0 0
    | key == Char 'a' = -- pan left
        rotate (-rotV) $ vector3f 0 1 0
    | key == Char 's' = -- pan down
        rotate rotV $ vector3f 1 0 0
    | key == Char 'd' = -- pan right
        rotate rotV $ vector3f 0 1 0
    | key == Char 'q' = -- tilt counter-clockwise
        rotate rotV $ vector3f 0 0 1
    | key == Char 'e' = -- tilt clockwise
        rotate (-rotV) $ vector3f 0 0 1
    -- and the arrow keys work too how about
    -- you shouldn't use them but everyone does
    | key == SpecialKey KeyUp = -- move forward
        translate $ vector3f 0 0 transV
    | key == SpecialKey KeyDown = -- move backward
        translate $ vector3f 0 0 (-transV)
    | key == SpecialKey KeyLeft = -- pan left
        rotate (-rotV) $ vector3f 0 1 0
    | key == SpecialKey KeyRight = -- pan right
        rotate rotV $ vector3f 0 1 0
    | otherwise = return ()

-- wrapper around user-supplied display callback
-- keeps track of housekeeping stuff
display :: IORef Navigator -> DisplayCallback -> IO ()
display navRef display = do
    start <- getPOSIXTime
    nav <- get navRef
    
    -- matrix trickery
    loadIdentity
    matrixMode $= Modelview 0
    camera <- preservingMatrix $ do
        loadIdentity
        multMatrix $ cameraDelta nav
        multMatrix $ cameraMatrix nav
        get $ matrix Nothing
    multMatrix camera
    
    display -- supplied display loop type operations
    
    stop <- getPOSIXTime
    -- keep track of a running window of 5 frame times
    let elapsed = fromRational . toRational $ stop - start
    
    navRef $= nav {
        frameTimes = elapsed : (take 4 $ frameTimes nav),
        cameraMatrix = camera
    }

-- to make types easier:
vector3f :: GLfloat -> GLfloat -> GLfloat -> Vector3 GLfloat
vector3f x y z = Vector3 x y z

vector3d :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vector3d x y z = Vector3 x y z

vertex3d :: GLdouble -> GLdouble -> GLdouble -> Vertex3 GLdouble
vertex3d x y z = Vertex3 x y z

vertex3f :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3f x y z = Vertex3 x y z
