-- Shaders.hs - conveniences for working with shaders
module Shaders where
import Graphics.UI.GLUT
import Control.Monad

-- shader stuff based heavily off orange book Brick.hs example
-- which was itself adapted from ogl2brick.c
-- takes a vertex source filename and a fragment source filename
buildShaders :: FilePath -> FilePath -> IO (Maybe Program)
buildShaders vPath fPath = do
    [vShader] <- genObjectNames 1
    [fShader] <- genObjectNames 1
    let 
        compile path shader = do
            source <- readFile path
            shaderSource shader $= [source]
            compileShader shader
            reportErrors
            ok <- get $ compileStatus shader
            infoLog <- get $ shaderInfoLog shader
            putStrLn $ "Compile log (" ++ path ++ "):\n" ++ show infoLog
            unless ok $ putStrLn "Compilation failed"
            return ok
    vSuccess <- compile vPath vShader
    fSuccess <- compile fPath fShader
    let success = vSuccess && fSuccess
    
    [prog] <- genObjectNames 1
    when success $ do
        attachedShaders prog $= ([vShader], [fShader])
        linkProgram prog
        reportErrors
        ok <- get $ linkStatus prog
        infoLog <- get $ programInfoLog prog
        putStrLn $ "Install log:\n" ++ show infoLog
        unless ok $ putStrLn "linking failed"
    if success then return $ Just prog else return Nothing
