{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracketOnError)
import Control.Lens
import Control.Monad
import Control.Monad.State qualified as S
import Data.ByteString qualified as BS
import Data.Foldable (for_, traverse_)
import Data.Generics.Labels
import Data.Ix (inRange)
import Data.Maybe (fromJust)
import Deque.Lazy (Deque)
import Deque.Lazy qualified as D
import Foreign
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Graphics.Rendering.OpenGL as GL
import Linear.V2
import SDL qualified
import System.Exit (exitSuccess)
import System.Random.Stateful (globalStdGen, uniformRM)

type Direction = V2 Int
type Point = V2 Int
data Env = Env 
  { snake :: Deque Point
  , dir :: Direction
  , food :: Point
  , lastTick :: Word32
  } deriving Generic
type Game = S.StateT Env IO

gridSize :: Int
gridSize = 17

tickRate :: Word32
tickRate = 150

up, down, left, right :: V2 Int
up = V2 0 -1
down = V2 0 1
left = V2 -1 0
right = V2 1 0

red, blue :: Vector4 Float
red = Vector4 1.0 0.0 0.0 1.0
blue = Vector4 0.0 0.0 1.0 1.0

shutdown :: SDL.Window -> IO ()
shutdown window = do
  SDL.destroyWindow window
  exitSuccess

onEvent :: SDL.EventPayload -> Game ()
onEvent = \case
  SDL.WindowClosedEvent (SDL.WindowClosedEventData window) -> S.liftIO $ shutdown window
  SDL.KeyboardEvent (SDL.KeyboardEventData (Just window) SDL.Pressed _ sym) -> case sym.keysymKeycode of
    SDL.KeycodeEscape -> S.liftIO $ shutdown window
    SDL.KeycodeW -> #dir %= (`changeDir` up)
    SDL.KeycodeA -> #dir %= (`changeDir` left)
    SDL.KeycodeS -> #dir %= (`changeDir` down)
    SDL.KeycodeD -> #dir %= (`changeDir` right)
    _ -> pure ()
  SDL.WindowResizedEvent (SDL.WindowResizedEventData _ (V2 x y)) -> 
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral x) (fromIntegral y))
  _ -> pure ()
 where
  changeDir dir dir'
    | dir' == -dir = dir
    | otherwise = dir'

drawTile :: (?square :: VertexArrayObject) => Vector4 Float -> Point -> IO ()
drawTile color (V2 x y) = do
  setUniform "inColor" color
  let offset = Vector2 (2 * toEnum x / toEnum gridSize) (2 * toEnum y / toEnum gridSize) :: Vector2 Float
  setUniform "offset" offset
  bindVertexArrayObject $= Just ?square
  drawArrays Triangles 0 6
 where
  setUniform name value = do
    Just program <- get currentProgram
    location <- get $ uniformLocation program name
    get errors >>= traverse_ \(Error _ message) -> putStrLn $ "GL error: " <> message
    uniform location $= value

defaultEnv :: IO Env
defaultEnv = Env snake dir food <$> SDL.ticks
 where
  snake = fromList $ (`V2` 8) <$> reverse [0..3]
  dir = right
  food = V2 10 8

gameLoop :: (?square :: VertexArrayObject) => SDL.Window -> Game ()
gameLoop window = forever do 
  events <- SDL.pollEvents
  for_ events $ onEvent . SDL.eventPayload
  time <- SDL.ticks
  Env snake dir food lastTick <- S.get
  when (time - lastTick > tickRate) do
    let mouth = fromJust (D.head snake) + dir
    #snake %= D.init . D.cons mouth
    when (mouth == food) do
      #snake %= \snake -> 
        let end = fromJust (D.last snake) + foldl1 (-) (D.take 2 $ D.reverse snake)
        in D.snoc end snake
      moveFood
    resetted <- S.liftIO defaultEnv 
    when (mouth `elem` D.tail snake || not (inBounds mouth)) $ id .= resetted
    snake <- use #snake
    food <- use #food
    S.liftIO do
      GL.clearColor $= Color4 1 1 1 1
      GL.clear [ColorBuffer]
      traverse_ (drawTile blue) snake
      drawTile red food
      SDL.glSwapWindow window
    #lastTick .= time
 where
  inBounds = inRange (V2 0 0, V2 (gridSize - 1) (gridSize - 1))
  moveFood = do
    x <- uniformRM (0, gridSize - 1) globalStdGen
    y <- uniformRM (0, gridSize - 1) globalStdGen
    let food = V2 x y
    snake <- use #snake
    if food `elem` snake
      then moveFood
      else #food .= food

initWindow :: IO SDL.Window
initWindow = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "snek" windowConfig
  -- SDL.showWindow window
  SDL.glCreateContext window
  pure window
 where
  windowGraphicsContext = SDL.defaultOpenGL
    & #glProfile .~ SDL.Core SDL.Normal 4 6
  windowConfig = SDL.defaultWindow
    & #windowInitialSize .~ V2 600 600
    & #windowGraphicsContext .~ SDL.OpenGLContext windowGraphicsContext

loadShaders :: [(ShaderType, FilePath)] -> IO Program
loadShaders info = createProgram `bracketOnError` deleteObjectName $ \program -> do
  for_ info \(shType, path) -> createShader shType `bracketOnError` deleteObjectName $ \shader -> do
    source <- BS.readFile path
    shaderSourceBS shader $= source
    checked compileShader compileStatus shaderInfoLog "compile" shader
    attachShader program shader
  checked linkProgram linkStatus programInfoLog "link" program
  pure program
 where
  checked action getStatus getInfoLog message object = do
    action object
    ok <- get $ getStatus object
    unless ok do
      infoLog <- get $ getInfoLog object
      fail $ message <> " log: " <> infoLog

initSquare :: IO VertexArrayObject
initSquare = do
  square <- genObjectName
  bindVertexArrayObject $= Just square
  let 
   size = (1 / toEnum gridSize) * 2
   vertices = 
     [ Vertex2 -1.0 1.0                   -- top left
     , Vertex2 (size - 1.0) 1.0           -- top right
     , Vertex2 -1.0 (1.0 - size)          -- bottom left
     , Vertex2 (size - 1.0) (1.0 - size)  -- bottom right
     , Vertex2 (size - 1.0) 1.0           -- top right
     , Vertex2 -1.0 (1.0 - size)          -- bottom left
     ] :: [Vertex2 Float]
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices \ptr -> 
    let arraySize = fromIntegral $ 6 * sizeOf (head vertices)
    in bufferData ArrayBuffer $= (arraySize, ptr, StaticDraw)
  program <- loadShaders 
    [ (VertexShader, "shader.vert")
    , (FragmentShader, "shader.frag")
    ]
  currentProgram $= Just program
  let vPosition = AttribLocation 0
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray vPosition $= Enabled 
  pure square

main :: IO ()
main = do
  window <- initWindow
  square <- initSquare
  env <- defaultEnv
  let ?square = square in S.runStateT (gameLoop window) env
  SDL.destroyWindow window
