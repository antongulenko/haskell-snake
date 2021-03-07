import System.IO
    ( hSetBuffering, hSetEcho, stdin, hReady, BufferMode(NoBuffering) )
import Control.Monad ( when )
import System.Console.ANSI ( clearScreen, setCursorPosition )
import Data.IORef ( newIORef, readIORef, writeIORef )
import qualified Data.Foldable
import Control.Concurrent ( threadDelay, forkIO )
import System.Random ( Random(randomRIO) )

fieldX = 20
fieldY = 20
moveSleep = 100 * 1000

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  direction <- newIORef "right"
  collision <- newIORef False

  -- Move and draw the snake in parallel, while reading user input in the main thread
  forkIO $ runSnake direction collision [Point (fieldX `div` 2) (fieldY `div` 2)]
  handleInputLoop direction collision

getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

data Point = Point { x :: Int, y :: Int }
    deriving (Eq, Show)

handleInputLoop direction collision = do
  key <- getKey
  hasCollided <- readIORef collision
  continue <- handleInput key hasCollided direction
  when continue (handleInputLoop direction collision)

handleInput key hasCollided direction
  | hasCollided || key == "\ESC" = return False
  | otherwise = do
      propagateDirection key direction
      return True

propagateDirection char direction = do
  let translated = translateDirection char
  writeIORef direction translated

translateDirection char
    | char == "\ESC[A" = "up"
    | char == "\ESC[B" = "down"
    | char == "\ESC[C" = "right"
    | char == "\ESC[D" = "left"
    | otherwise = "none"

runSnake direction collision snake = do
    x <- randomRIO (0, fieldX-1)
    y <- randomRIO (0, fieldY-1)
    let food = Point x y
    snake <- moveSnakeLoop direction collision food snake
    Data.Foldable.forM_ snake (runSnake direction collision)

data Event = Moved [Point] | Fed [Point] | Dead

moveSnakeLoop direction collision food snake = do
  threadDelay moveSleep -- Delay next move
  move <- moveSnake direction food snake
  case move of
    Moved snake -> moveSnakeLoop direction collision food snake
    Fed snake -> return (Just snake)
    Dead -> do
      writeIORef collision True
      return Nothing

moveSnake direction food snake@(head:_) = do
    dir <- readIORef direction
    let (Point x y) = moveHead head dir                     -- Compute new snake head
    let newHead = Point (x `mod` fieldX) (y `mod` fieldY)   -- Translate snake head inside the field
    let movedSnake = newHead : snake
    draw food movedSnake
    return $ collide food movedSnake

moveHead :: Point -> [Char] -> Point
moveHead point@(Point x y) dir
    | dir == "up" = point { y = y - 1 }
    | dir == "down" = point { y = y + 1 }
    | dir == "left" = point { x = x - 1 }
    | dir == "right" = point { x = x + 1 }
    | otherwise = point -- "none" means no movement. TODO causes instant death.

trimTail [head] = []
trimTail (head:tail) = head : trimTail tail

collide food snake@(head:tail)
  | food == head = Fed snake           -- Head reached food
  | head `elem` tail = Dead            -- Head inside tail
  | otherwise = Moved (trimTail snake) -- No collision: trim tail

draw food snake = do
  -- Compute content first
  let content = drawLines "" 0 fieldY food snake
  let line = replicate fieldX '━'
  let buf = "┏" ++ line ++ "┓\n" ++ content ++ "┗" ++ line ++ "┛\n"

  -- Flush and draw screen afterwards
  setCursorPosition 0 0
  putStr buf

drawLines content lineNum maxLines food snake
  | lineNum < maxLines = do
      let newContent = content ++ "┃" ++ drawLine "" 0 fieldX lineNum food snake ++ "┃\n"
      drawLines newContent (lineNum+1) maxLines food snake
  | otherwise = content

drawLine line colNum maxCols lineNum food snake
  | colNum < maxCols = do
      let newLine = line ++ getPixel colNum lineNum food snake
      drawLine newLine (colNum+1) maxCols lineNum food snake
  | otherwise = line

getPixel x y food snake
  | Point x y == food = "◈"
  | Point x y == head snake = "●"
  | Point x y `elem` snake = "○"
  | otherwise = " "
