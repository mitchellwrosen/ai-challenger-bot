{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import Lib

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Time.Clock
import           Data.Time.Format
import           System.IO
import           Text.Printf

type Player = Int
type Energy = Int
type Cell   = (Player, Int, Int)

data Board = Board
    { boardWidth :: Int
    , boardHeight :: Int
    , boardCells :: [Cell]
    } deriving Show

overCells :: ([Cell] -> [Cell]) -> Board -> Board
overCells f (Board w h cs) = Board w h (f cs)

data JudgeMessage = JudgeMessage Player Energy Board
  deriving Show

type Order = [Command]
type Command = (Int, Int)

data BotEnv = BotEnv
    { botIteration :: Int
    , botLogHandle :: Handle
    }

newtype Bot a = Bot { unBot :: ReaderT BotEnv IO a }
  deriving (Functor, Applicative, Monad, MonadReader BotEnv, MonadIO)

runBot :: FilePath -> Bot a -> IO ()
runBot path bot = withFile path WriteMode $ \h -> do
    hSetBuffering h NoBuffering
    forM_ [1..200] $ \n -> runReaderT (unBot bot) (BotEnv n h)

logMsg :: String -> Bot ()
logMsg str = do
    BotEnv n h <- ask
    t <- liftIO getCurrentTime

    let msg = printf "[#%d] " n
           ++ (formatTime defaultTimeLocale "[%H:%M:%S%Q] " t)
           ++ str
    liftIO $ hPutStrLn h msg

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runBot "/tmp/oink.log" bot

bot :: Bot ()
bot = do
    jmsg <- recvJudgeMessage
    logMsg "Got judge message:"
    logMsg (unlines jmsg)

    let JudgeMessage player energy Board{..} = parseJudgeMessage (init jmsg)

    logMsg "Parsed judge message:"
    logMsg $ "player: " ++ show player
    logMsg $ "energy: " ++ show energy
    logMsg $ "board width: " ++ show boardWidth
    logMsg $ "board height: " ++ show boardHeight
    logMsg $ "board cells: " ++ show boardCells

    let occupiedCells :: Set (Int, Int)
        occupiedCells = S.fromList (map (\(_,x,y) -> (x,y)) boardCells)

        ourCells :: [Cell]
        ourCells = filter (\(p,_,_) -> p == player) boardCells

        adjacentCells :: [(Int, Int)]
        adjacentCells = do
            (_, x, y) <- ourCells
            (x', y')  <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
            guard (x' >= 0 && x' < boardWidth &&
                   y' >= 0 && y' < boardHeight)
            pure (x', y')

    logMsg "occupied cells:"
    logMsg (show occupiedCells)

    logMsg "our cells:"
    logMsg (show ourCells)

    logMsg "adjacent cells:"
    logMsg (show adjacentCells)

    let order :: Order
        order = take energy $ S.toList (S.fromList adjacentCells S.\\ occupiedCells)

    logMsg "writing order:"
    logMsg (showOrder order)

    liftIO (putStrLn (showOrder order))

recvJudgeMessage :: Bot [String]
recvJudgeMessage = do
    xs <- liftIO getLine
    case xs of
        "." -> pure ["."]
        x   -> (x :) <$> recvJudgeMessage

parseJudgeMessage :: [String] -> JudgeMessage
parseJudgeMessage (('Y':' ':player):('E':' ':energy):bs) =
    JudgeMessage (read player) (read energy) (parseBoard bs)

parseBoard :: [String] -> Board
parseBoard rs = Board (length (head rs)) (length rs) (concatMap f (zip [0..] rs))
  where
    f :: (Int, String) -> [Cell]
    f (h, cs) = map readPlayer (filter isPlayer (zipWith (\w c -> (c, w, h)) [0..] cs))
      where
        isPlayer :: (Char, Int, Int) -> Bool
        isPlayer ('-', _, _) = False
        isPlayer _           = True

        readPlayer :: (Char, Int, Int) -> (Player, Int, Int)
        readPlayer (c, x, y) = (read [c], x, y)

showOrder :: Order -> String
showOrder [] = "."
showOrder (x:xs) = showCmd x ++ "\n" ++ showOrder xs
  where
    showCmd :: Command -> String
    showCmd (x,y) = "C " ++ show x ++ " " ++ show y
