module Main (main) where

import System.IO
import System.Random
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

data Action = Action Integer Integer

instance Show Action where
    show (Action s g) = "show " ++ show s ++ ", guess " ++ show g

data Winner = P1 | P2 | Nobody deriving (Eq, Show)

evalWinner :: Action -> Action -> Winner
evalWinner (Action s1 g1) (Action s2 g2)
    | g1 == g2      = Nobody
    | g1 == s1 + s2 = P1
    | g2 == s1 + s2 = P2
    | otherwise     = Nobody

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

humanAction :: IO Action
humanAction = do
    s <- read <$> prompt "Your show: "
    g <- read <$> prompt "Your guess: "
    return $ Action s g

aiAction :: IO Action
aiAction = do
    s  <- randomRIO (0, 5) -- ai showing
    s' <- randomRIO (0, 5) -- guess op
    return $ Action s (s + s')

oneGame :: IO Action -> IO Action -> IO Winner
oneGame p1 p2 = do
    a1 <- p1
    a2 <- p2
    putStrLn $ "P1 try: " ++ show a1
    putStrLn $ "P2 try: " ++ show a2

    let winner = evalWinner a1 a2

    if winner == Nobody
    then oneGame p1 p2
    else return winner

newtype Player = Player { score :: Integer }

instance Show Player where
    show = show . score

addScore :: Integer -> Player -> Player
addScore n p = p { score = score p + n }

data Game = Game
    { player1 :: Player
    , player2 :: Player
    }

recordWinner1 :: Game -> Game
recordWinner1 game = Game
    { player1 = addScore 1 (player1 game)
    , player2 = player2 game
    }

recordWinner2 :: Game -> Game
recordWinner2 game = Game
    { player1 = player1 game
    , player2 = addScore 1 (player2 game)
    }

recordWinner :: Winner -> Game -> Game
recordWinner winner game = case winner of
    P1 -> recordWinner1 game
    P2 -> recordWinner2 game
    _ -> error "recordWinner error"

mkGame :: Game
mkGame = Game
    { player1 = Player 0
    , player2 = Player 0
    }

playGame :: IO Action -> IO Action -> StateT Game IO ()
playGame a1 a2 = do
    winner <- liftIO $ oneGame a1 a2
    liftIO $ putStrLn $ "winner: " ++ show winner

    st <- get
    let st' = recordWinner winner st
    liftIO $ putStrLn $ "player 1: " ++ show (player1 st') ++ ", "
                     ++ "player 2: " ++ show (player2 st')
    put st'

    continue <- liftIO $ prompt "one more? "
    when (continue == "Y") (playGame a1 a2)

main :: IO ()
main = do
    _ <- runStateT (playGame aiAction aiAction) mkGame
    return ()
