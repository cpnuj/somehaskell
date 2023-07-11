module Main (main) where

data Code =
      Add
    | Push Int
    | Pop
    deriving Show

data Machine = Machine
    { codes :: [Code]
    , stack :: [Int]
    , cpt   :: Int
    }
    deriving (Show)

main :: IO ()
main = putStrLn "VM.hs"