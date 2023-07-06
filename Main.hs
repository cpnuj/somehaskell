{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main (main) where

import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Accum (add)
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))

listDir :: FilePath -> IO [String]
listDir = fmap (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    contents <- listDir path
    rest <- forM contents $ \name -> do
        let newName = path </> name
        isDir <- doesDirectoryExist newName
        if isDir then countEntriesTrad newName else return []
    return $ (path, length contents) : concat rest

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDir $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO $ doesDirectoryExist newName
        when isDir $ countEntries newName

newtype MyReader r a = MyReader { runMyReader :: r -> a }

instance Functor (MyReader r) where
    fmap :: (a -> b) -> MyReader r a -> MyReader r b
    fmap f (MyReader ra) = MyReader $ f . ra

instance Applicative (MyReader r) where
    pure :: a -> MyReader r a
    pure a = MyReader $ const a

    (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
    (<*>) (MyReader rab) (MyReader ra) = MyReader $ \r -> rab r (ra r)

instance Monad (MyReader r) where
    return :: a -> MyReader r a
    return = pure

    (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
    (>>=) (MyReader ra) a2rb = MyReader $ \r ->
        runMyReader (a2rb (ra r)) r

myask :: MyReader a a
myask = MyReader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

myasks :: (r -> a) -> MyReader r a
myasks = MyReader

newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person = Person
    { humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    }
    deriving (Eq, Show)

data Dog = Dog
    { dogsName :: DogName
    , dogsAddress :: Address
    }
    deriving (Eq, Show)

readDogsName :: Reader Person DogName
readDogsName = do
    p <- ask
    return $ dogName p

readDogsAddress :: Reader Person Address
readDogsAddress = do
    p <- ask
    return $ address p

readDog :: Reader Person Dog
readDog = do
    name <- readDogsName
    addr <- readDogsAddress
    return $ Dog name addr

newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }

instance Functor m => Functor (MyReaderT r m) where
    fmap :: (a -> b) -> MyReaderT r m a -> MyReaderT r m b
    fmap f (MyReaderT rma) = MyReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (MyReaderT r m) where
    pure :: a -> MyReaderT r m a
    pure a = MyReaderT $ pure (pure a)

    (<*>) :: MyReaderT r m (a -> b) -> MyReaderT r m a -> MyReaderT r m b
    (<*>) (MyReaderT rmab) (MyReaderT rma) = MyReaderT $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (MyReaderT r m) where
    return :: a -> MyReaderT r m a
    return = pure

    (>>=) :: MyReaderT r m a -> (a -> MyReaderT r m b) -> MyReaderT r m b
    (MyReaderT rma) >>= f = MyReaderT $ \r -> do
        a <- rma r
        runMyReaderT (f a) r

newtype PWriter a = PWriter (a, [String]) deriving (Eq, Show)

instance Functor PWriter where
    fmap :: (a -> b) -> PWriter a -> PWriter b
    fmap f (PWriter (a, s)) = PWriter (f a, s)

instance Applicative PWriter where
    pure :: a -> PWriter a
    pure a = PWriter (a, [])

    (<*>) :: PWriter (a -> b) -> PWriter a -> PWriter b
    (PWriter (f, s)) <*> (PWriter (a, s')) = PWriter (f a, s ++ s')

instance Monad PWriter where
    return :: a -> PWriter a
    return = pure

    (>>=) :: PWriter a -> (a -> PWriter b) -> PWriter b
    (PWriter (a, s)) >>= f =
        let (PWriter (b, s')) = f a in
        PWriter (b, s ++ s')

ptell :: String -> PWriter ()
ptell s = PWriter ((), [s])

gen :: Int -> PWriter Int
gen i = do
    ptell $ "gen " ++ show i
    return i

padd :: Int -> Int -> PWriter Int
padd i j = do
    i' <- gen i
    j' <- gen j
    ptell $ "add " ++ show i' ++ " " ++ show j'
    return $ i' + j'

-- padd' :: Int -> Int -> PWriter Int
-- padd' i j = gen i >>=
--     \i' -> gen j >>= \j' -> PWriter (i' + j', ["add " ++ show i' ++ " " ++ show j'])


-- readDog' :: MyReaderT Person Maybe Dog

main :: IO ()
main = putStrLn "somehaskell"