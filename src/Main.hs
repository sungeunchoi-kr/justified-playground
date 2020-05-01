{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Map.Justified

type Graph = Map Int

main :: IO ()
main = do
    test01

test01 :: IO ()
test01 = do
    load "1 2\n3 4" onLoad
    putStrLn "exit"

load :: String
     -> (forall ph. Map ph Int Int -> [Key ph Int] -> t)
     -> t
load str onComplete = withMap (toMap str) $ \map -> do
    onComplete map $ keys map
        where
            toMap :: String -> M.Map Int Int
            toMap str = M.fromList $ parseline <$> lines str

            parseline :: String -> (Int, Int)
            parseline line =
                let [a,b] = read <$> (words line) in (a,b)

-- from here, we have a verified map and its list of keys.
onLoad :: forall ph. Map ph Int Int -> [Key ph Int] -> IO ()
onLoad map keys = putStrLn $ show $ fmap (\key -> lookup key map) keys

