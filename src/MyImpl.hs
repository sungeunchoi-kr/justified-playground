{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.IORef
import Control.Concurrent
import Control.Monad


-- The 'PMap_c' is a **value constructor** taking in a 'M.Map k v'.
newtype PMap_t ph k v = PMap_c (M.Map k v) deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype PKey_t ph k = PKey_c k deriving (Eq, Ord, Show)

--newtype Index ph = Index Int deriving (Eq, Ord, Show)
--type role Index nominal


theMap :: PMap_t ph k v -> M.Map k v
theMap (PMap_c m) = m


withMap :: M.Map k v -- ^ The map to use as input
        -> (forall ph. PMap_t ph k v -> t) -- ^ The computation to apply
        -> t -- ^ The resulting value
withMap m cont = cont (PMap_c m)


member :: Ord k => k -> PMap_t ph k v -> Maybe (PKey_t ph k)
member k (PMap_c m) = fmap (const $ PKey_c k) (M.lookup k m)


lookup :: Ord k => PKey_t ph k -> PMap_t ph k v -> v
lookup (PKey_c k) (PMap_c m) = case M.lookup k m of
    Just value -> value
    Nothing    -> error "Data.Map.Justified has been subverted!"


deleting :: Ord k
         => k  -- ^ key to remove
         -> PMap_t ph k v -- ^ initial map
         -> (forall ph'. (PKey_t ph' k -> PKey_t ph k, PMap_t ph' k v) -> t) -- ^ continuation
         -> t
deleting k m cont = cont (qed, mmap (M.delete k) m)


-- | Coerce one map type to another, using a function on "Data.Map"'s @'Data.Map.Map'@.
mmap :: (M.Map k1 v1 -> M.Map k2 v2) -> PMap_t ph1 k1 v1 -> PMap_t ph2 k2 v2
mmap f (PMap_c m) = PMap_c (f m)

-- | Coerce key-existence evidence
qed :: PKey_t ph k -> PKey_t ph' k
qed (PKey_c k) = PKey_c k


main :: IO ()
main = do
    --crossLookupTest
    iorefTest


sampleMap = M.fromList [(1,2),(2,3),(3,4)] 
anotherMap = M.fromList [('a',1),('b',2),('c',3)]


crossLookupTest :: IO ()
crossLookupTest = do
    withMap anotherMap $ \map2 -> do
        withMap sampleMap $ \map1 -> do
            -- we prove that 1 exists in map1.
            case member 1 map1 of
                Just keyp1 -> do
                    putStrLn "proven!"
                    -- now, we try to use keyp1 (proven only for map1) on map2.
                    -- change 'map1' to 'map2' to see compilation fail.
                    let r = lookup keyp1 map1
                    putStrLn $ "Got value " ++ (show r)
                Nothing -> do
                    putStrLn "dne"

    putStrLn "done"


iorefTest :: IO ()
iorefTest = do
    withMap sampleMap $ \map -> do
        case member 1 map of
          Just key1 -> do
            ref <- newIORef map

            putStrLn "forking modifier"
            forkIO $ modifier ref

            threadDelay 1000000
            putStrLn "checking..."

            map' <- readIORef ref
            let v = lookup key1 map'

            putStrLn $ "got value " ++ (show v)

            return ()
          Nothing -> do return ()
        
    where
        modifier :: IORef (PMap_t ph Integer Integer) -> IO ()
        modifier ref = do
            modifyIORef ref $ \map' -> 
                -- and herein lies the fundamental problem if you try to 
                -- 're-insert' the modified 'delmap' into the 'ref' -- the 
                -- type simply does not match. All you can to is return the
                -- original, unmodified 'map''.
                --
                -- change map' to delmap to see compilation fail.
                deleting 1 map' $ \(f, delmap) -> map'

            map' <- readIORef ref
            -- and of course, trying to delete the element in the "underlying" 
            -- map is futile because the delete operation just returns a new 
            -- map without modifying the previous one.
            let delmap = M.delete 1 (theMap map')

            return ()

