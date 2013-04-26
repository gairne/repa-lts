{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rope 
 (mapLTS, filterLTS, reduceLTS, zipWithLTS
  , set_mAX_LEAF_SIZE, mAX_LEAF_SIZE
  , Rope(..)
  , fromVector, toVector, fromList, toList
  , leaves, toSeq, length, isEmpty, depth, empty, singleton, leaf
  , check, isBalanced, balance, rebuildMD
  , ncat, ccat, cat, split, splitAt
 )
where

import Data.Rope.Operators.Mapping (mapLTS, filterLTS)
import Data.Rope.Operators.Reduction (reduceLTS)
import Data.Rope.Operators.Zipping (zipWithLTS)
import Data.Rope.Rope
import qualified Prelude as P

-----------------------------------------------------------------------------------------------
--
-- Contextual Operations
--
-----------------------------------------------------------------------------------------------

-- mapP :: forall a b . (a -> b) -> Rope a -> Par (Rope b)
-- {-# INLINE mapP #-}
-- mapP f rp =
--     go rp
--   where
--     go :: Rope a -> Par (Rope b)
--     go (Leaf u)
--         | len < mAX_LEAF_SIZE = return $ traceEvent "map" $ Leaf (Seq.map f u)
--         | otherwise           = do  let (v, w) = traceEvent "split" $ Seq.splitAt (len `div` 2) u
--                                     iv'  <- spawn $ go (Leaf v)
--                                     iw'  <- spawn $ go (Leaf w)
--                                     v'   <- get iv'
--                                     w'   <- get iw'
--                                     return $ traceEvent "ncat" $ v' `ncat` w'
--       where
--         len :: Int
--         len = Seq.length u

--     go (Cat l d v w) = do  iv' <- spawn $ go v
--                            iw' <- spawn $ go w
--                            v' <- get iv'
--                            w' <- get iw'
--                            return $ traceEvent "Cat" $ Cat l d v' w'
