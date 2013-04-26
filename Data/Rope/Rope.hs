{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rope.Rope
where

import Control.DeepSeq
import Control.Monad.Par
import Control.Monad     (when)
import Data.IORef        (IORef, newIORef, readIORef, writeIORef)
import Debug.Trace       (trace, traceEvent)
import System.IO.Unsafe  (unsafePerformIO)

import Prelude hiding (length, splitAt)
import qualified Prelude as P

import qualified Data.Vector.Generic as G

import qualified Data.Rope.Seq as Seq
import Data.Rope.Seq (Progress(..), Seq)

-----------------------------------------------------------------------------------------------
--
-- Definitions and constants
--
-----------------------------------------------------------------------------------------------

mAX_LEAF_SIZE_ref :: IORef Int
{-# NOINLINE mAX_LEAF_SIZE_ref #-}
mAX_LEAF_SIZE_ref = unsafePerformIO (newIORef 512)

set_mAX_LEAF_SIZE :: Int -> IO ()
set_mAX_LEAF_SIZE n = trace ("max leaf size set to " ++ (show n)) $ writeIORef mAX_LEAF_SIZE_ref n

mAX_LEAF_SIZE :: Int
mAX_LEAF_SIZE = unsafePerformIO $ readIORef mAX_LEAF_SIZE_ref

data Rope a = Leaf !(Seq a)
            | Cat  {-# UNPACK #-} !Int  -- ^ Length --Of all sequences in left and right branches
                   {-# UNPACK #-} !Int  -- ^ Depth  --Max depth from this node.
                   !(Rope a)            -- ^ Left branch
                   !(Rope a)            -- ^ Right branch
 deriving (Show)

instance NFData (Rope a) where
    rnf (Leaf xs)     = rnf xs
    rnf (Cat l d v w) = l `seq` d `seq` rnf v `seq` rnf w

data Dir = L | R
 deriving (Show)

-----------------------------------------------------------------------------------------------
--
-- Functions that convert between Ropes and other structures
--
-----------------------------------------------------------------------------------------------

fromVector :: G.Vector v a => v a -> Rope a
{-# INLINE fromVector #-}
fromVector = Leaf . Seq.fromVector

toVector :: G.Vector v a => Rope a -> v a
{-# INLINE toVector #-}
toVector (Leaf xs)       = Seq.toVector xs
toVector (Cat _ _ xs ys) = toVector xs G.++ toVector ys

fromList :: [a] -> Rope a
fromList xs = go xs (P.length xs)
  where
    go xs len | len < mAX_LEAF_SIZE = leaf (Seq.fromList xs)
              | otherwise           = ncat  (go (take len' xs) len')
                                            (go (drop len' xs) (len - len'))
      where
        len' = len `div` 2

toList :: Rope a -> [a]
toList (Leaf xs)         = Seq.toList xs
toList (Cat _ _ rp1 rp2) = toList rp1 ++ toList rp2

-----------------------------------------------------------------------------------------------
--
-- Primitive Rope operations
--
-----------------------------------------------------------------------------------------------

leaves :: Rope a -> [Seq a]
leaves (Leaf xs)         = [xs]
leaves (Cat _ _ rp1 rp2) = leaves rp1 ++ leaves rp2

toSeq :: Rope a -> Seq a
toSeq rp = foldr1 Seq.append (leaves rp)

length :: Rope a -> Int
{-# INLINE length #-}
length (Leaf xs)       = Seq.length xs
length (Cat len _ _ _) = len

isEmpty :: Rope a -> Bool
{-# INLINE isEmpty #-}
isEmpty rp = length rp == 0

depth :: Rope a -> Int
{-# INLINE depth #-}
depth (Leaf _)      = 0
depth (Cat _ d _ _) = d

empty :: Rope a
{-# INLINE empty #-}
empty = Leaf Seq.empty

singleton :: a -> Rope a
{-# INLINE singleton #-}
singleton x = Leaf (Seq.singleton x)

leaf :: Seq a -> Rope a
{-# INLINE leaf #-}
leaf xs | Seq.length xs > mAX_LEAF_SIZE = error "Leaf too large"
        | otherwise                     = Leaf xs

-- | Check that a rope's length and depth fields are correct
check :: Rope a -> Bool
check (Leaf _) = True
check rp@(Cat len dep rp1 rp2) = len == length' rp &&
                                 dep == depth' rp &&
                                 check rp1 &&
                                 check rp2
  where
    length' :: Rope a -> Int
    length' (Leaf xs)         = Seq.length xs
    length' (Cat _ _ rp1 rp2) = length' rp1 + length' rp2

    depth' :: Rope a -> Int
    depth' (Leaf _)          = 0
    depth' (Cat _ _ rp1 rp2) = max (depth' rp1) (depth' rp2) + 1

-- | Test whether or not a rope is balanced
isBalanced :: Rope a -> Bool
isBalanced (Leaf _) = True
isBalanced rp       = depth rp <= 2 * ceilingLg (length rp)
  where
    ceilingLg :: Int -> Int
    ceilingLg x = ceiling (log (fromIntegral x) / log 2)

-- | Rebalance a rope
balance :: Rope a -> Rope a
balance rp
    | isBalanced rp                   = rp
    | len <= mAX_LEAF_SIZE || len < 2 = leaf (toSeq rp)
    | otherwise                       = balance rp1 `ncat` balance rp2
  where
    (rp1, rp2) = splitAt rp (len `div` 2 - 1)

    len :: Int
    len = length rp

rebuildMD :: Rope a -> (Rope a, Int, Int)
rebuildMD (Leaf x) = ((Leaf x), Seq.length x, 0)
rebuildMD (Cat _ _ v w) = let (l, ll, ld) = rebuildMD v
                              (r, rl, rd) = rebuildMD w
                          in ((Cat (ll+rl) ((max ld rd)+1) l r), ll+rl, (max ld rd) + 1)

-- | Take n from the rightmost leaf in the left branch and place it in the leftmost leaf
-- in the right branch. Ignores max leaf size. Updates length.
--leftToRight :: Int -> Rope a -> Rope a
--leftToRight n (Cat l d v w) = leftMost 

-----------------------------------------------------------------------------------------------
--
-- Rope splitting and concatenation
--
-----------------------------------------------------------------------------------------------

-- | Non-coalescing rope concatenation
-- Creates a new node with the two parameter ropes as the two (left, right) subtrees
ncat :: Rope a -> Rope a -> Rope a
{-# INLINE ncat #-}
ncat rp1 rp2 = Cat len dep rp1 rp2
  where
    len, dep :: Int
    len = length rp1 + length rp2
    dep = max (depth rp1) (depth rp2) + 1

-- | Coalescing rope concatenation
-- As with ncat, but creates a Leaf if the two parameter ropes are small enough.
ccat :: Rope a -> Rope a -> Rope a
{-# INLINE ccat #-}
ccat rp1 rp2 =
    case ncat rp1 rp2 of
      rp | length rp <= mAX_LEAF_SIZE -> Leaf (toSeq rp)
         | otherwise                  -> rp

-- | Rope concatenation, ensuring a balanced result
cat :: Rope a -> Rope a -> Rope a
{-# INLINE cat #-}
cat rp1 rp2 = balance (rp1 `ncat` rp2)

-- | Split a rope into two (nearly equally sized) halves
split :: Rope a -> (Rope a, Rope a)
split (Leaf xs)         = let (xs1, xs2) = Seq.split xs
                          in (Leaf xs1, Leaf xs2)
split (Cat _ _ rp1 rp2) = (rp1, rp2)

-- | Split a rope at index 'i'
splitAt :: Rope a -> Int -> (Rope a, Rope a)
splitAt (Leaf xs) i =
    (Leaf xs1, Leaf xs2)
  where
    (xs1, xs2) = Seq.splitAt i xs

splitAt (Cat _ _ l r) i
    | i == len_l - 1 = (l, r)
    | i <  len_l     = let (l1, l2) = splitAt l i
                       in
                         (l1, l2 `ncat` r)
    | otherwise      = let (r1, r2) = splitAt r (i - len_l)
                       in
                         (l `ncat` r1, r2)
  where
    len_l :: Int
    len_l = length l

(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a1,b1) +++ (a2,b2) = (a1+a2, b1+b2)

--Split ropes into halves
-- (a, b, c, d)
-- a is the list of ropes that are prior to b that do not contain the midpoint
-- b is the rope that contains the midpoint
-- c the element offset into b where the midpoint os
-- d the rest of the ropes after the midpoint. 
-- so, left rope is a ++ b[:c]
--    right rope is b[c:] ++ d
divideRopes :: [Rope a] -> Int -> ([Rope a], Rope a, Int, [Rope a])
divideRopes (rp:rps) n | n <= length rp =
    ([], rp, n, rps)
divideRopes (rp:rps) n =
    let (rps1, rp', n', rps2) = divideRopes rps (n - length rp)
    in
      (rp:rps1, rp', n', rps2)

encodeRopes :: forall a . [Rope a] -> (Rope a, Int)
encodeRopes rps =
    (go rps, P.length rps)
  where
    go :: [Rope a] -> Rope a
    go []       = error "encodeRopes: can't happen"
    go [rp]     = rp
    go (rp:rps) = rp `ncat` go rps

decodeRope :: Rope a -> Int -> [Rope a]
decodeRope rp            1 = [rp]
decodeRope (Cat _ _ l r) n = l:decodeRope r (n-1)
decodeRope (Leaf {})     _ = error "decodeRope: can't happen"