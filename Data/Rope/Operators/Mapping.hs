{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rope.Operators.Mapping
 (mapLTS, filterLTS, UnzipMapCtx(..), curUnzip, splitAtAsCur) --, MapCtx, MapCur, MCTop, MCLeft, MCRight
 --, next, leftmost, curLength, splitCur, join, root)
where

import Data.Rope.Rope --(Rope, length, depth, divideRopes, decodeRope, encodeRopes, mAX_LEAF_SIZE)

import Debug.Trace       (trace, traceEvent)

import qualified Data.Rope.Seq as Seq
import Data.Rope.Seq (Progress(..), Seq)

import Control.Monad.Par

import Prelude hiding (length, splitAt)
import qualified Prelude as P

data MCTop
data MCLeft
data MCRight

data MapCtx a b = MCTop
                | MCLeft  (MapCtx a b) (Rope a)
                | MCRight (Rope b)     (MapCtx a b)
 deriving (Show)

data MapCur a b = MapCur (Seq b) (Seq a) (MapCtx a b)
 deriving (Show)

data UnzipMapCtx a b = UnzipMapCtx [Rope b] [Rope a] [Dir]
 deriving (Show)

type UnzipMapCur a b = UnzipMapCtx a b

data MapCurReb b = MapCurReb [Rope b] [Dir] [Dir]
    {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
 deriving (Show)

--
-- Helper Functions
--

plug :: Rope a -> MapCtx a a -> Rope a
plug rp MCTop = rp
plug rp (MCLeft  ctx' rrp)  = plug (rp `ncat` rrp) ctx'
plug rp (MCRight lrp  ctx') = plug (lrp `ncat` rp) ctx'

ctxLength :: MapCtx a b -> (Int, Int)
ctxLength MCTop = (0, 0)
ctxLength (MCLeft  ctx' rrp)  = ctxLength ctx' +++ (0, length rrp)
ctxLength (MCRight lrp  ctx') = ctxLength ctx' +++ (length lrp, 0)

ctxDepth :: MapCtx a b -> (Int, Int)
ctxDepth  MCTop = (0, 0)
ctxDepth (MCLeft  ctx' rrp)  = ctxDepth ctx' +++ (0, depth rrp)
ctxDepth (MCRight lrp  ctx') = ctxDepth ctx' +++ (depth lrp, 0)

curDepth :: MapCur a b -> (Int, Int)
curDepth (MapCur pseq useq ctx) =
    ctxDepth ctx +++ (1, 1)

lengthRight :: MapCur a b -> Int
lengthRight cur = snd (curLength cur)

depthRight :: MapCur a b -> Int
depthRight cur = snd (curDepth cur)

--

ctxUnzip :: MapCtx a b -> UnzipMapCtx a b
ctxUnzip MCTop         = UnzipMapCtx [] [] []
ctxUnzip (MCLeft c r)  = let UnzipMapCtx ls rs ds = ctxUnzip c
                         in
                           UnzipMapCtx ls (r:rs) (L:ds)
ctxUnzip (MCRight l c) = let UnzipMapCtx ls rs ds = ctxUnzip c
                         in
                           UnzipMapCtx (l:ls) rs (R:ds)

curUnzip :: MapCur a b -> UnzipMapCur a b
curUnzip (MapCur pseq useq ctx) =
    UnzipMapCtx (Leaf pseq:ls) (Leaf useq:rs) ds
  where
    (UnzipMapCtx ls rs ds) = ctxUnzip ctx

ctxZip :: UnzipMapCtx a b -> MapCtx a b
ctxZip (UnzipMapCtx [] [] []) = MCTop
ctxZip (UnzipMapCtx ls (r:rs) (L:ds)) = MCLeft (ctxZip (UnzipMapCtx ls rs ds)) r
ctxZip (UnzipMapCtx (l:ls) rs (R:ds)) = MCRight l (ctxZip (UnzipMapCtx ls rs ds))

curZip :: UnzipMapCur a b -> MapCur a b
curZip (UnzipMapCtx (Leaf pseq:ls) (Leaf useq:rs) ds) =
    MapCur pseq useq (ctxZip (UnzipMapCtx ls rs ds))

splitAtAsCur :: Rope a -> Int -> MapCur a a
splitAtAsCur rp n =
    s rp MCTop n
  where
    s (Leaf seq) ctx n = let (lseq, rseq) = Seq.splitAt n seq
                         in
                           (MapCur lseq rseq ctx)
    s (Cat _ _ lrp rrp) ctx n
        | n < length rrp = s lrp (MCLeft ctx rrp) n
        | otherwise      = s rrp (MCRight lrp ctx) (n - length lrp)

--
-- Exported
--
next :: Rope b -> MapCtx a b -> Progress (Seq a, MapCtx a b) (Rope b)
next rp MCTop              = Finished rp
next rp (MCLeft ctx' rrp)  = let (xs, ctx'') = leftmost rrp (MCRight rp ctx')
                             in
                               More (xs, ctx'')
next rp (MCRight lrp ctx') = next (lrp `ncat` rp) ctx'

leftmost :: Rope a -> MapCtx a b -> (Seq a, MapCtx a b)
leftmost (Leaf xs)         ctx = (xs, ctx)
leftmost (Cat _ _ lrp rrp) ctx = leftmost lrp (MCLeft ctx rrp)

curLength :: MapCur a b -> (Int, Int)
curLength (MapCur pseq useq ctx) =
    ctxLength ctx +++ (Seq.length pseq, Seq.length useq)

splitCur :: MapCur a b -> (Rope a, Rope a, MapCurReb b)
splitCur cur =
  let n = snd (curLength cur) `div` 2
      UnzipMapCtx ls rs ds = curUnzip cur
      (rps1, mrp, k, rps2) = divideRopes rs n
      UnzipMapCtx mls mrs mds = curUnzip (splitAtAsCur mrp k)
      n1 = P.length rps1
      n2 = P.length mrs
      (rp1, l1) = encodeRopes (rps1 ++ mls)
      (rp2, l2) = encodeRopes (mrs ++ rps2)
  in
    (rp1, rp2, MapCurReb ls ds mds n1 n2 l1 l2)

join :: Rope a -> Rope a -> MapCurReb b -> MapCur a b
join rp1 rp2 (MapCurReb ls ds mds n1 n2 l1 l2) =
    let  xs1 = decodeRope rp1 l1
         rps1 = take n1 xs1
         mls = drop n1 xs1
         xs2 = decodeRope rp2 l2
         mrs = take n2 xs2
         rps2 = drop n2 xs2
         mrp = root (curZip (UnzipMapCtx mls mrs mds))
         rs = rps1 ++ [mrp] ++ rps2
    in
      curZip (UnzipMapCtx ls rs ds)

root :: MapCur a a -> Rope a
root (MapCur xs ys ctx) = plug (Leaf (xs `Seq.append` ys)) ctx

--
-- Body
--

mapLTSUntil :: forall a b . Par Bool
            -> (a -> b)
            -> Rope a
            -> Par (Progress (MapCur a b) (Rope b))
mapLTSUntil cond f rp =
    m xs ctx
  where
    mSeq :: Seq b -> MapCtx a b -> Rope b
    mSeq xs ctx = case next (Leaf xs) ctx of
                    Finished rp' -> rp'
                    More (xs', ctx') -> mSeq (Seq.map f xs') ctx'

    m :: Seq a -> MapCtx a b -> Par (Progress (MapCur a b) (Rope b))
    m xs ctx =
        case Seq.mapUntil cond f xs of
          Finished pseq' -> case next (Leaf pseq') ctx of
                              Finished rp'-> return $ Finished rp'
                              More (seq', ctx') -> m seq' ctx'
          More (pseq', useq') -> if snd (curLength (MapCur pseq' useq' ctx)) >= 2
                                 then return $ More (MapCur pseq' useq' ctx)
                                 else return $ Finished (mSeq (pseq' `Seq.append` Seq.map f useq') ctx)

    xs :: Seq a
    ctx :: MapCtx a b
    (xs, ctx) = leftmost rp MCTop

mapLTS :: forall a b . (a -> b) -> Rope a -> Par (Rope b)
{-# INLINE mapLTS #-}
mapLTS f rp0 =
    go rp0
  where
    go :: Rope a -> Par (Rope b)
    go rp@(Leaf u)
        | len < mAX_LEAF_SIZE = do  prog <- mapLTSUntil isHungry' f rp --isHungry f rp
                                    case prog of
                                      Finished rp' -> return $ traceEvent ("finished: "  ++ show (length rp')) $ rp'
                                      More cur' -> do  let (rp1, rp2, reb) = splitCur cur'
                                                       prp1' <- traceEvent ("spawn left: "  ++ show (length rp1)) $
                                                                spawn (mapLTS f rp1)
                                                       prp2' <- traceEvent ("spawn right: " ++ show (length rp2)) $
                                                                spawn (mapLTS f rp2)
                                                       rp1'  <- get prp1'
                                                       rp2'  <- get prp2'
                                                       return $ root (join rp1' rp2' reb)

        | otherwise           = do  let (v, w) = traceEvent "split" $ Seq.splitAt (len `div` 2) u
                                    iv'  <- spawn $ go (Leaf v)
                                    iw'  <- spawn $ go (Leaf w)
                                    v'   <- get iv'
                                    w'   <- get iw'
                                    return $ traceEvent "ncat" $ v' `cat` w'
      where
        len :: Int
        len = Seq.length u

    go (Cat l d v w) = do  iv' <- spawn $ go v
                           iw' <- spawn $ go w
                           v' <- get iv'
                           w' <- get iw'
                           return $ traceEvent "Cat" $ Cat l d v' w'

filterLTSUntil :: forall a . Par Bool
            -> (a -> Bool)
            -> Rope a
            -> Par (Progress (MapCur a a) (Rope a))
filterLTSUntil cond f rp =
    m xs ctx
  where
    mSeq :: Seq a -> MapCtx a a -> Rope a
    mSeq xs ctx = case next (Leaf xs) ctx of
                    Finished rp' -> rp'
                    More (xs', ctx') -> mSeq (Seq.filter f xs') ctx'

    m :: Seq a -> MapCtx a a -> Par (Progress (MapCur a a) (Rope a))
    m xs ctx =
        case Seq.filterUntil cond f xs of
          Finished pseq' -> case next (Leaf pseq') ctx of
                              Finished rp'-> return $ Finished rp'
                              More (seq', ctx') -> m seq' ctx'
          More (pseq', useq') -> if snd (curLength (MapCur pseq' useq' ctx)) >= 2
                                 then return $ More (MapCur pseq' useq' ctx)
                                 else return $ Finished (mSeq (pseq' `Seq.append` Seq.filter f useq') ctx)

    xs :: Seq a
    ctx :: MapCtx a a
    (xs, ctx) = leftmost rp MCTop

filterLTS :: forall a . (a -> Bool) -> Rope a -> Par (Rope a)
{-# INLINE filterLTS #-}
filterLTS f rp0 =
    go rp0
  where
    go :: Rope a -> Par (Rope a)
    go rp@(Leaf u)
        | len < mAX_LEAF_SIZE = do prog <- filterLTSUntil isHungry' f rp --isHungry f rp
                                   case prog of
                                     Finished rp' -> return $ rp'
                                     More cur' -> do let (rp1, rp2, reb) = splitCur cur'
                                                     prp1' <- spawn (filterLTS f rp1)
                                                     prp2' <- spawn (filterLTS f rp2)
                                                     rp1' <- get prp1'
                                                     rp2' <- get prp2'
                                                     return $ root (join rp1' rp2' reb)
        | otherwise            = do let (v, w) = Seq.splitAt (len `div` 2) u
                                    iv' <- spawn $ go (Leaf v)
                                    iw' <- spawn $ go (Leaf w)
                                    v' <- get iv'
                                    w' <- get iw'
                                    return $ v' `cat` w'
      where
        len :: Int
        len = Seq.length u

    go (Cat l d v w) = do iv' <- spawn $ go v
                          iw' <- spawn $ go w
                          v' <- get iv'
                          w' <- get iw'
                          return $ Cat l d v' w'

isFalse :: Par Bool
isFalse = do return False
isHungry' :: Par Bool
isHungry' = do return runParHungry
