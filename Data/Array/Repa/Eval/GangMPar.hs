{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Repa.Eval.GangMPar
        ( gangIOMPar )	
where
import Control.Monad.Par.IO as IO
import qualified Control.Monad.Par.Class as S
import Control.Monad.IO.Class

import Debug.Trace

gangIOMPar :: Int -> (Int -> IO ()) -> IO ()
gangIOMPar n f = do traceEventIO "+runParIO"
                    IO.runParIO $ do gangIO' n f
                    traceEventIO "-runParIO"

gangIO' :: Int -> (Int -> IO ()) -> ParIO ()
gangIO' 1 f = do liftIO $ f 0 --x <- S.spawn $ liftIO $ f 0
                 --y <- S.get x
                 --return ()
gangIO' n f = do x <- S.spawn $ liftIO $ (f (n-1))
                 gangIO' (n-1) f
                 S.get x --return (S.get x)
--                 return ()
