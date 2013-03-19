{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Repa.Eval.GangMPar
        ( gangIOMPar )	
where
import Control.Monad.Par.IO as IO
--import qualified Control.Monad.Par as P
import qualified Control.Monad.Par.Class as S
import Control.Monad.IO.Class

gangIOMPar :: Int -> (Int -> IO ()) -> IO ()
gangIOMPar n f = IO.runParIO $ do gangIO' n f

gangIO' :: Int -> (Int -> IO ()) -> ParIO ()
gangIO' 1 f = do x <- S.spawn $ liftIO $ f 0
                 _ <- S.get x
                 return ()
gangIO' n f = do x <- S.spawn $ liftIO $ (f (n-1))
                 _ <- gangIO' (n-1) f
                 _ <- S.get x
                 return ()
