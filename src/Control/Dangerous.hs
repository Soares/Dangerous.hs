module Control.Dangerous
    ( Exit(..)
    , Warning(..)
    , Dangerous(..)
    , DangerousT(..)
    , Errorable(..)
    , succeeded
    , failed
    , stopped
    , exited
    , warnings
    , result
    , execute
    , extract
    , dangerize
    ) where


import Prelude hiding ( log )
import Control.Arrow
import Control.Monad.Trans
import System.Exit
import System.IO
import Text.Printf


-- A signal that computation should exit.
data Exit = Stop String     -- The computation was succesful
          | Failure String  -- The computation failed
          | Exit Int String -- The computation failed with an error code

instance Show Exit where
    show (Stop s) = printf "Stop: %s" s
    show (Exit n s) = printf "Error(%d): %s" n s
    show (Failure s) = printf "Error: %s" s


-- A wrapped warning message
data Warning = Warning String

instance Show Warning where
    show (Warning w) = printf "Warning: %s" w


-- A class of computation that can both log warnings and exit early
class (Monad m) => Errorable m where
    -- Log warnings
    log :: Warning -> m ()
    warn :: (Show w) => w -> m ()
    warn = log . Warning . show

    -- Stop the computation
    exit :: Exit -> m a
    exit_ :: Exit -> m ()
    exit_ x = exit x >> return ()

    -- Exit with a showable error and an error code
    die :: (Show s) => Int -> s -> m a
    die_ :: (Show s) => Int -> s -> m ()
    die n = exit . Exit n . show
    die_ n = (>> return ()) . die_ n

    -- Exit with a showable error
    throw :: (Show s) => s -> m a
    throw_ :: (Show s) => s -> m ()
    throw = exit . Failure . show
    throw_ = (>> return ()) . throw_

    -- Exit successfully with a showable message
    stop :: (Show s) => s -> m a
    stop_ :: (Show s) => s -> m ()
    stop = exit . Stop . show
    stop_ = (>> return ()) . stop_


-- The Dangerous Monad
-- Preforms computations that can be exited early and result in warnings
data Dangerous a = Dangerous { runDangerous :: (Either Exit a, [Warning]) }

instance Functor Dangerous where
    fmap f (Dangerous (Right v, ws)) = Dangerous (Right (f v), ws)
    fmap _ (Dangerous (Left e, ws)) = Dangerous (Left e, ws)

instance Monad Dangerous where
    fail s = Dangerous (Left $ Failure s, [])
    return x = Dangerous (Right x, [])
    (Dangerous (Left e, ws)) >>= _ = Dangerous (Left e, ws)
    (Dangerous (Right v, ws)) >>= f = Dangerous $
        second (ws ++) $ runDangerous $ f v

instance Errorable Dangerous where
    log w = Dangerous (Right (), [w])
    exit x = Dangerous (Left x, [])


-- The Dangerous Monad Transformer
data DangerousT m a = DangerousT {
    runDangerousT :: m (Either Exit a, [Warning]) }

instance (Functor m) => Functor (DangerousT m) where
    fmap f (DangerousT mapable) = DangerousT (fmap (first apply) mapable) where
        apply (Right v) = Right $ f v
        apply (Left e) = Left e

instance (Monad m) => Monad (DangerousT m) where
    fail s = DangerousT (return (Left $ Failure s, []))
    return = lift . return
    (DangerousT m) >>= f = DangerousT $ m >>= \(r, ws) -> case r of
        Right x -> runDangerousT (f x) >>= return . second (ws ++)
        Left e -> return (Left e, ws)

instance MonadTrans DangerousT where
    lift x = DangerousT $ x >>= (\v -> return (Right v, []))

instance (MonadIO m) => MonadIO (DangerousT m) where
  liftIO = lift . liftIO

instance (Monad m) => Errorable (DangerousT m) where
    log w = DangerousT $ return (Right (), [w])
    exit x = DangerousT $ return (Left x, [])


-- Functions that work on dangerous results
succeeded :: (Either Exit a, [Warning]) -> Bool
succeeded (Right _, _) = True
succeeded _ = False

exited :: (Either Exit a, [Warning]) -> Bool
exited (Left _, _) = True
exited _ = False

stopped :: (Either Exit a, [Warning]) -> Bool
stopped (Left (Stop _), _) = True
stopped _ = False

failed :: (Either Exit a, [Warning]) -> Bool
failed e = exited e && not (stopped e)

warnings :: (Either Exit a, [Warning]) -> [Warning]
warnings = snd

result :: (Either Exit a, [Warning]) -> Either Exit a
result = fst

execute :: (Either Exit a, [Warning]) -> IO a
execute (r, ws) = mapM_ (hPrint stderr) ws >> extract r

extract :: Either Exit a -> IO a
extract (Left (Stop s)) = putStrLn s >> exitSuccess
extract (Left (Failure s)) = hPutStrLn stderr s >> exitFailure
extract (Left (Exit n s)) = hPutStrLn stderr s >> exitWith (ExitFailure n)
extract (Right a) = return a

dangerize :: (Errorable m, Show s) => Either s a -> m a
dangerize (Left e) = throw e
dangerize (Right v) = return v
