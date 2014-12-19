{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}
module Wf.Control.Eff.Logger
( log
, logDebug
, logNotice
, logInfo
, logWarn
, logError
, runLoggerStdIO
, Logger(..)
, LogLevel(..)
, LogOutputType
) where

import Control.Eff ((:>), Eff, Member, SetMember, handleRelay, inj, send, freeMap)
import Control.Eff.Lift (Lift, lift)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Prelude hiding (log)

data LogLevel = DEBUG | NOTICE | INFO | WARN | ERROR deriving (Show, Read, Eq, Enum, Ord, Typeable)

type family LogOutputType logger :: *

data Logger logger a = Logger logger LogLevel (LogOutputType logger) a deriving (Typeable, Functor)

log :: (Typeable logger, Member (Logger logger) r) => logger -> LogLevel -> LogOutputType logger -> Eff r ()
log logger lv s = send . inj $ Logger logger lv s ()

logDebug, logNotice, logInfo, logWarn, logError :: (Typeable logger, Member (Logger logger) r) => logger -> LogOutputType logger -> Eff r ()
logDebug = flip log DEBUG
logNotice = flip log NOTICE
logInfo = flip log INFO
logWarn = flip log WARN
logError = flip log ERROR

runLoggerStdIO :: (Show (LogOutputType logger), Typeable logger, Member (Lift IO) r, SetMember Lift (Lift IO) r)
               => LogLevel -> Eff (Logger logger :> r) a -> Eff r a
runLoggerStdIO minL = loop
    where
    loop = freeMap return $
        \u -> handleRelay u loop $
            \(Logger _ lv s f) -> when (lv >= minL) (lift $ p lv s) >> loop f
    p lv s = putStrLn $ "[" ++ show lv ++ "] " ++ show s
