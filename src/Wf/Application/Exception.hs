{-# LANGUAGE FlexibleContexts #-}
module Wf.Application.Exception
( Exception
, throwException
, throwSomeException
, liftException
) where

import qualified Control.Exception (Exception, SomeException(..))
import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Exception (Exc, throwExc)
import Control.Eff.Lift (Lift, lift)
import Control.Exception (try, SomeException)

type Exception = Exc Control.Exception.SomeException

throwException :: (Control.Exception.Exception e, Member (Exc Control.Exception.SomeException) r) => e -> Eff r a
throwException = throwExc . Control.Exception.SomeException

throwSomeException :: (Member (Exc Control.Exception.SomeException) r) => Control.Exception.SomeException -> Eff r a
throwSomeException = throwExc

liftException :: (SetMember Lift (Lift IO) r, Member Exception r)
            => IO a -> Eff r a
liftException = (f =<<) . lift . try
    where f (Right a) = return a
          f (Left e) = throwSomeException (e :: SomeException)
