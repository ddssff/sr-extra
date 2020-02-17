{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies #-}

-- | Type level "sets" as a cons list constructed of Eithers.

module Extra.ErrorSet
  ( Insert
  , Delete
  , Member(follow, delete, insert)
  , OneOf(OneOf)
  ) where

import Control.Lens
import Data.SafeCopy
import Data.Proxy (Proxy(Proxy))
import Generic.Data.Internal.Newtype (Newtype, Old, pack, unpack)
import GHC.Generics
import Unsafe.Coerce

-- See SafeCopy issue #78
-- instance SafeCopy Void

type family Insert t s where
  Insert t (Either t a) = Either t a
  Insert t () = Either t ()
  Insert t (Either a b) = Either a (Insert t b)
  Insert t (OneOf es) = OneOf (Insert t es)

type family Delete t s where
  Delete t (Either t a) = a
  Delete t () = ()
  Delete t (Either a b) = Either a (Delete t b)
  Delete t (OneOf es) = OneOf (Delete t es)

class Member t set where
  follow :: Prism' set t
  delete :: Proxy t -> set -> Either t (Delete t set)
  insert :: Proxy t -> Delete t set -> set

instance Member t (Either t a) where
  follow = _Left
  delete :: Proxy t -> (Either t a) -> Either t (Delete t (Either t a)) -- Maybe a
  delete proxy (Left t) = Left t
  delete proxy (Right a) = Right a -- We know a doesn't contain t, so return it
  insert :: a ~ Delete t (Either t a) => Proxy t -> a -> Either t a
  insert proxy = Right -- there can't be a t in Delete t a

instance {-# OVERLAPS #-} Member t b => Member t (Either a b) where
  follow = _Right . follow
  delete proxy (Left a) =
    -- We know t is not a so this Left will remain a Left
    Right (unsafeCoerce (Left a) :: Delete t (Either a b))
  delete proxy (Right b) =
    -- We know t is somewhere in b
    case delete proxy b of
      -- we found t and deleted it
      Left t -> Left t
      -- We found something other than t.  The compiler doesn't know
      -- that somewhere in b there is a t, so we have to tell it what
      -- Delete t (Either a b) is.
      Right r -> Right (unsafeCoerce (Right r) :: Delete t (Either a b))
  insert :: Proxy t -> Delete t (Either a b) -> Either a b
  -- Tell the compiler that Delete t (Either a b) ~ Either a (Delete t b) here
  insert proxy d = case (unsafeCoerce d :: Either a (Delete t b)) of
                     Left a -> Left a
                     Right d' -> Right (insert proxy d')

instance Member t () where
  follow = error "Type Set Error"
  delete proxy = error "Type Set Error"
  insert proxy = error "Type Set Error"

newtype OneOf es = OneOf es deriving (Eq, Ord, Show, Functor)

instance Member e es => Member e (OneOf es) where
  follow = iso (\(OneOf es) -> es) OneOf . follow
  delete proxy (OneOf es) = fmap OneOf (delete proxy es)
  insert proxy (OneOf es) = OneOf (insert proxy es)
