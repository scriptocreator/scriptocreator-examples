module Scriptocreator.MayEither where


data MayEither a b
    = MyNothing
    | MyLeft a
    | MyRight b
  deriving (Show, Eq, Ord)

instance Functor (MayEither a) where
    fmap _ MyNothing = MyNothing
    fmap _ (MyLeft a) = MyLeft a
    fmap f (MyRight a) = MyRight $ f a

instance Applicative (MayEither a) where
    pure a = MyRight a
    _ <*> MyNothing = MyNothing
    _ <*> (MyLeft a) = MyLeft a
    (MyRight f) <*> (MyRight a) = MyRight $ f a

instance Monad (MayEither a) where
    return = pure
    MyNothing >>= _ = MyNothing
    (MyLeft a) >>= _ = MyLeft a
    (MyRight a) >>= f = f a
