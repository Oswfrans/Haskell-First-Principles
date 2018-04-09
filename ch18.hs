
--ch18 chapter excercises
--1
data Nope a = NopeDotJpg
instance Monad Nope where
  return = NopeDotJpg
  (>>=) = NopeDotJpg

--2
data PhhhbbtttEither b a = Left a | Right b
instance MOndad PhhhbbtttEither where
  return = Right
  Left l >>= _ = Left l
  Right r >>= k = k r
  --(>>=) =

--3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

----?
instance Functor Identity where fmap = Identity a

instance Applicative Identity where 
  ----?
  pure a = Identity a
  (Identity x) <*> f = f x

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x
  --(>>=) = undefined

--4
data List a = Nil | Cons a (List a)

instance Monad List where
  return x = [x]
  m >>= f = concat (map f m)
