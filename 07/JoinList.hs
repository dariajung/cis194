module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m jl1 jl2) = tag (jl1) `mappend` tag (jl2)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- For any index i and join-list jl, it should be the case that
-- (indexJ i jl) == (jlToList jl !!? i)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
--indexJ _ Empty = Nothing
indexJ 0 (Single m a) = Just a
indexJ _ (Single m a) = Nothing
indexJ 0 (Append m jl1 jl2) = indexJ 0 jl1

indexJ n (Append m jl1@(Single m1 _) jl2)
    | n >= (getSize $ size m)           = Nothing
    | n < (getSize $ size m1)           = indexJ n jl1
    | otherwise                         = indexJ (n - (getSize $ size m1)) jl2

indexJ n (Append m jl1@(Append m1 _ _) jl2)
    | n >= (getSize $ size m)           = Nothing
    | n < (getSize $ size m1)           = indexJ n jl1
    | otherwise                         = indexJ (n - (getSize $ size m1)) jl2


-- jlToList (dropJ n jl) == drop n (jlToList jl)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ _ (Single m a) = Empty






