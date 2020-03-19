{-# LANGUAGE NoStarIsType,DataKinds,PolyKinds,TypeFamilies,UndecidableInstances,TypeOperators #-}

import Data.Kind

infixr 0 $
infixr 5 :::
infixl 9 :*:

type family f $ x where
  f $ x = f x

type family If b x y where
  If True  x y  = x
  If False x y = y


data Pair a b = a :*: b

data Nat = Z | S Nat

type family a + b where
  Z   + b = b
  S a + b = S (a + b)

type family a * b where
  Z   * b = Z
  S Z * b = b
  S a * b = b + (a * b)


type family a < b where
  Z < S n = True
  S a < S b = a < b
  _ < _ = False


data List a = Nil | a ::: (List a)

type family Len xs where
  Len Nil         = Z
  Len (_ ::: xs) = S $ Len xs

type family xs ++ ys where
  Nil ++ ys        = ys
  (x ::: xs) ++ ys = x ::: xs ++ ys

type family C2 p1 p2 where
  C2 (x :*: y) (xs :*: ys) = (x ::: xs) :*: (y ::: ys)

type family Split xs where
  Split Nil = Nil :*: Nil
  Split (x ::: Nil) = (x ::: Nil) :*: Nil
  Split (x1 ::: x2 ::: xs) = C2 (x1 :*: x2) (Split xs)

type family Merge xs ys where
  Merge Nil ys = ys
  Merge xs Nil = xs
  Merge (x ::: xs) (y ::: ys) =
    If (x < y) (x ::: Merge xs (y ::: ys)) (y ::: Merge (x ::: xs) ys)

type family Fst p where
  Fst (a :*: b) = a

type family Snd p where
  Snd (a :*: b) = b

type family MergeSort xs where
  MergeSort Nil         = Nil
  MergeSort (x ::: Nil) = x ::: Nil
  MergeSort (x ::: xs)  =
    Merge (MergeSort (Fst (Split (x:::xs)))) (MergeSort (Snd (Split (x:::xs))))



data Tree a = Empty | Node (Tree a) a (Tree a)

type family Size t where
  Size Empty        = Z
  Size (Node l x r) = S $ Size l + Size r


data ToType :: k -> Type

a :: ToType $ (S $ S $ S Z) * (S $ S $ S Z)
a = undefined

type Xs = Split (Z:::Nil)
b :: ToType $ Merge (Fst Xs) (Snd Xs)
b = undefined

m :: ToType $ MergeSort (Z:::S (S Z):::S Z:::Z:::Nil)
m = undefined

