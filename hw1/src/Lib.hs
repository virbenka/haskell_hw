{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( 
      hello,
      distributivity,
      associator,
      pairProd,
      weirdFunction,
      distr,
      eitherAssoc,
      eitherAssoc1,
      iterateElement,
      fibonacci,
      factorial,
      mapFix,
      Nat,
      zero,
      churchPlus,
      churchMult,
      churchToInt,
      succChurch,
      smartReplicate,
      contains,
      stringSum,
      mergeSort
    ) where


import Data.Function (fix)

hello :: IO ()
hello = putStrLn "HW BEGINS"

--Task 1

{-
1. ((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s)) -> λ q'. ((q' (((q ((λ p. p) r)) s)r)) s)) -> 
λ q'. ((q' (((q r) s)r)) s)  -> λ q'. q' (q r s r) s

2. ((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b] -> 
((λ a. λ b. b a (a b x) (λ b. x)) (λ b. b))[x := b] -> ((λ a. λ b. b a (a b b) (λ b. b)) (λ b. b))-> λb.b (λb.b) ((λb.b) b b) (λb.b) -> λb.b (λb.b) (b b) (λb.b)
-}
-- Task 2


distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

pairProd
  :: (a -> b)
  -> (c -> d)
  -> (a,c)
  -> (b,d)
pairProd f g (a, c) = ((f a), (g c))

weirdFunction
  :: (d -> d -> b)
  -> (a -> b -> c)
  -> (d -> b)
  -> d -> b
weirdFunction f g e d = e d 

distr
  :: (a -> b -> c)
  -> (a -> b)
  -> a -> c
distr f g h = f h (g h)

type (<->) a b = (a -> b, b -> a)

eitherAssoc1
  :: Either a (Either b c)
  <-> Either (Either a b) c
eitherAssoc1 = (eitherAssocLeft, eitherAssocRight)
  where
       eitherAssocRight (Left (Left   x)) = Left         x
       eitherAssocRight (Left (Right  x)) = Right (Left  x)
       eitherAssocRight (Right        x)  = Right (Right x)
       eitherAssocLeft  (Left         x)  = Left  (Left  x)
       eitherAssocLeft  (Right (Left  y)) = Left  (Right y)
       eitherAssocLeft  (Right (Right z)) = Right        z

eitherAssoc
  :: Either a (Either b c)
  -> Either (Either a b) c
eitherAssoc (Left         x ) = Left  (Left  x)
eitherAssoc (Right (Left  y)) = Left  (Right y)
eitherAssoc (Right (Right z)) = Right        z

-- В этом дз оказалось несколько модулей Nat, поэтому я вынесла задачу 3 в отдельный файл (мб было решение и попроще, но я не стала его искать, надеюсь это ок)

-- Task 4


iterateElement :: a -> [a]
iterateElement a = fix (a :)

fibonacci :: Integer -> Integer
fibonacci = fix (\rec n -> if n <= 1 then 1 else (rec (n - 2)) + (rec (n - 1)))


factorial :: Integer -> Integer
factorial = fix (\rec n -> if n <= 1 then 1 else n * (rec (n - 1)))

mapFix :: (a -> b) -> [a] -> [b]
mapFix f = fix helper
      where
        helper _ [] = []
        helper g (x:xs) =  f x : g xs

-- Task 5

{- — distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain")) = (Left ("harold" ++ " hide " ++ "the " ++ "pain") , Left ("harold" ++ " hide " ++ "the " ++ "pain"))
— \"harold" ++ " hide " ++ "the " ++ "pain" -> Left ("harold" ++ " hide " ++ "the " ++ "pain") , Left ("harold" ++ " hide " ++ "the " ++ "pain") 
— => СГНФ = (Left ("harold" ++ " hide " ++ "the " ++ "pain") , Left ("harold" ++ " hide " ++ "the " ++ "pain"))

— null $ mapMaybe foo "pole chudes ochen' chudesno" => null (mapMaybe foo "ole chudes ochen' chudesno") => null (exp pi : mapMaybe foo "le chudes ochen' chudesno") => False
=> СГНФ=False
-}

-- Task 6

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch n f x = n f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f = n (m f)

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0

-- Task 7

{- 
1) ++ : [a] -> [a] -> [a]
2) id : a -> a
3) uncurry : (a -> b -> c) -> (a, b) -> c
4) map : (a -> b) -> [a] -> [b]
5) $ : (a -> b) -> a -> b
6) head : [a] -> a
7) isUpper : Char -> Bool
8) Anna : [Char]
9) Maks : [Char]
10) [((++) "Anna ", "Maks")] : [([Char] -> [Char], [Char])]
11) uncurry id : (b -> c, b) -> c
12) map (uncurry id) : [(b -> c, b)] -> [c]
13) map (uncurry id) [((++) "Anna ", "Maks")] : [[Char]]
14) head $ map (uncurry id) [((++) "Anna ", "Maks")] : [Char]
15) isUpper . head . head $ map (uncurry id) [((++) "Anna ", "Maks")] : Bool

1) 2^6 : Integer
2) 1+2 : Integer
3) Right(2^6) : Either a Integer
4) Left (1 + 2) : Either Integer b
5) rights : [Either a b] -> [b]
6) lefts : [Either a b] -> [a]
7) zip : [a] -> [b] -> [(a,b)]
8) [Left (1 + 2), Right (2 ^ 6)] : [Either Integer Integer]
9) zip (lefts x) (rights x) : [(b, a)]
10) (\x -> zip (lefts x) (rights x)) : [Either b a] -> [(b, a)]
11) (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)] : [(Integer, Integer)]

1) x : Bool
2) y : Bool
3) not : Bool -> Bool
4) || : Bool -> Bool -> Bool
5) not x : Bool
6) not x || y : Bool
7) let impl = \x y -> not x || y in : Bool -> Bool -> Bool
8) 0 : Integer
9) 2 : Integer
10) == : Integer -> Integer -> Bool
11) x : Integer
12) mod x : Integer -> Integer
13) x mod 2 : Integer
14) x mod 2 == 0 : Bool
15) let isMod2 = \x -> x mod 2 == 0 in : Integer -> Bool
16) 0 : Integer
17) 4 : Integer
18) == : Integer -> Integer -> Bool
19) x : Integer
20) mod x : Integer -> Integer
21) x mod 4 : Integer
22) x mod 4 == 0 : Bool
23) let isMod4 = \x -> x mod 2 == 0 in : Integer -> Bool
24) x : Integer
25) isMod2 : Integer -> Bool
26) isMod4 : Integer -> Bool
27) (isMod2 x) : Bool
28) (isMod4 x) : Bool
29) impl : Bool -> Bool -> Bool
30) impl (isMod4 x) (isMod2 x) : Bool
31) \x -> impl (isMod4 x) (isMod2 x) : Integer -> Bool

-}

-- Task 9

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)

stringSum :: String -> Int
stringSum = sum . map read . words

mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [a]  = [a]
mergeSort list = merge' (mergeSort l) (mergeSort r)
  where
    (l, r) = splitAt (div (length list) 2) list
    merge' :: Ord a => [a] -> [a] -> [a]
    merge' [] xs = xs
    merge' xs [] = xs
    merge' (x:xs) (y:ys)
      | x <= y    = x:merge' xs (y : ys)
      | otherwise = y:merge' (x : xs) ys
