module Task3 where

data Nat = Zero | Succ Nat

instance Eq (Nat) where
  Zero == Zero = True
  Zero == Succ x = False
  Succ x == Zero = False
  Succ x == Succ y = x == y

instance Show Nat where
  show Zero = "Zero"
  show (Succ x) = "Succ (" ++ show x ++ ")"

instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero (Succ x) = LT
  compare (Succ x) Zero = GT
  compare (Succ x) (Succ y) = compare x y

instance Num Nat where
  Zero + x = x
  x + Zero = x
  (Succ x) + (Succ y) = Succ (Succ (x + y))

  Zero - (Succ x) = error "Natural numbers are nonnegative"
  x - Zero = x
  (Succ y) - (Succ x) = y - x

  Zero * _ = Zero
  _ * Zero = Zero
  (Succ x) * y = y + (x * y)

  abs x = x

  signum Zero = 0
  signum _ = 1

  fromInteger 0 = Zero
  fromInteger x | x > 0     = Succ (fromInteger (pred x))
                | otherwise = error "Natural numbers are nonnegative"

instance Enum Nat where
  toEnum 0 = Zero
  toEnum x | x > 0     = Succ (toEnum (pred x))
           | otherwise = error "Natural numbers are nonnegative"
  fromEnum Zero = 0
  fromEnum (Succ x) = 1 + (fromEnum x)
