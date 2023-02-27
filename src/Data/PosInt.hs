module Data.PosInt (PosInt, toPosInt, fromPosInt) where

newtype PosInt = PosInt { theInt :: Int } deriving (Show)

toPosInt :: Int -> Maybe PosInt
toPosInt x = if x <= 0 then Nothing else Just (PosInt x)

fromPosInt :: PosInt -> Int
fromPosInt = theInt