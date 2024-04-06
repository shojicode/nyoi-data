module Main (main) where

import Control.Monad (liftM3)
import Data.Maybe (fromJust)

source :: Term
source = TmIf TmFalse (TmIsZero (TmPred (TmSucc TmZero))) TmTrue

main :: IO ()
main = print $ fromJust $ eval source

data Term =
    TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    deriving Show

isNumeical :: Term -> Bool
isNumeical TmZero = True
isNumeical (TmSucc tm) = isNumeical tm
isNumeical _ = False

isVal :: Term -> Bool
isVal t = case t of
    TmTrue -> True
    TmFalse -> True
    _ | isNumeical t -> True
      | otherwise -> False

eval1 :: Term -> Maybe Term
eval1 t = case t of
    TmIf TmTrue t2 _ -> Just t2
    TmIf TmFalse _ t3 -> Just t3
    TmIf t1 t2 t3 -> liftM3 TmIf (eval1 t1) (Just t2) (Just t3)
    TmSucc t1 -> fmap TmSucc (eval1 t1)
    TmPred TmZero -> Just TmZero
    TmPred (TmSucc nv1) | isNumeical nv1 -> Just nv1
    TmPred t1 -> fmap TmPred (eval1 t1)
    TmIsZero TmZero -> Just TmTrue
    TmIsZero (TmSucc nv1) | isNumeical nv1 -> Just TmFalse
    TmIsZero t1 -> fmap TmIsZero (eval1 t1)
    _ -> Nothing

eval :: Term -> Maybe Term
eval t
    | isVal t = Just t
    | otherwise = do
        t' <- eval1 t
        eval t' 