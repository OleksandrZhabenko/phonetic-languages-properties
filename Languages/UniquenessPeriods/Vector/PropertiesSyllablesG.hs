-- |
-- Module      :  Languages.UniquenessPeriods.Vector.PropertiesSyllablesG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization and extension of the functionality of the DobutokO.Poetry.Norms
-- and DobutokO.Poetry.Norms.Extended modules
-- from the @dobutokO-poetry@ package. Uses syllables information.

{-# LANGUAGE CPP, BangPatterns #-}

module Languages.UniquenessPeriods.Vector.PropertiesSyllablesG (
    -- * Newtype to work with
  CoeffTwo(..)
  , Coeffs2
  , isEmpty
  , isPair
  , fstCF
  , sndCF
  , readCF
  -- * Rhythmicity metrices (semi-empirical)
  -- ** Simple ones
  , rhythmicity0
  , rhythmicityV0
  -- ** With weight coefficients
  , rhythmicityVK
  , rhythmicityK
  -- * Rhythmicity metrices from generated with r-glpk-phonetic-languages-ukrainian-durations package (since 0.2.0.0 version)
  -- ** Simple ones
  , rhythmicity02
  , rhythmicityV02
  -- ** With weight coefficients
  , rhythmicityVK2
  , rhythmicityK2
  -- * General
  , rhythmicity
  , rhythmicityV
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import qualified Data.Vector as VB
import Languages.Rhythmicity
import Languages.Phonetic.Ukrainian.Syllable
import Data.Maybe (isNothing,fromMaybe)
import Text.Read (readMaybe)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data CoeffTwo a = CF0 | CF2 (Maybe a) (Maybe a) deriving (Eq)

isEmpty :: CoeffTwo a -> Bool
isEmpty CF0 = True
isEmpty _ = False

isPair :: CoeffTwo a -> Bool
isPair CF0 = False
isPair _ = True

fstCF :: CoeffTwo a -> Maybe a
fstCF (CF2 x _) = x
fstCF _ = Nothing

sndCF :: CoeffTwo a -> Maybe a
sndCF (CF2 _ y) = y
sndCF _ = Nothing

readCF :: String -> Coeffs2
readCF xs
  | any (== '_') xs = let (!ys,!zs) = (\(ks,ts) -> (readMaybe ks::Maybe Float,readMaybe (drop 1 ts)::Maybe Float)) . break (== '_') $ xs in
     if (isNothing ys && isNothing zs) then CF0 else CF2 ys zs
  | otherwise = CF0

type Coeffs2 = CoeffTwo Float

--------------------------------------------------------------------------------------------

eval23 = evalRhythmicity23 . mconcat
{-# INLINE eval23 #-}

eval23K k2 k3 = evalRhythmicity23K k2 k3 . mconcat
{-# INLINE eval23K #-}

rhythmicity0 :: String -> Float
rhythmicity0 xs
 | null xs = 0.0
 | otherwise = eval23 . syllableDurations . createSyllablesUkr $ xs

rhythmicityK :: Float -> Float -> String -> Float
rhythmicityK k2 k3 xs
 | null xs = 0.0
 | otherwise = eval23K k2 k3 . syllableDurations . createSyllablesUkrP $ xs

rhythmicityV0 :: VB.Vector Char -> Float
rhythmicityV0 v
 | VB.null v = 0.0
 | otherwise = eval23 . syllableDurations . createSyllablesUkrV $ v

rhythmicityVK :: Float -> Float -> VB.Vector Char -> Float
rhythmicityVK k2 k3 v
 | VB.null v = 0.0
 | otherwise = eval23K k2 k3 . syllableDurations . createSyllablesUkrVP $ v

-------------------------------------------------------

rhythmicity02 :: String -> Float
rhythmicity02 xs
 | null xs = 0.0
 | otherwise = eval23 . syllableDurations2 . createSyllablesUkr $ xs

rhythmicityK2 :: Float -> Float -> String -> Float
rhythmicityK2 k2 k3 xs
 | null xs = 0.0
 | otherwise = eval23K k2 k3 . syllableDurations2 . createSyllablesUkrP $ xs

rhythmicityV02 :: VB.Vector Char -> Float
rhythmicityV02 v
 | VB.null v = 0.0
 | otherwise = eval23 . syllableDurations2 . createSyllablesUkrV $ v

rhythmicityVK2 :: Float -> Float -> VB.Vector Char -> Float
rhythmicityVK2 k2 k3 v
 | VB.null v = 0.0
 | otherwise = eval23K k2 k3 . syllableDurations2 . createSyllablesUkrVP $ v

------------------------------------------------------------------

rhythmicity :: String -> Coeffs2 -> String -> Float
rhythmicity choice CF0
  | choice == "0y" = rhythmicity0
  | otherwise = rhythmicity02
rhythmicity choice (CF2 x y)
  | choice == "0y" = rhythmicityK (fromMaybe 1.0 x) (fromMaybe 1.0 y)
  | otherwise = rhythmicityK2 (fromMaybe 1.0 x) (fromMaybe 1.0 y)

rhythmicityV :: String -> Coeffs2 -> VB.Vector Char -> Float
rhythmicityV choice CF0
  | choice == "0y" = rhythmicityV0
  | otherwise = rhythmicityV02
rhythmicityV choice (CF2 x y)
  | choice == "0y" = rhythmicityVK (fromMaybe 1.0 x) (fromMaybe 1.0 y)
  | otherwise = rhythmicityVK2 (fromMaybe 1.0 x) (fromMaybe 1.0 y)
