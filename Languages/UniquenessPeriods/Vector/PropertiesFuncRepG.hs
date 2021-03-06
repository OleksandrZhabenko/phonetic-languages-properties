-- |
-- Module      :  Languages.UniquenessPeriods.Vector.PropertiesFuncRepG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization of the functionality of the DobutokO.Poetry.Norms
-- and DobutokO.Poetry.Norms.Extended modules
-- from the @dobutokO-poetry@ package.

{-# LANGUAGE CPP, BangPatterns #-}

module Languages.UniquenessPeriods.Vector.PropertiesFuncRepG (
  -- * Functions with 'Int16'
  procDiverse2I
  , procDiverse2Ineg
  -- * Functions with 'Float'
  , procDiverse2F
  , procDiverse2Fneg
  , procRhythmicity23F
  , procRhythmicity23Fneg
  , procBothF
  , procBothFneg
  , procBothInvF
  , procBothInvFneg
  -- ** Working with generated by r-glpk-phonetic-languages-ukrainian-durations syllable durations
  , procRhythmicity232F
  , procRhythmicity232Fneg
  , procBoth2F
  , procBoth2Fneg
  , procBoth2InvF
  , procBoth2InvFneg
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import GHC.Int
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import String.Languages.UniquenessPeriods.VectorG
import Languages.UniquenessPeriods.Vector.PropertiesSyllablesG
import Languages.UniquenessPeriods.Vector.PropertiesG
import Languages.Rhythmicity
import Languages.UniquenessPeriods.Vector.DataG
import GHC.Float (int2Float)
import Melodics.ByteString.Ukrainian
import Languages.Phonetic.Ukrainian.Syllable
import Data.Maybe (isNothing,fromMaybe)
import Text.Read (readMaybe)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

procDiverse2I :: FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Int16]
procDiverse2I = D2 (uniquenessPeriodsVector3 " 01-" . convertToProperUkrainianV2X) ((:[]) . diverse2)
{-# INLINE procDiverse2I #-}

-- | Can be used to find out the minimum element.
procDiverse2Ineg :: FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Int16]
procDiverse2Ineg = D2 (uniquenessPeriodsVector3 " 01-" . convertToProperUkrainianV2X) ((:[]) . negate . diverse2)
{-# INLINE procDiverse2Ineg #-}

procDiverse2F :: FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procDiverse2F = D2 (uniquenessPeriodsVector3 " 01-" . convertToProperUkrainianV2X) ((:[]) . int2Float . fromEnum . diverse2)
{-# INLINE procDiverse2F #-}

procDiverse2Fneg :: FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procDiverse2Fneg = D2 (uniquenessPeriodsVector3 " 01-" . convertToProperUkrainianV2X) ((:[]) . int2Float . negate . fromEnum . diverse2)
{-# INLINE procDiverse2Fneg #-}

--------------------------------------------------------------------------------------------

procRhythmicity23F :: String -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procRhythmicity23F choice coeffs = procRhythm23F choice rhythmicityV coeffs
{-# INLINE procRhythmicity23F #-}

-- | Can be used to find out the minimum element.
procRhythmicity23Fneg :: String -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procRhythmicity23Fneg choice coeffs  = procRhythm23Fneg choice rhythmicityV coeffs
{-# INLINE procRhythmicity23Fneg #-}

procBothF :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBothF coeffs  = procB2F syllableDurations coeffs
{-# INLINE procBothF #-}

-- | Can be used to find out the minimum element.
procBothFneg :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBothFneg coeffs  = procB2Fneg syllableDurations coeffs
{-# INLINE procBothFneg #-}

procBothInvF :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBothInvF coeffs  = procB2InvF syllableDurations coeffs
{-# INLINE procBothInvF #-}

-- | Can be used to find out the minimum element.
procBothInvFneg :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBothInvFneg coeffs  = procB2InvFneg syllableDurations coeffs
{-# INLINE procBothInvFneg #-}

-------------------------------------------------------------------------------

procRhythmicity232F :: String -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procRhythmicity232F choice coeffs  = procRhythm23F choice rhythmicityV coeffs
{-# INLINE procRhythmicity232F #-}

-- | Can be used to find out the minimum element.
procRhythmicity232Fneg :: String -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procRhythmicity232Fneg choice coeffs  = procRhythm23Fneg choice rhythmicityV coeffs
{-# INLINE procRhythmicity232Fneg #-}

procBoth2F :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBoth2F coeffs  = procB2F syllableDurations2 coeffs
{-# INLINE procBoth2F #-}

-- | Can be used to find out the minimum element.
procBoth2Fneg :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBoth2Fneg coeffs  = procB2Fneg syllableDurations2 coeffs
{-# INLINE procBoth2Fneg #-}

procBoth2InvF :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBoth2InvF coeffs  = procB2InvF syllableDurations2 coeffs
{-# INLINE procBoth2InvF #-}

-- | Can be used to find out the minimum element.
procBoth2InvFneg :: Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procBoth2InvFneg coeffs  = procB2InvFneg syllableDurations2 coeffs
{-# INLINE procBoth2InvFneg #-}

-------------------------------------------------------------

eval23Coeffs :: Coeffs2 -> [Float] -> Float
eval23Coeffs (CF2 x y) = evalRhythmicity23K (fromMaybe 1.0 x) (fromMaybe 1.0 y)
eval23Coeffs CF0 = evalRhythmicity23

procRhythm23F :: String -> (String -> Coeffs2 -> VB.Vector Char -> Float) -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procRhythm23F choice g coeffs = U1 ((:[]) . g choice coeffs)
{-# INLINE procRhythm23F #-}

procRhythm23Fneg :: String -> (String -> Coeffs2 -> VB.Vector Char -> Float) -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procRhythm23Fneg choice g coeffs = U1 ((:[]) . negate . g choice coeffs)
{-# INLINE procRhythm23Fneg #-}

procB2F :: ([[[UZPP2]]] -> [[Float]]) -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procB2F g coeffs = U1 (\v -> let ys = convertToProperUkrainianV2S . VB.map (\x -> if x == '-' then ' ' else x) $ v in (:[]) ((int2Float . fromEnum . diverse2 . uniquenessPeriodsVector3 " 01-" . VB.fromList $ ys)*(eval23Coeffs coeffs . mconcat . g . map (divVwls . reSyllableCntnts . groupSnds . vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys)))
{-# INLINE procB2F #-}

-- | Can be used to find out the minimum element.
procB2Fneg :: ([[[UZPP2]]] -> [[Float]]) -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procB2Fneg g coeffs = U1 (\v -> let ys = convertToProperUkrainianV2S . VB.map (\x -> if x == '-' then ' ' else x) $ v in (:[]) ((int2Float . negate . fromEnum . diverse2 . uniquenessPeriodsVector3 " 01-" .
    VB.fromList$ ys)*(eval23Coeffs coeffs . mconcat . g . map (divVwls . reSyllableCntnts . groupSnds . vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys)))
{-# INLINE procB2Fneg #-}

procB2InvF :: ([[[UZPP2]]] -> [[Float]]) -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procB2InvF g coeffs = U1 (\v ->
  let !ys = convertToProperUkrainianV2S . VB.map (\x -> if x == '-' then ' ' else x) $ v
      !zs = uniquenessPeriodsVector3 " 01-" . VB.fromList $ ys in if VB.null zs then (:[]) ((evalRhythmicity23 . mconcat . g . map (divVwls .
     reSyllableCntnts . groupSnds . vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys) * (eval23Coeffs coeffs . mconcat . g . map (divVwls . reSyllableCntnts .
       groupSnds . vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys)) else (:[]) ((eval23Coeffs coeffs . mconcat . g . map (divVwls . reSyllableCntnts . groupSnds .
         vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys) / (int2Float . fromEnum . diverse2 $ zs)))
{-# INLINE procB2InvF #-}

-- | Can be used to find out the minimum element.
procB2InvFneg :: ([[[UZPP2]]] -> [[Float]]) -> Coeffs2 -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
procB2InvFneg g coeffs = U1 (\v ->
   let !ys = convertToProperUkrainianV2S . VB.map (\x -> if x == '-' then ' ' else x) $ v
       !zs = uniquenessPeriodsVector3 " 01-" .VB.fromList $ ys in if VB.null zs then (:[]) (negate (eval23Coeffs coeffs . mconcat . g . map (divVwls .
     reSyllableCntnts . groupSnds . vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys) * (eval23Coeffs coeffs . mconcat . g . map (divVwls . reSyllableCntnts .
       groupSnds . vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys)) else (:[]) ((eval23Coeffs coeffs . mconcat . g . map (divVwls . reSyllableCntnts . groupSnds .
         vec2UZPP2s) . vecWords . V.filter (/='0') . V.fromList $ ys) / (int2Float . negate .  fromEnum . diverse2 $ zs)))
{-# INLINE procB2InvFneg #-}

