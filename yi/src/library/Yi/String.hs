{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.String
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- String manipulation utilities

module Yi.String (isBlank,
                  chomp,
                  capitalize,
                  capitalizeFirst,
                  dropSpace,
                  fillText,
                  onLines,
                  mapLines,
                  lines',
                  unlines',
                  padLeft, padRight,
                  commonTPrefix,
                  commonTPrefix',
                  listify,
                  showT,
                 ) where

import           Data.Char (toUpper, toLower, isSpace, isAlphaNum)
import           Data.List (isSuffixOf)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Control.Lens
import           Control.Applicative
import qualified Data.Text as T
import qualified Yi.Rope as R

instance Cons R.YiString R.YiString Char Char where
  _Cons = prism' (uncurry R.cons) (\r -> (,) <$> R.head r <*> R.tail r)
instance Snoc R.YiString R.YiString Char Char where
  _Snoc = prism' (uncurry R.snoc) (\r -> (,) <$> R.init r <*> R.last r)

-- | Helper that shows then packs the 'Text', for all those cases
-- where we use 'show'.
showT :: Show a => a -> T.Text
showT = T.pack . show

-- | This is kind of like the default Show instance for lists except
-- over 'T.Text'. It does not leave the elements in extra quotes and
-- should not be attempted to be 'show'n and 'read' back.
listify :: [R.YiString] -> R.YiString
listify t = '[' `R.cons` R.intercalate ", " t `R.snoc` ']'

-- | Works by resupplying the found prefix back into the list,
-- eventually either finding the prefix or not matching.
commonTPrefix :: [T.Text] -> Maybe T.Text
commonTPrefix (x:y:xs) = case T.commonPrefixes x y of
  Nothing -> Nothing
  Just (p, _, _) -> commonTPrefix (p : xs)
commonTPrefix [x] = Just x
commonTPrefix _ = Nothing

-- | Like 'commonTPrefix' but returns empty text on failure.
commonTPrefix' :: [T.Text] -> T.Text
commonTPrefix' = fromMaybe T.empty . commonTPrefix

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : map toLower cs

capitalizeFirst :: R.YiString -> R.YiString
capitalizeFirst = R.withText go
  where
    go x = case T.break isAlphaNum x of
      (f, b) -> f <> case T.splitAt 1 b of
        (h, hs) -> T.toUpper h <> hs

-- | Remove any trailing strings matching /irs/ (input record separator)
-- from input string. Like perl's chomp(1).
chomp :: String -> String -> String
chomp irs st
    | irs `isSuffixOf` st
    = let st' = reverse $ drop (length irs) (reverse st) in chomp irs st'
    | otherwise = st
{-# INLINE chomp #-}


-- | Trim spaces at beginning /and/ end
dropSpace :: String -> String
dropSpace = let f = reverse . dropWhile isSpace in f . f

isBlank :: R.YiString -> Bool
isBlank = R.all isSpace

-- | Fills lines up to the given length, splitting the text up if
-- necessary.
fillText :: Int -> R.YiString -> [R.YiString]
fillText margin = map (R.unwords . reverse) . fill 0 [] . R.words
  where
    fill _ acc [] = [acc]
    fill n acc (w:ws)
      | n + R.length w >= margin = acc : fill (R.length w) [w] ws
      | otherwise = fill (n + 1 + R.length w) (w:acc) ws

-- | Inverse of 'lines''. In contrast to 'Prelude.unlines', this does
-- not add an empty line at the end.
unlines' :: [T.Text] -> T.Text
unlines' = T.intercalate "\n"

-- | Split a Text in lines. Unlike 'Prelude.lines', this does not
-- remove any empty line at the end.
lines' :: T.Text -> [T.Text]
lines' = T.splitOn "\n"

-- | A helper function for creating functions suitable for
-- 'modifySelectionB' and 'modifyRegionB'.
-- To be used when the desired function should map across
-- the lines of a region.
mapLines :: (R.YiString -> R.YiString) -> R.YiString -> R.YiString
mapLines f = onLines $ fmap f

onLines :: ([R.YiString] -> [R.YiString]) -> R.YiString -> R.YiString
onLines f = mconcat . f . R.lines'

padLeft, padRight :: Int -> String -> String
padLeft n [] = replicate n ' '
padLeft n (x:xs) = x : padLeft (n-1) xs

padRight n = reverse . padLeft n . reverse
