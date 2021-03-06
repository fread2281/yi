{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Misc
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Various high-level functions to further classify.

module Yi.Misc where

import           Control.Applicative
import           Control.Monad.Base
import           Data.Char (chr, isAlpha, isLower, isUpper, ord)
import           Data.List ((\\))
import           Data.Maybe (isNothing)
import qualified Data.Text as T
import           System.CanonicalizePath (canonicalizePath, replaceShorthands,
                                          replaceShorthands)
import           System.Directory (doesDirectoryExist, getDirectoryContents,
                                   getCurrentDirectory)
import           System.FilePath (takeDirectory, (</>), takeFileName,
                                  addTrailingPathSeparator,
                                  hasTrailingPathSeparator)
import           System.FriendlyPath (expandTilda, isAbsolute')
import           Yi.Completion (completeInList')
import           Yi.Core
import           Yi.MiniBuffer (withMinibufferGen, mkCompleteFn )
import           Yi.Monad
import qualified Yi.Rope as R

-- | Given a possible starting path (which if not given defaults to
-- the current directory) and a fragment of a path we find all files
-- within the given (or current) directory which can complete the
-- given path fragment. We return a pair of both directory plus the
-- filenames on their own that is without their directories. The
-- reason for this is that if we return all of the filenames then we
-- get a 'hint' which is way too long to be particularly useful.
getAppropriateFiles :: Maybe T.Text -> T.Text -> YiM (T.Text, [ T.Text ])
getAppropriateFiles start s' = do
  curDir <- case start of
    Nothing -> do bufferPath <- withBuffer $ gets file
                  liftBase $ getFolder bufferPath
    Just path -> return $ T.unpack path
  let s = T.unpack $ replaceShorthands s'
      sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir
        | null sDir = curDir
        | isAbsolute' sDir = sDir
        | otherwise = curDir </> sDir
  searchDir' <- liftBase $ expandTilda searchDir
  let fixTrailingPathSeparator f = do
        isDir <- doesDirectoryExist (searchDir' </> f)
        return . T.pack $ if isDir then addTrailingPathSeparator f else f

  files <- liftBase $ getDirectoryContents searchDir'

  -- Remove the two standard current-dir and parent-dir as we do not
  -- need to complete or hint about these as they are known by users.
  let files' = files \\ [ ".", ".." ]
  fs <- liftBase $ mapM fixTrailingPathSeparator files'
  let matching = filter (T.isPrefixOf . T.pack $ takeFileName s) fs
  return (T.pack sDir, matching)

-- | Given a path, trim the file name bit if it exists.  If no path
--   given, return current directory.
getFolder :: Maybe String -> IO String
getFolder Nothing     = getCurrentDirectory
getFolder (Just path) = do
  isDir <- doesDirectoryExist path
  let dir = if isDir then path else takeDirectory path
  if null dir then getCurrentDirectory else return dir


-- | Given a possible path and a prefix, return matching file names.
matchingFileNames :: Maybe T.Text -> T.Text -> YiM [T.Text]
matchingFileNames start s = do
  (sDir, files) <- getAppropriateFiles start s

  -- There is one common case when we don't need to prepend @sDir@ to @files@:
  --
  -- Suppose user just wants to edit a file "foobar" in current directory
  -- and inputs ":e foo<Tab>"
  --
  -- @sDir@ in this case equals to "." and "foo" would not be
  -- a prefix of ("." </> "foobar"), resulting in a failed completion
  --
  -- However, if user inputs ":e ./foo<Tab>", we need to prepend @sDir@ to @files@
  let results = if isNothing start && sDir == "." && not ("./" `T.isPrefixOf` s)
                   then files
                   else fmap (T.pack . (T.unpack sDir </>) . T.unpack) files

  return results

adjBlock :: Int -> BufferM ()
adjBlock x = withSyntaxB' (\m s -> modeAdjustBlock m s x)

-- | A simple wrapper to adjust the current indentation using
-- the mode specific indentation function but according to the
-- given indent behaviour.
adjIndent :: IndentBehaviour -> BufferM ()
adjIndent ib = withSyntaxB' (\m s -> modeIndent m s ib)



-- | Generic emacs style prompt file action. Takes a @prompt@ and a continuation
-- @act@ and prompts the user with file hints.
promptFile :: T.Text -> (T.Text -> YiM ()) -> YiM ()
promptFile prompt act = do
  maybePath <- withBuffer $ gets file
  startPath <- T.pack . addTrailingPathSeparator
               <$> liftBase (canonicalizePath =<< getFolder maybePath)
  -- TODO: Just call withMinibuffer
  withMinibufferGen startPath (findFileHint startPath) prompt
    (completeFile startPath) showCanon (act . replaceShorthands)
  where
    showCanon = withBuffer . replaceBufferContent . R.fromText . replaceShorthands

matchFile :: T.Text -> T.Text -> Maybe T.Text
matchFile path proposedCompletion =
  let realPath = replaceShorthands path
  in T.append path <$> T.stripPrefix realPath proposedCompletion

completeFile :: T.Text -> T.Text -> YiM T.Text
completeFile startPath =
  mkCompleteFn completeInList' matchFile $ matchingFileNames (Just startPath)

-- | For use as the hint when opening a file using the minibuffer. We
-- essentially return all the files in the given directory which have
-- the given prefix.
findFileHint :: T.Text -> T.Text -> YiM [T.Text]
findFileHint startPath s = snd <$> getAppropriateFiles (Just startPath) s

onCharLetterCode :: (Int -> Int) -> Char -> Char
onCharLetterCode f c | isAlpha c = chr (f (ord c - a) `mod` 26 + a)
                     | otherwise = c
                     where a | isUpper c = ord 'A'
                             | isLower c = ord 'a'
                             | otherwise = undefined

rot13Char :: Char -> Char
rot13Char = onCharLetterCode (+13)

printFileInfoE :: EditorM ()
printFileInfoE = printMsg . showBufInfo =<< withBuffer0 bufInfoB
    where showBufInfo :: BufferFileInfo -> T.Text
          showBufInfo bufInfo = T.concat
            [ T.pack $ bufInfoFileName bufInfo
            , " Line "
            , T.pack . show $ bufInfoLineNo bufInfo
            , " ["
            , bufInfoPercent bufInfo
            , "]"
            ]
