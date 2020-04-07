#!/usr/bin/env runhaskell
-- Copyright (C) 2018  Daniel Gröber <dxld@darkboxed.org>
--
-- Copying and distribution of this file, with or without modification,
-- are permitted in any medium without royalty provided the copyright
-- notice and this notice are preserved.  This file is offered as-is,
-- without any warranty.

-- stolen from: https://gist.github.com/DanielG/a325bf8ff099e21a7ae765eeb57cbd8b

{-|
License: GNU All-Permissive License

This program parses and merges Z-Shell (zsh) extended history files.

Example usages:

To merge @some/zsh_history_file@, @some/other/zsh_history_file@ and
@~/.zsh_history@ and write the merged history list to @~/.zsh_history@:

@
$ ./zsh-history-merge some/zsh_history_file some/other/zsh_history_file ~/.zsh_history
@

-}
module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.Environment
import System.IO

usage = do
  prog <- getProgName
  hPutStrLn stderr $ prog ++ " HISTORY_FILE [HISTORY_FILE..] MERGE_FILE"

main = do
  files <- getArgs
  case files of
    []  -> usage
    [_] -> usage
    _ -> do
      let merge_file:_ = reverse files

      hists <- mapM readFileLatin1 files
      writeFile merge_file
               $ joinLines
               $ removeDuplicates
               $ sortBy (compare `on` hlStart)
               $ concat
               $ map splitLines hists

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = map head . group

readFileLatin1 :: FilePath -> IO String
readFileLatin1 f = do
  hdl <- openFile f ReadMode
  hSetEncoding hdl latin1 -- this will result in bogus characters but it always
                          -- succeeds and we only need the lower ascii chars for
                          -- parsing so it should be fine

  -- read the file into memory to avoid truncating the merge file
  c <- evaluate =<< (force <$> hGetContents hdl)
  hClose hdl
  return c



splitLines :: String -> [HistLine]
splitLines (':':' ':ws) = let
    (start_time, ':':xs) = span isDigit ws
    (end_time, ';':ys)   = span isDigit xs
    (command, zs)        = spanLine ys
  in
    HistLine (read start_time) (read end_time) command : splitLines zs
splitLines [] = []
splitLines ws = error $ "unexpected line: " ++ take 100 ws

spanLine :: String -> (String, String)
spanLine xs = (ul, drop (length ul) xs)
  where
    ul = unlines (l++[ll])

    (l, ll:ys) = span isContLine $ lines xs

    isContLine l = lineEscape `isSuffixOf` l || oldLineEscape `isSuffixOf` l
    oldLineEscape = "\\"
    lineEscape = "\\\\"

joinLines :: [HistLine] -> String
joinLines ls = concat $ map renderLine ls

renderLine (HistLine s e c) =
    ": " ++ show s ++ ":" ++ show e ++ ";" ++ c

data HistLine =
    HistLine
    { hlStart :: Integer
    , hlEnd   :: Integer
    , hlCmd   :: String
    } deriving (Eq, Ord, Show)

-- NOTE: this program doesn't handle zsh's weird meta character unicode
-- encoding, see:
-- https://github.com/kawabata/emacs-zsh-history/blob/master/zsh-history.el but
-- we don't really need to just to merge the history files. Just keep that in
-- mind if you intend to use this for something else.
