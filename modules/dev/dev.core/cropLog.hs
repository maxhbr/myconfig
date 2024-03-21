#!/usr/bin/env nix-shell
#!nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import           System.Environment
import           System.Exit
import           Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

{-
 - TODOs:
 - * TODO: add line numbers from original file
 - * TODO: support regual expressions or wildcards
 - * TODO: support STDIN
 -}

usage :: IO ()
usage = do
  putStrLn "usage: "
  putStrLn "  $ cropLog.sh logFile id"

parse :: [String] -> IO [String]
parse [file,needle] = return [file,needle]
parse ["-h"]        = usage >> exitSuccess
parse _             = usage >> exitWith (ExitFailure 1)

crop :: String -> Text -> Text
crop needle = let
    guard = Text.isInfixOf (Text.pack needle)
    crop' :: [Text] -> [Text] -> [Text]
    crop' _ []                           = []
    crop' uncommitted (l:ls) | guard l   = uncommitted ++ [l] ++ crop' [] ls
                             | otherwise = crop' (uncommitted ++ [l]) ls
  in Text.unlines . crop' [] . dropWhile (not . guard) . Text.lines

main :: IO ()
main = do
  [file, needle] <- getArgs >>= parse
  crop needle <$> Text.readFile file >>= Text.putStrLn
