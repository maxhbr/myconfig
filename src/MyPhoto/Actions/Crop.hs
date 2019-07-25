module MyPhoto.Actions.Crop
    ( crop
    ) where


import           System.FilePath
import           System.Directory
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           Data.List (find, isPrefixOf)
import           System.Process
import           System.Exit
import           GHC.IO.Handle (hGetContents)

import MyPhoto.MyPhoto

calculateCroppedName :: String -> Img -> Img
calculateCroppedName addition img = let
    (bn,ext) = splitExtensions img
  in bn ++ "_" ++ addition ++ ext

cropImpl1 :: String -> (String,String,String,String) -> Img -> IO Img
cropImpl1 addition (t,r,b,l) img = let
    args = ["-crop", "+" ++ l ++ "+" ++ t, "-crop", "-" ++ r ++ "-" ++ b]
    out = calculateCroppedName addition img
  in do
    (_, _, _, pHandle) <- createProcess (proc "convert" (img : args ++ [out]))
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $
      fail ("Crop failed with " ++ show exitCode)
    return out

cropImpl :: String -> (String,String,String,String) -> [Img] -> PActionBody
cropImpl addition crp imgs = do
  pngs <- mapM (cropImpl1 addition crp) imgs
  return (Right pngs)

help :: PAction
help = PAction $ \_ -> pure (Left (unlines [ "Usage:"
                                             , "\tcrop <top-right-bottom-left>       img [img [img ...]]"
                                             , "\tcrop <top-bottom> <right-left>     img [img [img ...]]"
                                             , "\tcrop <top> <right> <bottom> <left> img [img [img ...]]"]))
crop :: PrePAction
crop ["-h"]       = help
crop [trbl]       = logSeparator "Run Crop" <> PAction (cropImpl ("c" ++ trbl) (trbl, trbl, trbl, trbl))
crop [tb, rl]     = logSeparator "Run Crop" <> PAction (cropImpl ("c" ++ tb ++ "-" ++ rl) (tb, rl, tb, rl))
crop [t, r, b, l] = logSeparator "Run Crop" <> PAction (cropImpl ("c" ++ t ++ "-" ++ r ++ "-" ++ b ++ "-" ++ l) (t, r, b, l))
crop _            = help

