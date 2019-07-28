module MyPhoto.Actions.UnTiff
    ( unTiff
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

import MyPhoto.Model
import MyPhoto.Utils

calculateUntiffedName :: Img -> Img
calculateUntiffedName = (`replaceExtension` "png")

unTiffImpl1 :: Bool -> Img -> IO Img
unTiffImpl1 removeTiff img = let
    args = [ "-depth", "24"
           , "-define", "png:compression-filter=2"
           , "-define", "png:compression-level=9"
           , "-define", "png:compression-strategy=1"]
    png = calculateUntiffedName img
  in do
    putStrLn (img ++ " --> " ++ png)
    (_, _, _, pHandle) <- createProcess (proc "convert" (args ++ [img, png]))
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $
      fail ("UnTiff failed with " ++ show exitCode)
    when removeTiff $
      removeFile img
    return png

unTiffImpl :: Bool -> [Img] -> PActionBody
unTiffImpl removeTiff imgs = do
  pngs <- mapM (unTiffImpl1 removeTiff) imgs
  return (Right pngs)

unTiff :: PrePAction
unTiff [] = logSeparator "Run UnTiff" <> PAction (unTiffImpl False)
unTiff ["--rm"] = logSeparator "Run UnTiff (with --rm)" <> PAction (unTiffImpl True)
unTiff _ = PAction $ \_ -> pure (Left "Usage: untiff [--rm] files...")
