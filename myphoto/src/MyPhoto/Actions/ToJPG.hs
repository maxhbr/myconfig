module MyPhoto.Actions.ToJPG
    ( toJPG
    ) where

import           System.FilePath
import           System.Directory
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (catMaybes)
import           Data.List (find, isPrefixOf)
import           System.Process
import           System.Exit
import           GHC.IO.Handle (hGetContents)

import MyPhoto.Model
import MyPhoto.Utils

calculateUntiffedName :: Img -> Img
calculateUntiffedName = (`replaceExtension` "jpg")

toJPGImpl1 :: Bool -> Img -> IO (Maybe Img)
toJPGImpl1 removeOld img = let
    jpg = calculateUntiffedName img
  in if jpg == img
     then return (Just jpg)
     else do
       putStrLn (img ++ " --> " ++ jpg)
       (_, _, _, pHandle) <- createProcess (proc "convert" [img, jpg])
       exitCode <- waitForProcess pHandle
       if exitCode == ExitSuccess
       then do
         when removeOld $
           removeFile img
         return (Just jpg)
       else do
         putStrLn ("toJPG failed with " ++ show exitCode)
         return Nothing

toJPGImpl :: Bool -> [Img] -> PActionBody
toJPGImpl removeOLD imgs = do
  jpgs <- mapM (toJPGImpl1 removeOLD) imgs
  return (Right (catMaybes jpgs))

toJPG :: PrePAction
toJPG []       = logSeparator "Run ToJPG" <> PAction (toJPGImpl False)
toJPG ["--rm"] = logSeparator "Run ToJPG (with --rm)" <> PAction (toJPGImpl True)
toJPG _        = PAction $ \_ -> pure (Left "Usage: tojpg [--rm] files...")
