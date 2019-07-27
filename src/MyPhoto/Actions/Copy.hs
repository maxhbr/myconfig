module MyPhoto.Actions.Copy
    ( copyPAct
    ) where

import           System.FilePath
import           System.Directory

import MyPhoto.Model
import MyPhoto.Utils

help :: PAction
help = PAction $ \_ -> pure (Left (unlines [ "copy TARGET_FOLDER" ]))

copyImpl :: FilePath -> [Img] -> PActionBody
copyImpl target imgs = do
  createDirectoryIfMissing True target

  imgs' <- mapM (\img -> let
                    img' = replaceDirectory img target
                  in do
                    copyFile img target
                    return img'
                ) imgs
  return (Right imgs')


copyPAct :: PrePAction
copyPAct ["-h"]   = help
copyPAct [target] = logSeparator ("Run copy (to " ++ target ++")") <> PAction (copyImpl target)
copyPAct _        = help
