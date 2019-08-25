module MyPhoto.Actions.Skip
    ( skipPAct
    ) where

import           System.Directory

import MyPhoto.Model
import MyPhoto.Utils

help :: PAction
help = PAction $ \_ -> pure (Left (unlines [ "skip NUMBER" ]))

skipImpl :: Int -> [Img] -> PActionBody
skipImpl i imgs = return (Right (drop i imgs))


skipPAct :: PrePAction
skipPAct ["-h"]   = help
skipPAct [number] = logSeparator ("Skip " ++ number ) <> PAction (skipImpl (read number))
skipPAct _        = help
