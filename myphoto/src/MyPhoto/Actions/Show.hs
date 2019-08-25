module MyPhoto.Actions.Show
    ( showImgs
    ) where

import           System.Process

import MyPhoto.Model
import MyPhoto.Utils

showImgsImpl :: [Img] -> PActionBody
showImgsImpl imgs = do
  createProcess (proc "sxiv" imgs)
  return (Right imgs)

help :: String
help = "show IMG [IMG ...]"

showImgs :: PrePAction
showImgs ["-h"] = PAction (\_ -> pure (Left help))
showImgs []     = logSeparator "Run Show" <> PAction showImgsImpl
showImgs args   = PAction (\_ -> fail ("show does not expect any arguments, got: " ++ show args))
