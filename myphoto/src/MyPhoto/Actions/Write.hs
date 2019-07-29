module MyPhoto.Actions.Write
    ( writePAction
    ) where

import MyPhoto.Model
import MyPhoto.Utils

writePAction :: PrePAction
writePAction [txtFile] = undefined
writePAction _         = undefined
