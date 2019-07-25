{-# LANGUAGE LambdaCase #-}
module MyPhoto.Wrapper.Dcraw
  ( getImageSize
  , calculateWhitebalance
  ) where

import           System.FilePath
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           Data.List (find, isPrefixOf)
import           System.Process
import           System.Exit
import           GHC.IO.Handle (hGetContents)

getImageSize :: FilePath -> IO (Int, Int)
getImageSize img = let
    imageSizePrefix = "Image size:  "
  in do
    putStrLn ("calculate image size from " ++ img)
    (_, Just hout, _, pHandle) <- createProcess (proc "dcraw" ["-v", "-i", img]){ std_out = CreatePipe }
    exitCode <- waitForProcess pHandle
    out <- hGetContents hout
    case exitCode of
      ExitSuccess ->  case find (imageSizePrefix `isPrefixOf`) (lines out) of
                        Just l -> let
                            str = drop (length imageSizePrefix) l
                            strItems = words str
                            x = read (strItems !! 0) :: Int
                            y = read (strItems !! 2) :: Int
                          in do
                            putStrLn ("Image size: " ++ show x ++ "x" ++ show y)
                            return (x,y)
                        Nothing -> fail ("Failed to get image size in : " ++ show out)
      _           -> fail ("Failed to get image size of " ++ img ++ " with " ++ show exitCode)

{-
    calculateWB() {
        local wbImg="$1"

        # Fraction of the image to use for white balance calculations (in both dimensions; centered)
        local frac=0.3

        read Nx Ny <<< $(LANG=en_US.UTF-8 dcraw -i -v "$wbImg" |
                            grep "^Image size" |
                            cut -d: -f2 |
                            sed 's/x//g')
        (>&2 echo "Image dimensions: $Nx x $Ny")

        # Center coordinates:
        local x=$(( $Nx / 2 ))
        local y=$(( $Ny / 2 ))
        # Crop dimensions:
        local w=$(echo $x $frac | awk '{print int($1*$2)}')
        local h=$(echo $y $frac | awk '{print int($1*$2)}')
        (>&2 echo "Crop area center and dimensions: x,y=$x,$y; w,h=$w x $h")

        wb=$(LANG=en_US.UTF-8 dcraw -v -A $x $y $w $h -c "$wbImg" 2>&1 >/dev/null |
                grep "^multipliers" |
                cut -d" " -f2-)
        (>&2 echo "Calculated whitebalance is: $wb")
        echo "$wb"
    }
-}
calculateWhitebalance :: FilePath -> IO String
calculateWhitebalance wbImg = let
    multipliersPrefix = "multipliers "
    getDcrawWBArgs :: (Int, Int) -> [String]
    getDcrawWBArgs (xSize,ySize) = let
        xMid = xSize `div` 2
        yMid = ySize `div` 2
        xHeight = xSize `div` 3
        yHeight = ySize `div` 3
      in ["-v", "-A", show xMid, show yMid, show xHeight, show yHeight, "-c"]
  in do
    size <- getImageSize wbImg
    putStrLn ("calculate WB from " ++ wbImg)
    let args = getDcrawWBArgs size ++ [wbImg]
    (_, _, Just herr, pHandle) <- createProcess (proc "dcraw" args){ std_out = NoStream, std_err = CreatePipe }
    exitCode <- waitForProcess pHandle
    err <- hGetContents herr
    case exitCode of
      ExitSuccess -> case find (multipliersPrefix `isPrefixOf`) (lines err) of
                       Just l  -> return (drop (length multipliersPrefix) l)
                       Nothing -> fail ("Failed to find multipliers in " ++ show err)
      _           -> fail ("Failed to calculate WB of " ++ wbImg ++ " with " ++ show exitCode)
