module MyPhoto.Actions.UnRAW
  ( unRAW
  ) where

import           System.FilePath
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           Data.List (find, isPrefixOf)
import           System.Process
import           System.Exit
import           GHC.IO.Handle (hGetContents)

import MyPhoto.MyPhoto
import MyPhoto.Wrapper.Dcraw

data ColorSpace
  = SRGBColorSpace
  | LinearColorSpace
  | DefaultColorSpace

instance Show ColorSpace where
  show SRGBColorSpace    = "-6 -g 2.4 12.92"
  show LinearColorSpace  = "-w"
  show DefaultColorSpace = ""

data Options
  = Options
  { optVerbose      :: Bool
  , optDebug        :: Bool
  , whitebalanceImg :: Maybe FilePath
  , colorSpace      :: ColorSpace
  , optQuality      :: Int
  } deriving Show

defaultOptions
  = Options
  { optVerbose      = False
  , optDebug        = False
  , whitebalanceImg = Nothing
  , optQuality      = 3
  , colorSpace      = SRGBColorSpace
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
      "chatty output on stderr"
  , Option [] ["debug"]
      (NoArg (\ opts -> opts { optDebug = True }))
      "very chatty output on stderr"
  , Option ['w'] ["wb", "whitebalanceImg"]
      (ReqArg (\f opts -> opts { whitebalanceImg = Just f })
              "FILE")
      "graycard image used for whitebalance (otherwise whitebalance information in RAW is used)"
  , Option ['q'] []
      (OptArg ((\f opts -> opts { optQuality = f })
               . fromMaybe 3
               . (fmap (read :: String -> Int)))
              "QUALITY_LEVEL")
      "set quality level"
  , Option ['l'] ["linearColorspace"]
      (NoArg (\opts -> opts { colorSpace = LinearColorSpace }))
      "use linear colorspace (no argument implies sRGB)"
  , Option [] ["defaultColorspace"]
      (NoArg (\opts -> opts { colorSpace = DefaultColorSpace }))
      "use default colorspace (no argument implies sRGB)"
  ]

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = let
    header = "Usage: unraw [OPTION...] files..."
  in case getOpt Permute options argv of
       (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

calculateUnRAWedName :: FilePath -> FilePath
calculateUnRAWedName = (`replaceExtension` "tiff")

calculateWhitebalanceArgs :: Options -> IO (String)
calculateWhitebalanceArgs Options{whitebalanceImg = Nothing}    = pure "-W"
calculateWhitebalanceArgs Options{whitebalanceImg = Just wbImg} = do
  multipliers <- calculateWhitebalance wbImg
  return ("-r " ++ multipliers)

buildDcrawArgs :: Options -> IO String
buildDcrawArgs opts = let
    verbosityArgs = if optVerbose opts
      then "-v"
      else ""
    colorspaceArgs = show $ colorSpace opts
    qualityArgs = case optQuality opts of
      3 -> ""
      i -> "-q " ++ (show i)
  in do
    whitebalanceArgs <- calculateWhitebalanceArgs opts
    return $ unwords [verbosityArgs, whitebalanceArgs, colorspaceArgs, qualityArgs, "-T"]

parseArgs :: [String] -> IO Options
parseArgs args = do
  (opts, nonOpts) <- getMyOpts args
  when (optDebug opts) $ do
    putStrLn "opts:"
    print opts
    putStrLn "nonOpts:"
    print nonOpts
  return opts

unRAWimpl :: [String] -> [Img] -> PActionBody
unRAWimpl args imgs = do
  opts <- parseArgs args
  dcrawArgs <- buildDcrawArgs opts

  let cmd = "dcraw " ++ dcrawArgs ++ " " ++ unwords imgs
  when (optDebug opts) $ do
    print cmd

  (_, _, _, pHandle) <- createProcess (shell cmd)
  exitCode <- waitForProcess pHandle
  return (case exitCode of
            ExitSuccess -> Right (map calculateUnRAWedName imgs) -- TODO: get generated output from "Writing data to <OUTPUT> ..." lines
            _           -> Left ("UnRAW failed with " ++ show exitCode))

unRAW :: [String] -> PAction
unRAW args = logSeparator "Run UnRAW" <> PAction (unRAWimpl args)
