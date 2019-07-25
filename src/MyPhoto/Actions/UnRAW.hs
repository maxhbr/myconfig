module MyPhoto.Actions.UnRAW
  ( unRAW
  ) where

import           System.FilePath
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           System.Process
import           System.Exit

import MyPhoto.MyPhoto
import MyPhoto.Wrapper.Dcraw

data ColorSpace
  = SRGBColorSpace
  | LinearColorSpace
  | DefaultColorSpace
  deriving Show
data WhiteBalanceOption
  = WBFromImage Img
  | WBFromFirstImage
  | WBFromRaw
  deriving Show

colorSpaceToArgs :: ColorSpace -> [String]
colorSpaceToArgs SRGBColorSpace    = ["-6", "-g", "2.4", "12.92"]
colorSpaceToArgs LinearColorSpace  = ["-w"]
colorSpaceToArgs DefaultColorSpace = []

data Options
  = Options
  { optVerbose      :: Bool
  , optHelp         :: Bool
  , optDebug        :: Bool
  , optWhitebalance :: WhiteBalanceOption
  , optColorSpace   :: ColorSpace
  , optQuality      :: Int
  } deriving Show

defaultOptions :: Options
defaultOptions
  = Options
  { optVerbose      = False
  , optHelp         = False
  , optDebug        = False
  , optWhitebalance = WBFromRaw
  , optColorSpace   = SRGBColorSpace
  , optQuality      = 3
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
      "chatty output on stderr"
  , Option ['h'] ["help"]
      (NoArg (\ opts -> opts { optHelp = True }))
      "print help"
  , Option [] ["debug"]
      (NoArg (\ opts -> opts { optDebug = True }))
      "very chatty output on stderr"
  , Option ['w'] ["wb", "whitebalanceImg"]
      (ReqArg (\f opts -> opts { optWhitebalance = WBFromImage f})
              "FILE")
      "graycard image used for whitebalance (otherwise whitebalance information in RAW is used)"
  , Option [] ["wb1"]
      (NoArg (\opts -> opts { optWhitebalance = WBFromFirstImage}))
      "use first images as graycard image for whitebalance (otherwise whitebalance information in RAW is used)"
  , Option ['q'] []
      (OptArg ((\f opts -> opts { optQuality = f })
               . fromMaybe 3
               . (fmap (read :: String -> Int)))
              "QUALITY_LEVEL")
      "set quality level"
  , Option ['l'] ["linearColorspace"]
      (NoArg (\opts -> opts { optColorSpace = LinearColorSpace }))
      "use linear colorspace (no argument implies sRGB)"
  , Option [] ["defaultColorspace"]
      (NoArg (\opts -> opts { optColorSpace = DefaultColorSpace }))
      "use default colorspace (no argument implies sRGB)"
  ]

help :: String
help = let
    header = "Usage: unraw [OPTION...] files..."
  in usageInfo header options

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = case getOpt Permute options argv of
                   (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ help))

calculateUnRAWedName :: FilePath -> FilePath
calculateUnRAWedName = (`replaceExtension` "tiff")

getDcrawArgs :: Options -> [Img] -> IO ([String], [Img])
getDcrawArgs opts = let
    addVerbosityArgs :: Options -> ([String], [Img]) -> IO ([String], [Img])
    addVerbosityArgs Options{optVerbose = True} (args,imgs) = pure (args ++ ["-v"], imgs)
    addVerbosityArgs _                          v           = pure v

    addWhiteBalanceArgs :: Options -> ([String], [Img]) -> IO ([String], [Img])
    addWhiteBalanceArgs Options{optWhitebalance = WBFromRaw}         (args, imgs)      = pure (args ++ ["-W"], imgs)
    addWhiteBalanceArgs Options{optWhitebalance = WBFromImage wbImg} (args, imgs)      = do
      multipliers <- calculateWhitebalance wbImg
      return (args ++ ["-r"] ++ multipliers, imgs)
    addWhiteBalanceArgs Options{optWhitebalance = WBFromFirstImage} (args, wbImg:imgs) = addWhiteBalanceArgs (defaultOptions{optWhitebalance = WBFromImage wbImg}) (args, imgs)
    addWhiteBalanceArgs Options{optWhitebalance = WBFromFirstImage} (_, [])            = fail "missing first image for whitebalance"

    addColorspaceArgs :: Options -> ([String], [Img]) -> IO ([String], [Img])
    addColorspaceArgs Options{optColorSpace = ocs} (args,imgs) = pure (args ++ colorSpaceToArgs ocs, imgs)

    addQualityArgs :: Options -> ([String], [Img]) -> IO ([String], [Img])
    addQualityArgs Options{optQuality = 3} v           = pure v
    addQualityArgs Options{optQuality = q} (args,imgs) = pure (args ++ ["-q", show q], imgs)

    addOutputFormaArgs :: ([String], [Img]) -> IO ([String], [Img])
    addOutputFormaArgs (args,imgs) = pure (args ++ ["-T"], imgs)
  in \imgs -> do
    (dcrawArgs, imgs') <- addVerbosityArgs opts ([], imgs) >>= addWhiteBalanceArgs opts >>= addColorspaceArgs opts >>= addQualityArgs opts >>= addOutputFormaArgs
    when (optDebug opts) $ do
      putStrLn "dcrawArgs:"
      print dcrawArgs
      putStrLn "imgs:"
      print imgs'
    return (dcrawArgs, imgs')

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
  (dcrawArgs, imgs') <- getDcrawArgs opts imgs

  (_, _, _, pHandle) <- createProcess (proc "dcraw" (dcrawArgs ++ imgs'))
  exitCode <- waitForProcess pHandle
  return (case exitCode of
            ExitSuccess -> Right (map calculateUnRAWedName imgs') -- TODO: get generated output from "Writing data to <OUTPUT> ..." lines
            _           -> Left ("UnRAW failed with " ++ show exitCode))

unRAW :: PrePAction
unRAW ["-h"] = PAction (\_ -> pure (Left help))
unRAW args   = logSeparator "Run UnRAW" <> PAction (unRAWimpl args)
