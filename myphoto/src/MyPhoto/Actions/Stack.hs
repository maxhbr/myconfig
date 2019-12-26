module MyPhoto.Actions.Stack
    ( stack
    ) where

import           System.FilePath
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           System.Process
import           System.Exit
import           System.Directory
import           System.IO.Temp
import           Text.Printf

import MyPhoto.Model
import MyPhoto.Utils

data Projection
  = Proj1
  | Proj2
  | Proj3
  deriving Show
projectionToArgs :: Projection -> [String]
projectionToArgs Proj1 = ["--gray-projector=l-star"]
projectionToArgs Proj2 = []
projectionToArgs Proj3 = ["--gray-projector=luminance"]
data Opts
  = Opts1
  | Opts2
  | Opts3
  deriving Show
optsToArgs :: Opts -> [String]
optsToArgs Opts1 = ["--contrast-window-size=5"]
optsToArgs Opts2 = ["--contrast-edge-scale=0.3"]
optsToArgs Opts3 = ["--contrast-edge-scale=31", "--contrast-min-curvature=11"]

data Options
  = Options
  { optVerbose    :: Bool
  , optHelp       :: Bool
  , optProjection :: Projection
  , optOpts       :: Opts
  , optSaveMasks  :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions
  = Options
  { optVerbose    = False
  , optHelp       = False
  , optProjection = Proj1
  , optOpts       = Opts1
  , optSaveMasks  = False -- TODO
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
      "chatty output on stderr"
  , Option ['h'] ["help"]
      (NoArg (\ opts -> opts { optHelp = True }))
      "print help"
  , Option ['p'] ["projection"]
      (ReqArg (\f opts -> opts { optProjection = case f of
                                   "1" -> Proj1
                                   "2" -> Proj2
                                   "3" -> Proj3
                                   _   -> Proj1
                               }) "PROJECTION_NUMBER")
      "choos kind of projection"
  , Option ['o'] ["opts"]
      (ReqArg (\f opts -> opts { optOpts = case f of
                                   "1" -> Opts1
                                   "2" -> Opts2
                                   "3" -> Opts3
                                   _   -> Opts1
                               }) "OPTS_NUMBER")
      "choos kind of opts"
  ]


help :: String
help = let
    header = "Usage: stack [OPTION...] files..."
  in usageInfo header options

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = case getOpt Permute options argv of
                   (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ help))

getEnfuseArgs :: Options -> [String]
getEnfuseArgs opts = let
    focusStackArgs = ["--exposure-weight=0", "--saturation-weight=0", "--contrast-weight=1"]
    hardMaskArgs = ["--hard-mask"]
    verbosityArgs = ["-v" | optVerbose opts]
  in verbosityArgs
     ++ focusStackArgs
     ++ hardMaskArgs
     ++ projectionToArgs (optProjection opts)
     ++ optsToArgs (optOpts opts)

getOutArguments :: Options -> Img -> IO (Img, [String])
getOutArguments opts img = let
    (bn,ext) = splitExtensions img
  in do
    outFile <- findOutFile (bn ++ "_STACKED") ext
    appendFile outFile ""
    let outMasksFolder = outFile ++ "-masks"
    when (optSaveMasks opts) $
      createDirectoryIfMissing True outMasksFolder
    return (outFile, ( "--output=" ++ outFile ++ "" ) :
                     ["--save-masks=\"" ++ outMasksFolder ++ "/softmask-%04n.tif:" ++ outMasksFolder ++ "/hardmask-%04n.tif\"" | optSaveMasks opts])

stackImpl :: [String] -> [Img] -> PActionBody
stackImpl args imgs = do
  (opts, _) <- getMyOpts args
  let enfuseArgs = getEnfuseArgs opts

  (outFile, outArgs) <- getOutArguments opts (head imgs)

  (_, _, _, pHandle) <- createProcess (proc "enfuse" (enfuseArgs
                                                      ++ outArgs
                                                      ++ imgs))
  exitCode <- waitForProcess pHandle
  return (case exitCode of
            ExitSuccess -> Right [outFile]
            _           -> Left ("Stack failed with " ++ show exitCode))

stack :: PrePAction
stack ["-h"] = PAction (\_ -> pure (Left help))
stack args   = logSeparator "Run stack" <> PAction (stackImpl args)
