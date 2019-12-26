{-# LANGUAGE LambdaCase #-}
module MyPhoto.Actions.Stack
    ( stack
    ) where

import           Control.Concurrent.Async ( mapConcurrently )
import           Control.Concurrent.MSem as MS
import           Control.Monad
import           Data.List.Split ( chunksOf )
import           Data.Maybe ( fromMaybe )
import           GHC.Conc ( numCapabilities )
import           System.Console.GetOpt
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

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
  , optChunks     :: Maybe Int
  , optProjection :: Projection
  , optOpts       :: Opts
  , optConcurrent :: Maybe Int
  , optAll        :: Bool
  , optSaveMasks  :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions
  = Options
  { optVerbose    = False
  , optHelp       = False
  , optChunks     = Nothing
  , optProjection = Proj1
  , optOpts       = Opts1
  , optConcurrent = Nothing
  , optAll        = False
  , optSaveMasks  = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
      "chatty output on stderr"
  , Option ['h'] ["help"]
      (NoArg (\ opts -> opts { optHelp = True }))
      "print help"
  , Option ['c'] ["chunks"]
      (ReqArg (\f opts -> opts { optChunks = Just (read f) })
       "CHUNKS")
      "split stacking in smaller chunks"
  , Option ['p'] ["projection"]
      (ReqArg (\f opts -> opts { optProjection = case f of
                                   "1" -> Proj1
                                   "2" -> Proj2
                                   "3" -> Proj3
                                   _   -> Proj1
                               }) "PROJECTION_NUMBER")
      "choose kind of projection"
  , Option ['o'] ["opts"]
      (ReqArg (\f opts -> opts { optOpts = case f of
                                   "1" -> Opts1
                                   "2" -> Opts2
                                   "3" -> Opts3
                                   _   -> Opts1
                               }) "OPTS_NUMBER")
      "choose kind of opts"
  , Option ['t'] ["threads"]
      (OptArg (\case
                  Just capabilities -> (\opts -> opts { optConcurrent = Just (read capabilities) })
                  Nothing           -> (\opts -> opts { optConcurrent = Just numCapabilities} ))
        "CAPABILITIES")
      "run in parallel"
  , Option ['a'] ["all"]
      (NoArg (\ opts -> opts { optSaveMasks = True }))
      "do all opts / projection variants"
  , Option ['s'] ["save-masks"]
      (NoArg (\ opts -> opts { optSaveMasks = True }))
      "save the masks"
  ]

optionsToFilenameAppendix :: Options -> String
optionsToFilenameAppendix o = let
    projectionOptionsToFilenameAppendix :: Options -> String
    projectionOptionsToFilenameAppendix Options {optProjection = Proj1} = "p1"
    projectionOptionsToFilenameAppendix Options {optProjection = Proj2} = "p2"
    projectionOptionsToFilenameAppendix Options {optProjection = Proj3} = "p3"
    optsOptionsToFilenameAppendix :: Options -> String
    optsOptionsToFilenameAppendix Options {optOpts = Opts1} = "o1"
    optsOptionsToFilenameAppendix Options {optOpts = Opts2} = "o2"
    optsOptionsToFilenameAppendix Options {optOpts = Opts3} = "o3"
  in "_" ++ projectionOptionsToFilenameAppendix o ++ optsOptionsToFilenameAppendix o

help :: String
help = let
    header = "Usage: stack [OPTION...] files..."
    projectionsDesc = unlines [ "    Projections:"
                              , "      1: " ++ show (projectionToArgs Proj1)
                              , "      2: " ++ show (projectionToArgs Proj2)
                              , "      3: " ++ show (projectionToArgs Proj3)
                              ]
    optionsDesc = unlines [ "    Opts:"
                          , "      1: " ++ show (optsToArgs Opts1)
                          , "      2: " ++ show (optsToArgs Opts2)
                          , "      3: " ++ show (optsToArgs Opts3)
                          ]
  in unlines [ usageInfo header options
             , projectionsDesc
             , optionsDesc]

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

getOutArguments :: Options -> Img -> IO (Img, Img -> IO [String])
getOutArguments opts img = let
    (bn,ext) = splitExtensions img
  in do
    outFile <- findOutFile (bn ++ "_STACKED" ++ optionsToFilenameAppendix opts) ext
    appendFile outFile ""
    return (outFile, \outFile -> let
                 outMasksFolder = outFile ++ "-masks"
               in do
                 when (optSaveMasks opts) $
                   createDirectoryIfMissing True outMasksFolder
                 return (( "--output=" ++ outFile ++ "" ) : ["--save-masks=\"" ++ outMasksFolder ++ "/softmask-%04n.tif:" ++ outMasksFolder ++ "/hardmask-%04n.tif\"" | optSaveMasks opts]))

runEnfuse :: (Img, Img -> IO [String], [String]) -> [Img] -> PActionBody
runEnfuse _ [img] = return (Right [img])
runEnfuse (outFile, outArgsFun, enfuseArgs) imgs' = do
      outArgs <- outArgsFun outFile
      (_, _, _, pHandle) <- createProcess (proc "enfuse" (enfuseArgs
                                                          ++ outArgs
                                                          ++ imgs'))
      exitCode <- waitForProcess pHandle

      case exitCode of
        ExitSuccess -> do
          putStrLn ("### done with " ++ outFile)
          return (Right [outFile])
        _           -> do
          let msg = "Stack of " ++ outFile ++ " failed with " ++ show exitCode
          putStrLn ("### " ++ msg)
          return (Left msg)

stackImpl :: [String] -> [Img] -> PActionBody
stackImpl args imgs = let
    foldResults :: Either String [Img] -> Either String [Img] -> Either String [Img]
    foldResults (Left err1)   (Left err2)   = Left (unlines [err1, err2])
    foldResults r1@(Left _)   _             = r1
    foldResults (Right imgs1) (Right imgs2) = Right (imgs1 ++ imgs2)
    foldResults _             r2@(Left _)   = r2

    stackImpl'' :: (MS.MSem Int) -> Options -> (Img, Img -> IO [String], [String]) -> [Img] -> PActionBody
    stackImpl'' sem opts (outFile, outArgsFun, enfuseArgs) imgs' = let
        numOfImages = length imgs'
        sizeToReduceTo = case optChunks opts of
          Nothing                                                        -> Nothing
          Just maxChunkSize | numOfImages <= maxChunkSize                -> Nothing
                            | numOfImages <= maxChunkSize * maxChunkSize -> Just maxChunkSize
                            | otherwise                                  -> Just (numOfImages `div` maxChunkSize)
      in case sizeToReduceTo of
        Nothing -> do
          putStrLn ("### use enfuse to calculate " ++ outFile)
          when (optVerbose opts) $
            putStrLn ("#### " ++ (show imgs'))

          runEnfuse (outFile, outArgsFun, enfuseArgs) imgs'
        Just maxChunkSize -> let
            chunks = chunksOf (numOfImages `div` (ceiling ((fromIntegral numOfImages) / (fromIntegral maxChunkSize)))) imgs'
            numberOfChunks = length chunks
          in do
            when (optVerbose opts) $ do
              putStrLn ("#### use " ++ show numberOfChunks ++ " chunks to calculate " ++ outFile)
            results <- mapConcurrently (MS.with sem . (\(i,c) -> do
                                let (bn,ext) = splitExtensions outFile
                                partOutFile <- findOutFile (bn ++ "_CHUNK_" ++ show i ++ "-" ++ show numberOfChunks) ext

                                stackImpl'' sem opts (partOutFile, outArgsFun, enfuseArgs) c)) (zip [1..] chunks)
            case foldl foldResults (Right []) results of
              Right imgs' -> stackImpl'' sem opts (outFile, outArgsFun, enfuseArgs) imgs'
              Left err    -> return (Left err)

    stackImpl' :: (MS.MSem Int) -> Options -> PActionBody
    stackImpl' sem opts = let
        enfuseArgs = getEnfuseArgs opts
      in do
        (outFile, outArgsFun) <- getOutArguments opts (head imgs)
        stackImpl'' sem opts (outFile, outArgsFun, enfuseArgs) imgs
  in do
  (opts, _) <- getMyOpts args
  sem <- MS.new (fromMaybe 1 (optConcurrent opts)) -- semathore to limit number of parallel threads
  if optAll opts
    then do
      results <- mapConcurrently (MS.with sem . (stackImpl' sem))
        [ opts {optAll = False, optProjection = Proj1, optOpts = Opts1}
        , opts {optAll = False, optProjection = Proj1, optOpts = Opts2}
        , opts {optAll = False, optProjection = Proj1, optOpts = Opts3}
        , opts {optAll = False, optProjection = Proj2, optOpts = Opts1}
        , opts {optAll = False, optProjection = Proj2, optOpts = Opts2}
        , opts {optAll = False, optProjection = Proj2, optOpts = Opts3}
        , opts {optAll = False, optProjection = Proj3, optOpts = Opts1}
        , opts {optAll = False, optProjection = Proj3, optOpts = Opts2}
        , opts {optAll = False, optProjection = Proj3, optOpts = Opts3}
        ]
      return (foldl foldResults (Right []) results)
    else stackImpl' sem opts

stack :: PrePAction
stack ["-h"] = PAction (\_ -> pure (Left help))
stack args   = logSeparator "Run stack" <> PAction (stackImpl args)
