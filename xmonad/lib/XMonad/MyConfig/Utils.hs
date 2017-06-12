module XMonad.MyConfig.Utils
       where

shortenStatus :: String -> String
shortenStatus = let

  shortenPaths :: String -> String
  shortenPaths ('~':('/': p)) = "~/" ++ shortenPaths' p ""
  shortenPaths ('/': p)       = "/" ++ shortenPaths' p ""
  shortenPaths p              = p

  shortenPaths' :: String -> String -> String
  shortenPaths' p@('<':_) r = r ++ p
  shortenPaths' p@('>':_) r = r ++ p
  shortenPaths' ""        r = r
  shortenPaths' "/"       r = r ++ "/"
  shortenPaths' ('/':p)   _ = shortenPaths' p ".../"
  shortenPaths' (c:p)     r = shortenPaths' p (r++[c])

  in unwords . map shortenPaths . words
