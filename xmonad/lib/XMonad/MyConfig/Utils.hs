module XMonad.MyConfig.Utils
       ( shortenStatus
       ) where

shortenPaths :: String -> String
shortenPaths ('~':('/': p)) = "~/" ++ shortenPaths' "" p
shortenPaths ('/': p)       = "/" ++ shortenPaths' "" p
shortenPaths ('[': p)       = '[' : if last p == ']'
                                    then shortenPaths' "" p
                                    else p
shortenPaths p              = p

shortenPaths' :: String -- output
              -> String -- input
              -> String
shortenPaths' r p@('<':_) = r ++ p
shortenPaths' r p@('>':_) = r ++ p
shortenPaths' r ""        = r
shortenPaths' r "/"       = r ++ "/"
shortenPaths' _ ('/':p)   = shortenPaths' ".../" p
shortenPaths' r (c:p)     = shortenPaths' (r++[c]) p

-- shortenStatus :: String -> String
-- shortenStatus = unwords . map shortenPaths . words

shortenStatus :: String -> String
shortenStatus = shortenStatus' "" ""

shortenStatus' :: String -- output
               -> String -- prefix of last word
               -> String -- input
               -> String
shortenStatus' r w ""              = r ++ shortenPaths w
shortenStatus' r w ('\\':(' ':is)) = shortenStatus' r (w ++"\\ ") is
shortenStatus' r w (' ':is)        = shortenStatus' (r ++ shortenPaths w ++ " ") "" is
shortenStatus' r w (i:is)          = shortenStatus' r (w++[i]) is
