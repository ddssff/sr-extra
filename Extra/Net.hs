module Extra.Net
    ( webServerDirectoryContents
    ) where

import qualified Data.ByteString.Lazy.Char8 as L
import		 Data.Maybe
import		 Text.Regex

-- | Parse the text returned when a directory is listed by a web
-- server.  This is currently only known to work with Apache.
webServerDirectoryContents :: L.ByteString -> [String]
webServerDirectoryContents text =
    catMaybes . map (second . matchRegex re) . lines . L.unpack $ text
    where
      re = mkRegex "( <A HREF|<a href)=\"([^/][^\"]*)/\""
      second (Just [_, b]) = Just b
      second _ = Nothing
