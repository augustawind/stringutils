module StringUtils where

import Data.Char (toLower)
import Data.List (dropWhileEnd)

-- | Wrap a String to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> String -> String
wordWrap width str
  | length str' <= width = str'
  | otherwise = rstrip firstLine ++ case wordWrap width rest of
                                      ""    -> ""
                                      rest' -> '\n':rest'
  where str' = filter notLineBreak . lstrip $ str
        notLineBreak = not . (`elem` "\n\r")
        (firstLine, rest) = splitAtWith isWhitespace width str'

-- | Split a String at the given width, or at the closest character that
-- fulfills the given predicate, whichever comes first. Return each
-- half of the split in a 2-tuple.
splitAtWith :: (Char -> Bool) -> Int -> String -> (String, String)
splitAtWith p width str = (first, rest)
    where 
        first = case dropWhileEnd (not . p) line of
                  ""   -> line 
                  str' -> init str'
        rest = drop (length first) $ str
        line = take width str

-- | Strip leading and trailing whitespace and make everything lowercase.
normalize :: String -> String
normalize = map toLower . strip

-- | Remove leading and trailing whitespace from a String.
strip :: String -> String
strip = lstrip . rstrip

-- | Remove leading whitespace from a String.
lstrip :: String -> String
lstrip = dropWhile isWhitespace

-- | Remove trailing whitespace from a String
rstrip :: String -> String
rstrip = dropWhileEnd isWhitespace

isWhitespace :: Char -> Bool
isWhitespace = (`elem` [' ', '\t', '\n', '\r'])
