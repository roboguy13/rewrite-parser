{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Regex
  where

import           SExpr
import           Parser

data Regex
  = MatchString String
  | MatchOptional Regex
  | MatchMany Regex
  | MatchAlt [Regex]
  | MatchSeq [Regex]

mkSExprRegex :: SExpr' -> Maybe Regex
mkSExprRegex (viewSExpr -> [Sym "string", Str s]) = Just $ MatchString s
mkSExprRegex (viewSExpr -> [Sym "optional", x]) = MatchOptional <$> mkSExprRegex x
mkSExprRegex (viewSExpr -> [Sym "many", x]) = MatchMany <$> mkSExprRegex x
mkSExprRegex (viewSExpr -> [Sym "some", x]) =
  let re = mkSExprRegex x
  in
  MatchAlt <$> sequence [re, MatchMany <$> re]
mkSExprRegex (viewSExpr -> (Sym "alts":xs)) = MatchAlt <$> mapM mkSExprRegex xs
mkSExprRegex (viewSExpr -> (Sym "seq":xs)) = MatchSeq <$> mapM mkSExprRegex xs

matchRegex :: Regex -> String -> Maybe String
matchRegex re str =
  case parse (parseWithRegex re) str of
    Left _ -> Nothing
    Right result -> Just result

parseWithRegex :: Regex -> Parser String
parseWithRegex (MatchString str) = parseString str
parseWithRegex (MatchOptional p) =
      optional (parseWithRegex p) >>= \case
        Nothing -> pure ""
        Just x -> pure x
parseWithRegex (MatchMany p) = concat <$> many (parseWithRegex p)
parseWithRegex (MatchAlt ps) = foldr (<|>) empty $ map parseWithRegex ps
parseWithRegex (MatchSeq ps) = concat <$> mapM parseWithRegex ps

