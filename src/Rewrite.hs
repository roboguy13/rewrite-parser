module Rewrite
  where

import           SExpr
import           Parser

data Rewrite a = Rewrite a a

type SExprRewrite' = Rewrite SExpr'

-- instance Parseable a => Parseable (Rewrite a) where
--   getParser = parseRewrite

parseRewrite :: Parseable a => Parser (Rewrite a)
parseRewrite = do
  x <- getParser
  some parseWhitespace
  parseString "==>"
  some parseWhitespace
  y <- getParser
  pure (Rewrite x y)



