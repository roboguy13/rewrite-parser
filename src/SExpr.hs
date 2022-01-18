{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

-- {-# OPTIONS_GHC -Wall #-}

module SExpr
  where

import           Parser
import           Unify

import           Control.Monad
import           Data.Void

data Tree a = Leaf a | Node [Tree a]
  deriving (Functor, Show, Foldable, Traversable)

instance Applicative Tree where
  pure = return
  (<*>) = ap

instance Monad Tree where
  return = Leaf
  Leaf x >>= f = f x
  Node xs >>= f = Node $ sequence $ mapM (>>= f) xs

data SExprTerminal = SESymbol String | SEString String
  deriving (Show, Eq)

data SExprPrim a = SExprTerminal SExprTerminal | SEVar a
  deriving (Functor, Show)

pattern SESymbol' x = SExprTerminal (SESymbol x)
pattern SEString' x = SExprTerminal (SEString x)

instance Applicative SExprPrim where
  pure = return
  (<*>) = ap

instance Monad SExprPrim where
  return = SEVar
  SESymbol' x >>= _ = SESymbol' x
  SEString' s >>= _ = SEString' s
  SEVar x >>= f = f x

-- TODO: Keep track of source locations for better error messages, better
-- debugging info and better profiling info? Also should annotate SEVars
-- with this
newtype SExpr a = SExpr { getSExpr :: Tree (SExprPrim a) }
  deriving (Functor, Show)

pattern Cons x xs = SExpr (Node (Leaf x : xs))
pattern Nil = SExpr (Node [])
pattern Sym x = SExpr (Leaf (SESymbol' x))
pattern Str x = SExpr (Leaf (SEString' x))
pattern Var x = SExpr (Leaf (SEVar x))

toTreeList :: Tree a -> [Tree a]
toTreeList (Node xs) = xs
toTreeList (Leaf x)  = [Node [Leaf x]]

seList :: [SExpr a] -> SExpr a
seList xs = SExpr $ Node (concatMap (toTreeList . getSExpr) xs)

viewSExpr :: SExpr a -> [SExpr a]
viewSExpr (SExpr e) = map SExpr $ toTreeList e

instance Unify SExpr where
  type UnifyTerm SExpr = SExprTerminal

  unifySubst v x expr = expr >>= go
    where
      go v' =
        if v' == v
          then fmap absurd x
          else pure v

  getUnifyPart (SExpr (Leaf (SESymbol' sym))) = UnifyTerminal (SESymbol sym)
  getUnifyPart (SExpr (Leaf (SEString' s)))   = UnifyTerminal (SEString s)
  getUnifyPart (SExpr (Leaf (SEVar v)))      = UnifyVar v
  getUnifyPart (SExpr (Node xs))             = UnifyNode (map SExpr xs)

  fromUnifyPart (UnifyTerminal (SESymbol sym)) = SExpr (Leaf (SESymbol' sym))
  fromUnifyPart (UnifyTerminal (SEString s)) = SExpr (Leaf (SEString' s))
  fromUnifyPart (UnifyVar v) = SExpr (Leaf (SEVar v))
  fromUnifyPart (UnifyNode xs) = SExpr (Node (map getSExpr xs))

instance Applicative SExpr where
  pure = return
  (<*>) = ap

instance Monad SExpr where
  return = SExpr . return . return
  SExpr x >>= f = SExpr (x >>= f')
    where
      f' (SESymbol' str) = Leaf $ SESymbol' str
      f' (SEVar v) = getSExpr $ f v

type SExprPrim' = SExprPrim String
type SExpr' = SExpr String

instance Parseable (SExpr String) where
  getParser = parseSExpr

forgetClosed :: SExpr Void -> SExpr a
forgetClosed = fmap absurd

toClosedSExpr :: SExpr' -> Maybe (SExpr Void)
toClosedSExpr (SExpr (Leaf (SESymbol' sym))) = Just $ SExpr (Leaf (SESymbol' sym))
toClosedSExpr (SExpr (Leaf (SEVar _))) = Nothing
toClosedSExpr (SExpr (Node xs)) = fmap (SExpr . Node . map getSExpr) $ traverse toClosedSExpr (map SExpr xs)

parseSExpr :: Parser SExpr'
parseSExpr = fmap SExpr parseSETree

parseSETree :: Parser (Tree SExprPrim')
parseSETree = parseSELeaf <|> parseSENode

parseSELeaf :: Parser (Tree SExprPrim')
parseSELeaf = Leaf <$> go
  where
    go = fmap SEVar parseVar <|> fmap SESymbol' parseSymbol

parseSENode :: Parser (Tree SExprPrim')
parseSENode = Node <$> go
  where
    go = parseBracketed (parseChar '(') (parseChar ')') (parseSequence parseWhitespace parseSETree)

parseVar :: Parser String
parseVar = parseChar '?' *> some (parseSymbolChar False)

parseSymbol :: Parser String
parseSymbol = (:) <$> (parseSymbolChar True) <*> (many (parseSymbolChar False))

parseSymbolChar :: Bool -> Parser Char
parseSymbolChar isInitial = parseNotOneOf (varPrefix ++ " \t\n\r'\"()")
  where
    varPrefix =
      if isInitial
        then "?"
        else ""

