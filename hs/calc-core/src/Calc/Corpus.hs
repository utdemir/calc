module Calc.Corpus
  ( Corpus,
    default_,
    lookup,
    lookupPrefixes,
    singleton,
  )
where

import Calc.Lexer.Types
import Calc.Tokenizer
import qualified Data.Map as Map
import qualified Data.Set as Set

data Corpus = Corpus
  { cContents :: Map (NonEmpty Token) (Set Lexeme),
    cMaxLength :: Int
  }

instance Semigroup Corpus where
  Corpus c1 l1 <> Corpus c2 l2 =
    Corpus
      (Map.unionWith (<>) c1 c2)
      (max l1 l2)

instance Monoid Corpus where
  mempty = Corpus mempty 1

singleton :: Lexeme -> NonEmpty Token -> Corpus
singleton l t =
  Corpus
    (Map.singleton t (Set.singleton l))
    (length t)

lookup :: NonEmpty Token -> Corpus -> Set Lexeme
lookup ts c =
  cContents c
    & Map.lookup ts
    & fromMaybe Set.empty

lookupPrefixes :: [Token] -> Corpus -> Set (Lexeme, [Token])
lookupPrefixes ts corpus =
  [1 .. cMaxLength corpus]
    & map (flip splitAt ts)
    & foldMap
      ( \(x, xs) ->
          case nonEmpty x of
            Nothing -> Set.empty
            Just x' ->
              lookup x' corpus
                & Set.map (,xs)
      )

default_ :: Corpus
default_ =
  mconcat
    []
