{-# LANGUAGE DeriveGeneric #-}
-- Required for constructing newtypes
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- We want to use forall quantification
-- Required for using forall quantification in Int
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

-- We want to make an instance for String, but String is a type synonym
module Final where

import Control.Applicative (liftA2)
import Control.Lens (Lens', makeLenses, set, use, (%=), (^.))
import qualified Control.Lens as L (over)
import Control.Lens.Tuple
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans (ilift)
import Control.Monad.Trans.Class (lift)
import Control.Newtype.Generics (Newtype, over, over2, pack, under, unpack)
import Data.List (delete)
import GHC.Generics (Generic)
import Model (Card (..), CardType (..), Game, GameLens (..))
import qualified Model as G
import Prelude (Bool (..), Eq, IO, Int, Monad, String, const, filter, flip, fromInteger, head, id, pure, putStrLn, show, ($), (++), (.), (<>))
import qualified Prelude as P

-- Integers
class IntSYM repr where
  lit :: Int -> repr
  (+) :: repr -> repr -> repr

-- Card predicate

class CardSYM repr where
  value :: repr Card -> repr Int
  cost :: repr Card -> repr Int
  cardType :: repr Card -> repr CardType
  liftCardType :: CardType -> repr CardType

class
  ( forall m. CardSYM (repr m),
    forall m. BoolSYM (repr m),
    forall m. IntSYM (repr m Int),
    VarSYM repr
  ) =>
  CardPSYM repr

class VarSYM repr where
  z :: repr (m, h) m
  s :: repr m a -> repr (any, m) a

class BoolSYM repr where
  true :: repr Bool
  not :: repr Bool -> repr Bool
  (&&) :: repr Bool -> repr Bool -> repr Bool
  (||) :: repr Bool -> repr Bool -> repr Bool
  (<) :: repr Int -> repr Int -> repr Bool
  (==) :: Eq a => repr a -> repr a -> repr Bool

instance BoolSYM ((->) a) where
  true = const True
  not = (P.not .)
  (&&) = liftA2 (P.&&)
  (||) = liftA2 (P.||)
  (<) = liftA2 (P.<)
  (==) = liftA2 (P.==)

instance IntSYM ((->) a Int) where
  lit = const
  (+) = liftA2 (P.+)

data CardPSYMSelf stack = CardPSYMSelf (forall p. CardPSYM p => p stack Bool)

minePredicate :: CardPSYM repr => repr (Card, (Card, m)) P.Bool
minePredicate = cost z < (cost (s z) + lit (fromInteger 4))

-- Piles
class PileSYM repr where
  supply :: repr
  trash :: repr
  hand :: repr
  deck :: repr
  discard :: repr

data PileSYMSelf = PileSYMSelf (forall repr. PileSYM repr => repr)

-- Action

class ActionSYM repr where
  pick :: CardPSYMSelf (Card, m) -> PileSYMSelf -> repr m a -> repr (Card, m) a
  put :: PileSYMSelf -> repr (Card, m) a -> repr m a

mine :: ActionSYM repr => repr m () -> repr m ()
mine =
  put (PileSYMSelf trash)
    . put (PileSYMSelf hand)
    . (pick (CardPSYMSelf (cost z < (cost (s z) + lit 4))) (PileSYMSelf supply))
    . (pick (CardPSYMSelf (cardType z == liftCardType Treasure)) (PileSYMSelf hand))

-- A pretty interpreter
newtype Pretty m a = Pretty String deriving (Generic)

instance Newtype (Pretty m a)

instance VarSYM Pretty where
  z = Pretty "z"
  s = over pack ("s" ++)

instance IntSYM (Pretty m Int) where
  lit = Pretty . show
  x + y = pack $ "(" <> unpack x <> " + " <> unpack y <> ")"

instance BoolSYM (Pretty m) where
  true = pack "true"
  not = over pack (\x -> "(not " ++ x ++ ")")
  (&&) = over2 pack (\x -> \y -> "(" ++ x ++ " and " ++ y ++ ")")
  (||) = over2 pack (\x -> \y -> "(" ++ x ++ " or " ++ y ++ ")")
  (<) = over2 pack (\x -> \y -> "(" ++ x ++ " < " ++ y ++ ")")
  (==) = over2 pack (\x -> \y -> "(" ++ x ++ " == " ++ y ++ ")")

-- instances for all the other Pretty things
instance CardSYM (Pretty m) where
  value = over pack ("value of " ++)
  cost = over pack ("cost of " ++)
  cardType = over pack ("card type of " ++)
  liftCardType = pack . show

instance CardPSYM Pretty

instance PileSYM (Pretty m a) where
  supply = Pretty "supply"
  trash = Pretty "trash"
  hand = Pretty "hand"
  deck = Pretty "deck"
  discard = Pretty "discard"

instance ActionSYM Pretty where
  pick (CardPSYMSelf p) (PileSYMSelf pile) prev =
    pack $
      "(" <> unpack prev <> "then (pick a card satisfying "
        <> unpack (pretty p)
        <> " from "
        <> unpack (pretty pile)
        <> "))"
  put (PileSYMSelf pile) card =
    pack $
      "(put " <> unpack card <> " into "
        <> unpack (pretty pile)
        <> ")"

pretty :: Pretty m a -> Pretty m a
pretty = id

prettyMine = pretty (mine (Pretty ""))

-- Card predicate interpreter

instance VarSYM (->) where
  z (m, _) = m
  s f (_, m) = f m

instance CardSYM ((->) m) where
  value = (_value .)
  cost = (_cost .)
  cardType = (_cardType .)
  liftCardType = const

instance CardPSYM (->)

-- Pile interpreter
instance PileSYM (GameLens [Card]) where
  supply = GameLens G.supply
  trash = GameLens G.trash
  hand = GameLens (G.player . G.hand)
  deck = GameLens (G.player . G.deck)
  discard = GameLens (G.player . G.discard)

-- Evaluator

data Eval m a = Eval {_eval :: IxStateT IO Game (Game, m) a}

instance ActionSYM Eval where
  pick (CardPSYMSelf p) (PileSYMSelf pileSyntax) prev =
    let (GameLens pile) = pileSyntax
        state = do
          a <- _eval prev
          cards <- igets (^. (_1 . pile))
          stack <- igets (^. _2)
          let selection = filter (\card -> p (card, stack)) cards
          card <- ilift (playerPicksFrom selection)
          ilift $ putStrLn ("deleting " ++ show card)
          imodify (L.over (_1 . pile) (delete card))
          imodify (L.over _2 (\prev -> (card, prev)))
          return a
     in Eval state
  put (PileSYMSelf pileSyntax) prev =
    let (GameLens pile) = pileSyntax
        state = do
          a <- _eval prev
          (card, stack) <- igets (^. _2)
          imodify (L.over (_1 . pile) (card :))
          imodify (set _2 stack)
          return a
     in Eval state

-- Mocked user input.  This will error on empty
playerPicksFrom :: [a] -> IO a
playerPicksFrom = pure . head

eval :: Eval m a -> Eval m a
eval = id

evalMine =
  let start = ireturn () >> imodify (\game -> (game, ()))
   in eval (mine (Eval start))

-- Indexed state monad helpers
return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) ::
  (Monad m) =>
  IxStateT m i j a ->
  (a -> IxStateT m j k b) ->
  IxStateT m i k b
(>>=) = flip ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = ibind (\_ -> w) v
