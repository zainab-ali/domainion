{-# LANGUAGE FlexibleInstances #-}
-- We want to make an instance for StateT, but:
    -- • Illegal instance declaration for ‘BoolSYM (StateT Game IO)’
    --     (All instance types must be of the form (T a1 ... an)
    --      where a1 ... an are *distinct type variables*,
{-# LANGUAGE RankNTypes #-}
-- We want to use a Lens in a newtype
{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is required for the 'makeLenses' expansion
module Predicate where

import Types (Pretty(..), GameLens(..), pretty')
import Final (IntSYM(..))
import Control.Newtype.Generics (over, pack, over2, unpack)
import GameJam (Game, CardType, Card(..))
import qualified GameJam as G (value, cost, cardType, supply, discard, hand, trash, deck, player)
import Control.Monad.Trans.State.Lazy as S (StateT)
import Control.Monad.Trans.Class (lift)
import Prelude (Bool(..), Int, Eq, IO, (++), pure, String, (.), const, id, filter, head, return, ($), (<>), show)
import Data.List (delete)
import Control.Lens (Lens', makeLenses, use, (%=))
import Control.Applicative (liftA2, liftA)
import qualified Prelude as P (not, (&&), (||), (<), (==), (+))


-- Workshop

-- There's something deeply, deeply dissatisfying about our previous solution

-- We probably want signatures like this
-- class MoveSYM repr where
--   pick :: repr (GameLens [Card]) -> (repr Card -> repr Bool) -> repr Card
--   put :: repr Card -> repr ()

-- We're going to interpret this into a StateT.  But it's a predicate!  There's nothing stateful about it!

-- But it _has_ to be a state, doesn't it?  Even if we add types, we can't get around that.
-- Or can we?
--
-- We've been assuming that we only have one language.  But I'll contend that we actually have more
-- We have a language of predicates, and a language of cards
--
-- The interpreter for our bigger language might be stateful, but the interpreter for our smaller one should not.
--

-- Let's try and code up a language of predicates and see

-- We'll need a language for Bools, and another one for getting Card fields
-- StateT Game IO (Card -> Int) StateT Game IO Card (Int -> Bool)
-- StateT

-- We may as well flesh it out some more
class BoolSYM repr where
  true :: repr Bool
  not :: repr Bool -> repr Bool
  (&&) :: repr Bool -> repr Bool -> repr Bool
  (||) :: repr Bool -> repr Bool -> repr Bool
  (<) :: repr Int -> repr Int -> repr Bool
  -- We can use typeclasses to enforce additional constraints within the syntax
  (==) :: Eq a => repr a -> repr a -> repr Bool

instance BoolSYM Pretty where
  true = Pretty "true"
  not = over pack (\x -> "(not " ++ x ++ ")")
  (&&)  = over2 pack (\x -> \y -> "(" ++ x ++ " and " ++ y ++ ")")
  (||)  = over2 pack (\x -> \y -> "(" ++ x ++ " or " ++ y ++ ")")
  (<)  = over2 pack (\x -> \y -> "(" ++ x ++ " < " ++ y ++ ")")
  (==)  = over2 pack (\x -> \y -> "(" ++ x ++ " == " ++ y ++ ")")

instance BoolSYM ((->) a) where
  true = const True
  not = (P.not .)
  (&&)  = liftA2 (P.&&)
  (||)  = liftA2 (P.||)
  (<)  = liftA2 (P.<)
  (==)  = liftA2 (P.==)

class CardSYM repr where
  value :: repr Int
  cost :: repr Int
  cardType :: repr CardType
  liftCardType :: CardType -> repr CardType

instance CardSYM Pretty where
  value = Pretty "value"
  cost = Pretty "cost"
  cardType = Pretty "type"
  liftCardType = Pretty . show

instance CardSYM ((->) Card) where
  value = _value
  cost = _cost
  cardType = _cardType
  liftCardType = const

instance IntSYM ((->) a Int) where
  lit = const
  (+) = liftA2 (P.+)

costsLessThan4 :: (
  CardSYM repr,
  BoolSYM repr,
  IntSYM (repr Int)
  ) => repr Bool
costsLessThan4 = cost < lit 4

-- It's a pain to write all these out, so let's clean them up intp a single typeclass
class (CardSYM repr, BoolSYM repr, IntSYM (repr Int)) => CardP repr

instance CardP Pretty
instance CardP ((->) Card)

fun :: (Card -> a) -> (Card -> a)
fun = id

-- Let's try
-- _pretty (pretty' costsLessThan4)
-- "(cost < 4)"
-- Awesome!

-- costsLessThan4 market
-- True

-- How do we "embed" a language in another?
--
-- We want to embed a piece of syntax belonging to the predicate language into the syntax of the action language
-- We don't know what it is yet.  We can't have interpreted it.
-- This means we want to take in a CardP repr as an _argument_
-- I will accept a function that is syntax of the CardP language
class MoveSYM repr where
  pick :: CardPSelf -> repr (GameLens [Card]) -> repr Card
  put :: repr (GameLens [Card]) -> repr Card -> repr ()

data CardPSelf = CardPSelf (forall p. CardP p => p Bool)

instance MoveSYM (StateT Game IO) where
  pick (CardPSelf p) mlens = do (GameLens lens) <- mlens
                                cards <- use lens
                                let selection = filter (fun p) cards
                                card <- lift (playerPicksFrom selection)
                                lens %= delete card
                                return card
  put mlens mcard = do card <- mcard
                       (GameLens lens) <- mlens
                       lens %= (card :)

instance MoveSYM Pretty where
  pick (CardPSelf p) pile = pack $ "(pick a card satisfying " <> unpack (pretty' p) <> " from " <> unpack pile <> ")"
  put pile card = pack $ "(put " <> unpack card <> " into " <> unpack pile <> ")"

-- Let's cheat a bit and not bother with user input
playerPicksFrom :: [a] -> IO a
playerPicksFrom = pure . head

workshop :: (
  MoveSYM repr,
  PileSYM (repr (GameLens [Card]))) => repr ()
workshop = put discard (pick (CardPSelf (cost < lit 4)) supply)

-- We need a langage for piles
class PileSYM repr where
  supply :: repr
  trash :: repr
  hand :: repr
  deck :: repr
  discard :: repr

instance PileSYM (Pretty a) where
  supply = Pretty "supply"
  trash = Pretty "trash"
  hand = Pretty "hand"
  deck = Pretty "deck"
  discard = Pretty "discard"

instance PileSYM (StateT Game IO (GameLens [Card])) where
  supply = pure supply
  trash = pure trash
  hand = pure hand
  deck = pure deck
  discard = pure discard

-- > game
-- Game {_player = Player {_gold = 2, _buys = 3, _actions = 1, _deck = [( Card "Market" "" ActionType 0 5),( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5),( Card "Market" "" ActionType 0 5)], _hand = [( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5)], _discard = []}, _others = [], _supply = [( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5)], _trash = []}
-- > runStateT (eval workshop) game
-- ((),Game {_player = Player {_gold = 2, _buys = 3, _actions = 1, _deck = [( Card "Market" "" ActionType 0 5),( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5),( Card "Market" "" ActionType 0 5)], _hand = [( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5)], _discard = [( Card "Village" "" ActionType 0 3)]}, _others = [], _supply = [( Card "Market" "" ActionType 0 5)], _trash = []})

-- But... piles aren't stateful either!
data PileSYMSelf = PileSYMSelf (forall repr. PileSYM repr => repr)

class MoveSYM' repr where
  pick' :: CardPSelf -> PileSYMSelf -> repr Card
  put' :: PileSYMSelf -> repr Card -> repr ()

instance MoveSYM' (StateT Game IO) where
  pick' (CardPSelf p) (PileSYMSelf pileSyntax) =
    let (GameLens pile) = pileSyntax
    in do cards <- use pile
          let selection = filter p cards
          card <- lift (playerPicksFrom selection)
          pile %= delete card
          return card

  put' (PileSYMSelf pileSyntax) mcard =
    let (GameLens pile) = pileSyntax
    in do card <- mcard
          pile %= (card :)

instance PileSYM (GameLens [Card]) where
  supply = GameLens G.supply
  trash = GameLens G.trash
  hand = GameLens (G.player . G.hand)
  deck = GameLens (G.player . G.deck)
  discard = GameLens (G.player . G.discard)


workshop' :: (
  MoveSYM' repr,
  PileSYM (repr (GameLens [Card]))) => repr ()
workshop' = put' (PileSYMSelf discard) (pick' (CardPSelf (cost < lit 4)) (PileSYMSelf supply))

-- runStateT (eval workshop') game
-- ((),Game {_player = Player {_gold = 2, _buys = 3, _actions = 1, _deck = [( Card "Market" "" ActionType 0 5),( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5),( Card "Market" "" ActionType 0 5)], _hand = [( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5)], _discard = [( Card "Village" "" ActionType 0 3)]}, _others = [], _supply = [( Card "Market" "" ActionType 0 5)], _trash = []})
