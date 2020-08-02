{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is required for the 'makeLenses' expansion

{-# LANGUAGE RankNTypes #-}
-- We want to use a Lens in a newtype

module Model where

import Control.Lens
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Lazy (StateT (..), gets, modify, withStateT)

data Game = Game
  { _player :: Player,
    _others :: [Player],
    _supply :: [Card],
    _trash :: [Card]
  }
  deriving (Show)

data CardType = ActionType | Treasure | Victory deriving (Show, Eq)

--TODO: Cards should be parameterized by their card types
--such that only Treasure cards have a value, Action cards have an action etc.
data Card = Card
  { _name :: String,
    _description :: String,
    _cardType :: CardType,
    _value :: Int,
    _cost :: Int
  } deriving (Show, Eq)

data Player = Player
  { _gold :: Int,
    _buys :: Int,
    _actions :: Int,
    _deck :: [Card],
    _hand :: [Card],
    _discard :: [Card]
  } deriving (Show)




-- Lenses.  These must be declared after all models or Template Haskell gives
-- nonsensical errors

makeLenses ''Game
makeLenses ''Card
makeLenses ''Player

-- Interpreters

type Action = StateT Game IO ()

newtype GameLens a = GameLens (Lens' Game a)


-- Card instances

market :: Card
market =
  Card
    { _name = "Market",
      _description = "",
      _cardType = ActionType,
      _value = 0,
      _cost = 5
    }

village :: Card
village =
  Card
    { _name = "Village",
      _description = "",
      _cardType = ActionType,
      _value = 0,
      _cost = 3
    }

copper :: Card
copper =
  Card
    { _name = "Copper",
      _description = "",
      _cardType = Treasure,
      _value = 1,
      _cost = 3
    }

player1 :: Player
player1 =
  Player
    { _gold = 2,
      _buys = 3,
      _actions = 1,
      _deck = [market, village, village, market, market],
      _hand = [market, copper],
      _discard = []
    }

game :: Game
game =
  Game
    { _player = player1,
      _others = [],
      _supply = [village, market],
      _trash = []
    }
