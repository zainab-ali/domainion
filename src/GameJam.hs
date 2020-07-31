{-# LANGUAGE TemplateHaskell #-}

-- Template Haskell is required for the 'makeLenses' expansion

module GameJam where

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

data Card = Card
  { _name :: String,
    _description :: String,
    _cardType :: CardType,
    _value :: Int,
    _cost :: Int,
    _action :: StateT Game IO ()
  }

instance Show Card where
  show (Card name desc tpe value cost _) =
    "( Card " <> show name <> " " <> show desc <> " " <> show tpe <> " " <> show value <> " " <> show cost <> ")"

instance Eq Card where
  x == y = _name x == _name y

data Player = Player
  { _gold :: Int,
    _buys :: Int,
    _actions :: Int,
    _deck :: [Card],
    _hand :: [Card],
    _discard :: [Card]
  }
  deriving (Show)

type Action = StateT Game IO ()

makeLenses ''Game
makeLenses ''Card
makeLenses ''Player

-- | This version uses a ton of lenses. They aren't necessary, but clean up a lot of boilerplate
-- Don't worry if you wrote something different
drawCard :: Action
drawCard = do
  cards <- use (player . deck)
  case cards of
    [] -> lift (fail "You need more cards!")
    card : rest ->
      player . deck .= rest
        >> player . hand %= (card :)

plusAction :: Action
plusAction = player . actions += 1

plusBuy :: Action
plusBuy = player . buys += 1

plusGold :: Action
plusGold = player . gold += 1

-- | Some game state to play with
marketAction :: Action
marketAction =
  drawCard
    >> plusAction
    >> plusBuy
    >> plusGold

market :: Card
market =
  Card
    { _name = "Market",
      _description = "",
      _cardType = ActionType,
      _value = 0,
      _cost = 5,
      _action = marketAction
    }

villageAction :: Action
villageAction =
  drawCard
    >> plusAction
    >> plusAction

village :: Card
village =
  Card
    { _name = "Village",
      _description = "",
      _cardType = ActionType,
      _value = 0,
      _cost = 3,
      _action = villageAction
    }

copper :: Card
copper = Card
    { _name = "Copper",
      _description = "",
      _cardType = Treasure,
      _value = 1,
      _cost = 3,
      _action = pure ()
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

game1 :: IO ((), Game)
game1 = runStateT marketAction game
