{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is required for the 'makeLenses' expansion
module GameJam where

import Control.Lens
import Control.Monad.Trans (lift)

import Control.Monad.Trans.State.Lazy (StateT(..), gets, modify, withStateT)

data Game = Game {
   _player :: Player,
   _others :: [Player],
   _supply :: [Card],
   _trash :: [Card]
}

data CardType = ActionType | Treasure | Victory deriving (Show, Eq)

data Card = Card {
  _name :: String,
  _description :: String,
  _cardType :: CardType,
  _action :: StateT Game IO ()
}

data Player = Player {
   _gold :: Int,
   _buys :: Int,
   _actions :: Int,
   _deck :: [Card],
   _hand :: [Card],
   _discard :: [Card]
}

type Action = StateT Game IO ()

makeLenses ''Game
makeLenses ''Card
makeLenses ''Player

marketAction :: Action
marketAction =
   drawCard >>
   plusAction >>
   plusBuy >>
   plusGold

villageAction :: Action
villageAction =
   drawCard >>
   plusAction >>
   plusAction

drawCard :: Action
drawCard = do cards <- use (player . deck)
              case cards of
                [] -> lift (fail "You need more cards!")
                card : rest ->
                  player . deck .= rest >>
                  player . hand %= (card :)

plusAction :: Action
plusAction = player . actions += 1

plusBuy :: Action
plusBuy = player . buys += 1

plusGold :: Action
plusGold = player . gold += 1
