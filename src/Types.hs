{-# LANGUAGE FlexibleInstances #-}
-- We want to make an instance for String, but String is a type synonym
{-# LANGUAGE RankNTypes #-}
-- We want to use a Lens in a newtype
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- Required for constructing newtypes
module Types where

import Prelude (String, (<$>), (.), IO, Int, id, pure, Show, Eq)
import Control.Applicative (liftA2)
import Control.Lens
import qualified Prelude as P ((+))
import GHC.Generics (Generic)
import Control.Newtype.Generics (unpack, pack, Newtype, over, under)
import Final (IntSYM(..), pretty)
import GameJam as G (Action, Player(..), Game(..), actions, buys, gold, player)
import Control.Monad.Trans.State.Lazy as S (get, StateT, put)
import qualified Control.Monad.Trans.State.Lazy as S (modify)

class ResourceSYM repr where
  action :: repr
  buy :: repr
  gold :: repr

plusOneAction :: (IntSYM repr, ResourceSYM repr) => repr
plusOneAction = action + lit 1

instance ResourceSYM String where
  action = "action"
  buy = "buy"
  gold = "gold"

text1 = pretty plusOneAction
-- "(action + 1)"
-- Not bad!

eval :: Action -> Action
eval = id

-- instance ResourceSYM Action where
--   action = (_actions . _player) <$> get
--   buy = (_buys . _player) <$> get
--   gold = (_gold . _player) <$> get

-- • Couldn't match type ‘Int’ with ‘()’
--   Expected type: Action
--     Actual type: StateT Game IO Int

instance ResourceSYM (StateT Game IO Int) where
  action = (_actions . _player) <$> get
  buy = (_buys . _player) <$> get
  gold = (_gold . _player) <$> get

instance IntSYM (StateT Game IO Int) where
  lit = pure
  (+) = liftA2 (P.+)

-- eval plusOneAction
-- • No instance for (IntSYM (StateT Game IO ()))
--     arising from a use of ‘plusOneAction’

-- Of course we can't!

-- The statements in our DSL shouldn't have the type 'Int'
-- Given that we have a type Int, how can we produce a type ()?
--
-- There's no way that a word 'action' compositionally produces the state change we want

-- Let's think about what our syntax _means_.
-- Not only that, some statements don't make sense (action + gold)
-- mutable thing -- update function
newtype GameLens a = GameLens (Lens' Game a)
-- prevents an "Illegal polymorphic type".  Lenses can be complex

instance ResourceSYM (GameLens Int) where
  action = GameLens (player . actions)
  buy = GameLens (player . buys)
  gold = GameLens (player . G.gold)

-- ResourceSYM should be a lens
-- IntSYM should be an Int
-- We make a statement by composing these together
class StatementSYM repr where
  modify :: repr (GameLens Int) -> (repr Int -> repr Int) -> repr ()

instance ResourceSYM (StateT Game IO (GameLens Int)) where
  action = pure action
  buy = pure buy
  gold = pure Types.gold

instance StatementSYM (StateT Game IO) where
    modify mlens mf = do (GameLens lens) <- mlens
                         next <- mf (use lens)
                         S.modify (\game -> set lens next game)
-- Don't worry if you think this is dissatisfying - so do I

plusOneAction' :: (
  ResourceSYM (repr (GameLens Int)),
  IntSYM (repr Int),
  StatementSYM repr) => repr ()
plusOneAction' = modify action (+ lit 1)


-- It works!
-- runStateT (eval plusOneAction') game
-- ((),Game {_player = Player {_gold = 2, _buys = 3, _actions = 2, _deck = [( Card "Market" "" ActionType),( Card "Village" "" ActionType),( Card "Village" "" ActionType),( Card "Market" "" ActionType),( Card "Market" "" ActionType)], _hand = [( Card "Village" "" ActionType),( Card "Village" "" ActionType),( Card "Market" "" ActionType)], _discard = []}, _others = [], _supply = [], _trash = []})

-- instance StatementSYM String where
--   modify resource f = f resource
-- This doesn't work - we need some type massaging
-- • Expected kind ‘* -> *’, but ‘String’ has kind ‘*’
-- • In the first argument of ‘StatementSYM’, namely ‘String’

-- This uses generalized newtype deriving to get the typeclass
newtype Pretty a = Pretty { _pretty :: String } deriving (Generic, Show, Eq, IntSYM, ResourceSYM)

instance Newtype (Pretty a)

instance StatementSYM Pretty where
  modify resource f = (pack . unpack . f . pack . unpack) resource
  -- TODO: over?  under?

pretty' :: Pretty a -> Pretty a
pretty' = id
-- pretty' plusOneAction'
-- Pretty {_pretty = "(action + 1)"}
