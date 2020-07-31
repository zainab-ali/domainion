{-# LANGUAGE FlexibleInstances #-}
-- We want to make an instance for Action, but Action is a type synonym
module Composition where

import Types (Pretty(..), ResourceSYM(..), StatementSYM (modify), GameLens, plusOneAction', pretty')
import Final (IntSYM(..))
import Control.Newtype.Generics (unpack, pack)
import Prelude (Int, (++), Semigroup(..))
import qualified Prelude as P ((>>))
import GameJam (Action)
import Control.Applicative (liftA2)

class ComposeSYM repr where
   compose :: repr -> repr -> repr
 -- Does this look familiar?

instance Semigroup (Pretty a) where
  x <> y = pack (unpack x ++ "\n" ++ unpack y)

instance Semigroup Action where
  (<>) = liftA2 (<>)

plusOneGold :: (
  ResourceSYM (repr (GameLens Int)),
  IntSYM (repr Int),
  StatementSYM repr) => repr ()
plusOneGold = modify gold (+ lit 1)

plusOneBuy :: (
  ResourceSYM (repr (GameLens Int)),
  IntSYM (repr Int),
  StatementSYM repr) => repr ()
plusOneBuy = modify buy (+ lit 1)

marketAction :: (
  ResourceSYM (repr (GameLens Int)),
  IntSYM (repr Int),
  StatementSYM repr,
  Semigroup (repr ())) => repr ()
marketAction = plusOneAction' <> plusOneGold <> plusOneBuy
-- It works!
-- > putStrLn (unpack (pretty' marketAction))
-- (action + 1)
-- (gold + 1)
-- (buy + 1)

-- It works too!
-- runStateT (eval marketAction) game
-- ((),Game {_player = Player {_gold = 3, _buys = 4, _actions = 2, _deck = [( Card "Market" "" ActionType),( Card "Village" "" ActionType),( Card "Village" "" ActionType),( Card "Market" "" ActionType),( Card "Market" "" ActionType)], _hand = [( Card "Village" "" ActionType),( Card "Village" "" ActionType),( Card "Market" "" ActionType)], _discard = []}, _others = [], _supply = [], _trash = []})
