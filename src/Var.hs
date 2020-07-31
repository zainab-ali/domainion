{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
-- We want to use forall quantification
{-# LANGUAGE QuantifiedConstraints #-} -- Required for using forall quantification in Int

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- Required for constructing newtypes
{-# LANGUAGE FlexibleInstances #-}
-- We want to make an instance for String, but String is a type synonym
module Var where

import Predicate
import GameJam (CardType(..), Card(..), Game)
import Types(GameLens(..), Pretty(..))
import qualified Types as T (pretty')
import Final (IntSYM(..))
import Prelude (Bool, Int, (.), String, (++), show, ($), (<>), id, const, IO, filter, fromInteger, Monad, flip, pure, putStrLn)
import qualified Prelude as P
import Control.Newtype.Generics (unpack, pack, Newtype, over, under, over2)
import Control.Lens.Tuple
import Control.Monad.Trans.Class (lift)
import Control.Monad.Indexed.State
import Control.Monad.Indexed
import Control.Monad.Indexed.Trans (ilift)
import Data.List (delete)
import GHC.Generics (Generic)
import Control.Lens (Lens', makeLenses, use, (%=), (^.), set)
import qualified Control.Lens as L (over)

-- Mine: a card costing up to 3 more...
-- We need our predicate to take in not one card, but two

-- This needs to be well typed
--
-- something like put (put (take (take (take treasure))))
-- we can already take a treasure card
--
takeATreasure :: (
  MoveSYM' repr,
  PileSYM (repr (GameLens [Card]))) => repr ()
takeATreasure = put' (PileSYMSelf trash) (pick' (CardPSelf (cardType == liftCardType Treasure)) (PileSYMSelf hand))


-- Each time we want to add something to the stack.  Our predicate can refer to anything in the stack of cards
-- If we attempt to use a predicate on a stack that is too small, we should get a compile error
--
-- Let's model the stack as a type m`

data CardPSelf' stack = CardPSelf' (forall p. CardP' p => p stack Bool)

class MotionSYM'' repr where
  pick'' :: CardPSelf' (Card, m) -> PileSYMSelf -> repr m a -> repr (Card, m) a
  put'' :: PileSYMSelf -> repr (Card, m) a -> repr m a

class CardSYM' repr where
  value' :: repr Card -> repr Int
  cost' ::  repr Card -> repr Int
  cardType' :: repr Card -> repr CardType
  liftCardType' :: CardType -> repr CardType

class VarSYM repr where
  z :: repr (m, h) m
  s :: repr m a -> repr (any, m) a

class (forall m. CardSYM' (repr m),
       forall m. BoolSYM (repr m),
       forall m. IntSYM (repr m Int),
       VarSYM repr) => CardP' repr

minePredicate :: CardP' repr => repr (Card, (Card, m)) P.Bool
minePredicate = cost' z < (cost' (s z) + lit (fromInteger 4))

mine :: MotionSYM'' repr => repr m () -> repr m ()
mine =
  put'' (PileSYMSelf trash) .
  put'' (PileSYMSelf hand) .
  (pick'' (CardPSelf' (cost' z < (cost' (s z) + lit 4))) (PileSYMSelf supply)) .
  (pick'' (CardPSelf' (cardType' z == liftCardType' Treasure)) (PileSYMSelf hand))

newtype (Pretty' m a) = Pretty' String deriving (Generic)

instance Newtype (Pretty' m a)

instance VarSYM Pretty' where
  z = Pretty' "z"
  s = over pack ("s" ++)

instance IntSYM (Pretty' m Int) where
  lit = Pretty' . show
  x + y = pack $ "(" <> unpack x <> " + " <> unpack y <> ")"

instance BoolSYM (Pretty' m) where
  true = pack "true"
  not = over pack (\x -> "(not " ++ x ++ ")")
  (&&)  = over2 pack (\x -> \y -> "(" ++ x ++ " and " ++ y ++ ")")
  (||)  = over2 pack (\x -> \y -> "(" ++ x ++ " or " ++ y ++ ")")
  (<)  = over2 pack (\x -> \y -> "(" ++ x ++ " < " ++ y ++ ")")
  (==)  = over2 pack (\x -> \y -> "(" ++ x ++ " == " ++ y ++ ")")

-- instances for all the other Pretty' things
instance CardSYM' (Pretty' m) where
  value' = over pack ("value of " ++)
  cost' = over pack ("cost of " ++)
  cardType' = over pack ("card type of " ++)
  liftCardType' = pack . show

instance CardP' Pretty'


instance MotionSYM'' Pretty' where
  pick'' (CardPSelf' p) (PileSYMSelf pile) prev = pack $ "(" <> unpack prev <> "then (pick a card satisfying " <>  unpack (pretty' p)  <> " from " <> unpack (T.pretty' pile) <> "))"
  put'' (PileSYMSelf pile) card = pack $ "(put " <> unpack card <> " into " <> unpack (T.pretty' pile) <> ")"

pretty' :: Pretty' m a -> Pretty' m a
pretty' = id

prettyMine = pretty' (mine (Pretty' ""))

instance VarSYM (->) where
  z (m, _) = m
  s f (_, m) = f m

instance CardSYM' ((->) m) where
  value' = (_value .)
  cost' = (_cost .)
  cardType' = (_cardType .)
  liftCardType'  = const

instance CardP' (->)

-- Is this an indexed state monad?  But the state itself doesn't change during the computation
data Eval m a = Eval { _eval :: IxStateT IO Game (Game, m) a }

return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) :: (Monad m) =>  IxStateT m i j a -> (a -> IxStateT m j k b) -> IxStateT m i k b
(>>=) = flip ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = ibind (\_ -> w) v

instance MotionSYM'' Eval where
  pick'' (CardPSelf' p) (PileSYMSelf pileSyntax) prev =
    let (GameLens pile) = pileSyntax
        state = do a <- _eval prev
                   cards <- igets (^. (_1.pile))
                   stack <- igets (^. _2)
                   let selection = filter (\card -> p (card, stack)) cards
                   card <- ilift (playerPicksFrom selection)
                   ilift $ putStrLn ("deleting " ++ show card)
                   imodify (L.over (_1.pile) (delete card) )
                   imodify (L.over _2 (\prev -> (card, prev)))
                   return a
    in Eval state
  put'' (PileSYMSelf pileSyntax) prev =
    let (GameLens pile) = pileSyntax
        state = do a <- _eval prev
                   (card, stack) <- igets (^. _2)
                   imodify (L.over (_1.pile) (card :))
                   imodify (set _2 stack)
                   return a
    in Eval state

eval' :: Eval m a -> Eval m a
eval' = id

evalMine =
  let start = ireturn () >> imodify (\game -> (game, ()))
  in eval' (mine (Eval start))

-- runIxStateT (_eval evalMine) game
-- ((),(Game {_player = Player {_gold = 2, _buys = 3, _actions = 1, _deck = [( Card "Market" "" ActionType 0 5),( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5),( Card "Market" "" ActionType 0 5)], _hand = [( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5)], _discard = []}, _others = [], _supply = [( Card "Market" "" ActionType 0 5)], _trash = [( Card "Copper" "" Treasure 1 3)]},()))
-- It works!
-- ((),(Game {_player = Player {_gold = 2, _buys = 3, _actions = 1, _deck = [( Card "Market" "" ActionType 0 5),( Card "Village" "" ActionType 0 3),( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5),( Card "Market" "" ActionType 0 5)], _hand = [( Card "Village" "" ActionType 0 3),( Card "Market" "" ActionType 0 5)], _discard = []}, _others = [], _supply = [( Card "Market" "" ActionType 0 5)], _trash = [( Card "Copper" "" Treasure 1 3)]},()))
