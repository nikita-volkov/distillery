module Distillery.Refine where

import Distillery.Prelude hiding (lookup)

-- *

refine :: Refine i o -> i -> Either (Text, [String]) o
refine =
  error "TODO"

-- *

newtype Refine i o
  = -- |
    -- Function from input into either an error with a stack of contextual inputs
    -- or a successful refinement.
    -- Contextual inputs are introduced using 'locally'.
    Refine (i -> Either (Text, [String]) o)
  deriving
    (Functor, Applicative, Monad, Alternative, MonadPlus)
    via (ExceptT (Text, [String]) ((->) i))
  deriving
    (Profunctor, Strong, Cochoice, Choice, Traversing, Category)
    via (Star (Either (Text, [String])))

deriving via (Either (Text, [String])) instance Sieve Refine (Either (Text, [String]))

locally :: Show i => i -> Refine i o -> Refine i' o
locally i (Refine run) =
  Refine $ \_ ->
    case run i of
      Left (msg, stack) -> Left (msg, show i : stack)
      Right r -> Right r
