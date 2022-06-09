module Distillery.Refine where

import Distillery.Prelude hiding (lookup)

-------------------------

refine :: Refine i o -> i -> Either (Text, [Text]) o
refine =
  error "TODO"

-------------------------

newtype Refine i o
  = -- |
    -- Function from input into either an error with a stack of contextual inputs
    -- or a successful refinement.
    -- Contextual inputs are introduced using 'locally'.
    Refine (i -> Either (Text, [Text]) o)
  deriving
    (Functor, Applicative, Monad, Alternative, MonadPlus)
    via (ExceptT (Text, [Text]) ((->) i))
  deriving
    (Profunctor, Strong, Cochoice, Choice, Traversing, Category)
    via (Star (Either (Text, [Text])))

deriving via (Either (Text, [Text])) instance Sieve Refine (Either (Text, [Text]))

locally :: Text -> i -> Refine i o -> Refine i' o
locally contextLabel i (Refine run) =
  Refine $ \_ ->
    case run i of
      Left (msg, stack) -> Left (msg, contextLabel : stack)
      Right r -> Right r
