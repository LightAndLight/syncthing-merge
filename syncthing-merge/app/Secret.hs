{-# LANGUAGE DataKinds #-}

module Secret (Secret, protect, leak) where

import GHC.TypeLits (ErrorMessage (..), TypeError)

newtype Secret a = Secret a deriving (Eq)

protect :: a -> Secret a
protect = Secret

leak :: Secret a -> a
leak (Secret a) = a

instance TypeError ('Text "'show' cannot be used on secrets") => Show (Secret a) where
  show = undefined
