module Optional where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend Nada Nada = Nada
  mappend (Only a1) (Only a2) = Only $ mappend a1 a2
