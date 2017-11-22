-- Sum type
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

-- Product type
data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

-- Pattern matches over the WherePenguinsLive data constructors
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

-- Pulls the WherePenguinsLive data constructor from the Penguin product type
gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng origin) = origin

humboldt :: Penguin
humboldt = Peng SouthAmerica

gentoo :: Penguin
gentoo = Peng Antarctica

macaroni :: Penguin
macaroni = Peng Antarctica

little :: Penguin
little = Peng Australia

galapagos :: Penguin
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
     (galapagosPenguin p)
  || (antarcticPenguin p)
