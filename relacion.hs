data Relacion = MasGrande
			  | MasChico
			  | Igual
			  deriving (Eq, Show)

esIgual :: Relacion -> Bool
esIgual MasGrande = False
esIgual MasChico = False
esIgual _ = True

esMasGrande :: Relacion -> Bool
esMasGrande MasGrande = True
esMasGrande _ = False

esMasGrandeIgual :: Relacion -> Bool
esMasGrandeIgual MasGrande = True
esMasGrandeIgual Igual = True
esMasGrandeIgual _ = False

esMasChico :: Relacion -> Bool
esMasChico MasChico = True
esMasChico _ = False

esMasChicoIgual :: Relacion -> Bool
esMasChicoIgual MasChico = True
esMasChicoIgual Igual = True
esMasChicoIgual _ = False

comparar :: (Ord a) => a -> a -> Relacion
comparar a b | a == b = Igual
comparar a b | a > b = MasGrande
comparar a b | a < b = MasChico
