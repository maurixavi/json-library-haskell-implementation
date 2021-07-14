module Examples where

import JSONLibrary
import TypedJSON

import Data.Maybe


instance FromJSON Integer where
  fromJSON i = fromJNumber i

instance ToJSON Integer where
  toJSON i   = mkJNumber i

instance FromJSON Bool where
  fromJSON b = fromJBoolean b

instance ToJSON Bool where
  toJSON b   = mkJBoolean b

instance FromJSON a => FromJSON [a] where
  fromJSON j = if isJust $ (fromJArray j) then Just $ res 
                                        else Nothing
    where res =  map (fromJust) (map (fromJSON) (fromJust $ fromJArray j))


instance ToJSON a => ToJSON [a] where
  toJSON jarr = mkJArray res
    where res = (map (toJSON) jarr)


instance FromJSON () where
  fromJSON n | isJNull n = Just ()
             | otherwise = Nothing

instance ToJSON () where
  toJSON   = mkJNull 


data BinTree a
    = Fork {l :: BinTree a, r :: BinTree a}
    | Leaf {val :: a}
    deriving (Show, Eq)

{-
La notación anterior es equivalente a definir:

data BinTree a
    = Fork (BinTree a)(BinTree a)
    | Leaf a
    deriving (Show, Eq)

l :: BinTree a -> BinTree a
l (Fork l _) = l

r :: BinTree a -> BinTree a
r (Fork _ r) = r

val :: BinTree a -> BinTree a
val (Leaf a) = a

-}

instance (FromJSON a) => FromJSON (BinTree a) where
    fromJSON j = if (isJObject j) then  
                    let l = entriesOf (fromJust $ fromJObject j) in
                    case l of
                        [("l",a),("r",b)] -> case fromJSON a of
                                                  Nothing -> Nothing
                                                  Just a' -> case fromJSON b of
                                                                Nothing -> Nothing
                                                                Just b' -> Just $ Fork a' b'
                        
                        [("r",b),("l",a)] -> case fromJSON b of
                                                  Nothing -> Nothing
                                                  Just b' -> case fromJSON a of
                                                                Nothing -> Nothing
                                                                Just a' -> Just $ Fork a' b'

                        [("val",a)] -> case fromJSON a of
                                            Just a -> Just $ Leaf a
                                            Nothing -> Nothing
                        _ -> Nothing

                else Nothing



instance ToJSON a => ToJSON (BinTree a) where
    toJSON (Leaf a) = mkJObject ([("val", toJSON (a))])
    toJSON (Fork l r) = mkJObject ([("l", toJSON l), ("r", toJSON r)])

-- decide si un valor que representa un estudiante esta bien formado
estBienFormado :: JSON -> Bool
estBienFormado jelem = if (typeOf (jelem)) == Just(TyObject [("CI", TyNum), ("apellido", TyString), ("cursos",   TyArray tyCurso), ("nombre",   TyString)]) then True else False
  where
      tyCurso =
        TyObject [("anio",     TyNum),
                  ("codigo",   TyNum),
                  ("nombre",   TyString),
                  ("nota",     TyNum),
                  ("semestre", TyNum)]


-- dado un valor JSON que representa a un estudiante, retorna su cédula
ci :: JSON -> Maybe Integer
ci jelem = fromJNumber (fromJust $ lookupField jelem "CI")


-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados jelem = if isJust (lookupField jelem "cursos") 
                  then Just $ mkJArray $ (filterArray (isAprobado') (fromJust $ fromJArray $ fromJust $ (lookupField jelem "cursos"))) 
                  else Nothing

isAprobado' :: JSON -> Bool
isAprobado' jelem = (fromJust $ fromJNumber (fromJust $ lookupField jelem "nota"))  >= 3


-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio anio jelem = if isJust (lookupField jelem "cursos") 
                  then Just $ mkJArray $ (filterArray (isAnio' anio) (fromJust $ fromJArray $ fromJust $ (lookupField jelem "cursos"))) 
                  else Nothing

isAnio' :: Integer -> JSON -> Bool
isAnio' anio jcurso = (fromJust $ fromJNumber (fromJust $ lookupField jcurso "anio")) == anio


-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad jelem = if isJust (lookupField jelem "cursos") 
                            then Just ( (fromIntegral $ sum $ map (getNotaCurso) (fromJust $ fromJArray $ fromJust $ (lookupField jelem "cursos"))) / (fromIntegral (length (fromJust $ fromJArray $ fromJust $ (lookupField jelem "cursos"))))  :: Float )
                            else Nothing 

getNotaCurso jcurso = fromJust $ fromJNumber (fromJust $ lookupField jcurso "nota") 