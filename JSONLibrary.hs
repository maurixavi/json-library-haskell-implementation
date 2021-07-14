module JSONLibrary
 (lookupField,
  lookupFieldObj,
  keysOf,
  valuesOf,
  entriesOf,
  leftJoin,
  rightJoin,
  filterArray,
  insertKV,
  sortKeys,
  trJString,
  trJNumber,
  trJBoolean,
  trEveryWhere,
  mkJString, mkJNumber, mkJBoolean, mkJNull, mkJObject, mkJArray,
  fromJString, fromJNumber, fromJBoolean, fromJObject, fromJArray,
  isJString, isJNumber, isJBoolean, isJNull, isJObject, isJArray,
  JSON(),ToJSON(toJSON), FromJSON(fromJSON), importJSON
 )
where

import AST
import Data.List
import Data.Maybe


{- lookupField:
 Cuando el primer argumento es un objeto y tiene como clave el valor
 dado como segundo argumento, entonces se retorna el valor JSON
 correspondiente (bajo el constructor {\tt Just}). De lo contrario se
 retorna {\tt Nothing}. Si un objeto tiene claves repetidas, se
 retorna el valor de más a la derecha.
-}
lookupField :: JSON -> Key -> Maybe JSON
lookupField jelem key
  | (isJObject jelem) == True = lookup key $ reverse (fromJust $ fromJObject jelem)
  | otherwise                 = Nothing

-- Análoga a la anterior, pero el primer argumento es un objeto.
lookupFieldObj :: Object a -> Key -> Maybe a
lookupFieldObj obj key = lookup key $ reverse obj

-- retorna la lista de claves de un objeto, manteniendo el orden en el
-- que se encontraban.
keysOf :: Object a -> [Key]
keysOf ((k,obj):xs) = k : keysOf xs
keysOf _            = []

-- Retorna una lista con los valores contenidos en los campos de un objeto,
-- manteniendo el orden en el que se encontraban.
valuesOf :: Object a -> [a]
valuesOf ((k,obj):xs) = obj : valuesOf xs
valuesOf _            = []

-- retorna todos los campos de un objeto, en el orden en que se encontraban.
entriesOf :: Object a -> [(Key, a)]
entriesOf o = o

-- ejemplos de valores JSON
jval 1 =
    JObject
    [("employees",
     JArray [
       JObject [("firstName", JString "John"),
                ("lastName", JString "Doe")],
       JObject [("firstName", JString "Anna"),
                ("lastName", JString "Smith")],
       JObject [("firstName", JString "Peter"),
                ("lastName", JString "Jones")]
      ]),
     ("number", JNumber 12),
     ("boolean", JBoolean False),
     ("null", JNull)]

jval 2 =
    JArray [JNull,
            JObject [("key 1", JNumber 1),
                     ("key 2", JBoolean False)],
            jval 3,
            JNull ]

jval 3 = JObject [("1", JNull), ("2", JObject (obj 2))]


-- ejemplos de objetos JSON
-- bien tipado, ordenado
obj 1 = [("1", JNumber 1), ("2", JNumber 1), ("3", JNumber 1)]

-- bien tipado, no ordenado
obj 2 = [("4", JNumber 1), ("6", JNumber 1), ("5", JObject (obj 1))]

-- mal tipado
obj 3 = [("1", JNumber 1), ("3", JNumber 1), ("3", JNumber 2)]

-- otro, para testear joins
obj 4 = [("1", JNull), ("3", JNull), ("4", JNull)]


{- Cuando el primer argumento es un objeto y tiene como clave el valor dado como segundo
argumento, entonces se retorna el valor JSON correspondiente (bajo el constructor
Just). De lo contrario se retorna Nothing. Si un objeto tiene claves repetidas, se
retorna el valor de m´as a la derecha -}
leftJoin :: Object a -> Object a -> Object a
leftJoin []           []           = []
leftJoin ((k1,v1):xs) []           = ((k1,v1):xs)
leftJoin []           ((k2,v2):ys) = ((k2,v2):ys)
leftJoin ((k1,v1):xs) ((k2,v2):ys) = 
  ((k1,v1):xs) ++ rightSinRepetidos ((k1,v1):xs) ((k2,v2):ys)

rightSinRepetidos ::  Object a -> Object a -> Object a
rightSinRepetidos []           []           = []
rightSinRepetidos ((k1,v1):xs) []           = []
rightSinRepetidos []           ((k2,v2):ys) = ((k2,v2):ys)
rightSinRepetidos ((k1,v1):xs) ((k2,v2):ys)
  | isJust $ (lookupFieldObj ((k1,v1):xs) k2) = rightSinRepetidos ((k1,v1):xs) ys
  | otherwise =  [(k2,v2)] ++ rightSinRepetidos ((k1,v1):xs) ys


--Analoga a leftJoin, pero el primer argumento es un objeto
rightJoin :: Object a -> Object a -> Object a
rightJoin []           []           = []
rightJoin ((k1,v1):xs) []           = ((k1,v1):xs)
rightJoin []           ((k2,v2):ys) = ((k2,v2):ys)
rightJoin ((k1,v1):xs) ((k2,v2):ys) = 
  leftSinRepetidos ((k1,v1):xs) ((k2,v2):ys) ++ ((k2,v2):ys)

leftSinRepetidos ::  Object a -> Object a -> Object a
leftSinRepetidos []           []           = []
leftSinRepetidos ((k1,v1):xs) []           = ((k1,v1):xs)
leftSinRepetidos []           ((k2,v2):ys) = []
leftSinRepetidos ((k1,v1):xs) ((k2,v2):ys)
  | isJust $ (lookupFieldObj ((k2,v2):ys) k1) = leftSinRepetidos xs ((k2,v2):ys) 
  | otherwise =  [(k1,v1)] ++ leftSinRepetidos xs ((k2,v2):ys) 


-- Dado un predicado sobre objetos JSON, y un arreglo, construye el
-- arreglo con los elementos que satisfacen el predicado.
filterArray :: (JSON -> Bool) ->  Array -> Array
filterArray p (x:xs) 
  | p x       = x : filterArray p xs
  | otherwise = filterArray p xs
filterArray _ [] = []


-- Se inserta un campo en un objeto. Si las claves del objeto están
-- ordenadas lexicográficamente, el resultado debe conservar esta
-- propiedad.
insertKV :: (Key, v) -> Object v -> Object v
insertKV pairKV [] = [pairKV]
insertKV pairKV ((k,v):xs) = if fst pairKV <= fst (k,v)
                             then pairKV : (k,v):xs
                             else (k,v) : insertKV pairKV xs
  

-- Se inserta un campo en un objeto, al inicio
consKV :: (Key, v) -> Object v -> Object v
consKV pairKV ((key,val):xs) = pairKV : ((key,val):xs) 


-- ordena claves de un objeto
sortKeys :: Object a -> Object a
sortKeys []         = []
sortKeys ((k,v):xs) = ins (k,v) (sortKeys xs)

ins (k,v) []       = [(k,v)]
ins (k,v) ((k',v'):ys) 
  | fst(k,v)  <= fst(k',v')     = (k,v) : (k',v') : ys
  | otherwise = (k',v') : ins (k,v) ys


-- constructoras
mkJString :: String -> JSON
mkJString s = (JString s)

mkJNumber :: Integer -> JSON
mkJNumber n = (JNumber n)

mkJBoolean :: Bool -> JSON
mkJBoolean bool = (JBoolean bool)

mkJNull :: () -> JSON
mkJNull _ = (JNull)

mkJArray :: [JSON] -> JSON
mkJArray arr = (JArray arr)

mkJObject :: [(Key, JSON)] -> JSON
mkJObject pairKV = (JObject pairKV)


-- destructoras
fromJString :: JSON -> Maybe String
fromJString (JString s) = Just s
fromJString _           = Nothing

fromJNumber :: JSON -> Maybe Integer
fromJNumber (JNumber n) = Just n
fromJNumber _           = Nothing

fromJBoolean  :: JSON -> Maybe Bool
fromJBoolean (JBoolean True)  = Just True
fromJBoolean (JBoolean False) = Just False
fromJBoolean _                = Nothing

fromJObject :: JSON -> Maybe (Object JSON)
fromJObject (JObject o) = Just o
fromJObject _           = Nothing

fromJArray :: JSON -> Maybe [JSON]
fromJArray (JArray arr) = Just arr
fromJArray _            = Nothing
 

-- predicados
isJNumber :: JSON -> Bool
isJNumber (JNumber jnum) = True
isJNumber _              = False

isJNull :: JSON -> Bool
isJNull (JNull) = True
isJNull _       = False

isJString :: JSON -> Bool
isJString (JString jstr) = True
isJString _              = False

isJObject :: JSON -> Bool
isJObject (JObject jo) = True
isJObject _            = False

isJArray :: JSON -> Bool
isJArray (JArray jarr) = True
isJArray _             = False

isJBoolean :: JSON -> Bool
isJBoolean (JBoolean jb) = True
isJBoolean _             = False


-- transforma un string, según la función argumento.
-- Si el objeto no es un string, no cambia
trJString :: (String -> String) -> JSON -> JSON
trJString f jvalue
  | (isJString jvalue) == True = mkJString $ f (fromJust (fromJString jvalue)) 
  | otherwise                  = jvalue


-- idem para números
trJNumber :: (JSONNumber -> JSONNumber) -> JSON -> JSON
trJNumber f jvalue
  | (isJNumber jvalue) == True = mkJNumber $ f (fromJust (fromJNumber jvalue)) 
  | otherwise                  = jvalue


-- idem para booleanos
trJBoolean :: (Bool -> Bool) -> JSON -> JSON
trJBoolean f jvalue
  | (isJBoolean jvalue) == True = mkJBoolean $ f (fromJust (fromJBoolean jvalue)) 
  | otherwise                   = jvalue


-- transforma *todas* las ocurrencias de strings, números y booleanos
-- aplicandoles la función pasada como argumento.
trEveryWhere :: (JSON -> JSON) -> JSON -> JSON
trEveryWhere f jvalue 
  | (isJBoolean jvalue) = f jvalue 
  | (isJNumber  jvalue) = f jvalue 
  | (isJString  jvalue) = f jvalue 
  | (isJObject  jvalue) = JObject (map ( \(k,v) -> (k, trEveryWhere f v)) (entriesOf (fromJust $ fromJObject jvalue)))
  | (isJArray   jvalue) = JArray (map (trEveryWhere f) (fromJust $ fromJArray jvalue))
  | otherwise = jvalue
