module TypedJSON where

import AST
import JSONLibrary
import Control.Monad
import Data.List

import Data.Maybe


-- Tipos JSON
data JSONType
  = TyString
  | TyNum
  | TyObject (Object JSONType)
  | TyArray JSONType
  | TyBool
  | TyNull
  deriving (Show, Eq)


-- dado un valor JSON se infiere el tipo. Se devuelve
-- Nothing si el valor está mal tipado
typeOf :: JSON -> Maybe JSONType
typeOf (JString s)      = Just (TyString)
typeOf (JNumber n)      = Just (TyNum)
typeOf (JBoolean True)  = Just (TyBool)
typeOf (JBoolean False) = Just (TyBool)
typeOf (JNull)          = Just (TyNull)
typeOf (JArray arrtypeof) = res
 where res = if isJust (auxTypeOfArr arrtypeof) then Just (TyArray (fromJust (auxTypeOfArr arrtypeof))) 
                                          else Nothing
typeOf (JObject objtypeof) = resobj
 where resobj = if isJust (auxTypeOfObj objtypeof) then Just (TyObject (fromJust (auxTypeOfObj (sortKeys objtypeof)))) 
                                          else Nothing

auxTypeOfArr :: [JSON] -> Maybe JSONType
auxTypeOfArr []     = Nothing
auxTypeOfArr (x:xs) = foldl (\acc b -> if (typeOf b) == acc then acc else Nothing) (typeOf x) xs


auxTypeOfObj :: [(Key, JSON)] -> Maybe [(Key, JSONType)]
auxTypeOfObj []           = Just []
auxTypeOfObj ((k,j):xs)   = foldl (\acc (sigk, sigj) 
                            -> if typeWf (fromJust (typeOf sigj)) 
                              then Just ((fromJust $ acc) ++ [((sigk, fromJust $ typeOf sigj))])  
                              else Just []) (Just [(k, fromJust (typeOf j))]) xs


-- decide si las claves de un objeto están ordenadas
-- lexicográficamente y no se repiten.
objectWf :: Object JSONType -> Bool
objectWf  obj = isSorted (keysOfAux obj)


keysOfAux :: Object JSONType -> [Key]
keysOfAux ((k,obj):xs) = k : keysOfAux xs
keysOfAux _            = []

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x < y then isSorted (y:xs) 
                              else False

entriesOfAux :: Object JSONType -> [(Key, JSONType)]
entriesOfAux o = o

valuesOfAux :: Object JSONType -> [JSONType]
valuesOfAux ((k,obj):xs) = obj : valuesOfAux xs
valuesOfAux _            = []


-- decide si todos los tipos objeto contenidos en un tipo JSON
-- están bien formados.
typeWf :: JSONType -> Bool
typeWf (TyObject tobj) = objectWf (tobj) && all (==True) (map (typeWf) (valuesOfAux tobj))
typeWf _ = True 


-- dado un valor JSON v, y un tipo t, decide si v tiene tipo t.
hasType :: JSON -> JSONType -> Bool
hasType j jt = if isJust (typeOf j) then (fromJust $ typeOf j) == jt
                          else False

