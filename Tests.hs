module Tests where

import AST
import JSONLibrary
import TypedJSON
import Examples

import Data.Maybe (fromJust)


data TestRes = Ok | Fail Message deriving (Show, Eq)

type Message = String

assertEq m a b = if a == b then Ok else Fail m

data FuncsToTest
  = Show'      | LookupField | LookupFieldObj | KeysOf   | ValuesOf | EntriesOf
  | LeftJoin   | RightJoin   | FilterArray    | InsertKV | SortKeys | TrJString
  | TrJBoolean | TrJNumber   | TrEveryWhere   | TypeWf   | ObjectWf | TypeOf
  | HasType    | ToJSON      | FromJSON       | EstBienFormado      | CI
  | Aprobados  | EnAnio      | PromedioEscolaridad
  deriving Enum


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


-- ejemplos de tipos
typ 1 = TyObject
        [ ("boolean", TyBool)
        , ( "employees"
              , TyArray (TyObject [("firstName", TyString),
                                   ("lastName", TyString)]))
        , ("null", TyNull)
        , ("number", TyNum)
       ]

typ 2 = TyObject [("1", TyNum), ("2", TyNum), ("3", TyNum)]


-- tipos (de objetos)
-- ok
typObj 1 = [("1", TyNull),("2", TyArray TyNull),("4", TyNull)]

-- objectWf pero no se cumple `typeWf (Tybject (typObj 2))`
typObj 2 =
  [("1", TyNull),("2", TyArray TyNull),
   ("4", TyObject [("2", TyNull), ("1", TyNull)])]

-- mal formados
typObj 3 = [("1", TyNull),("2", TyArray TyNull),("2", TyNull)]
typObj 4 = [("1", TyNull),("4", TyArray TyNull),("2", TyNull)]


-- ejemplo estudiante
est 1 =
  (JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos"
       , JArray
           [ JObject
               [ ("nombre", JString "Calculo DIV")
               , ("codigo", JNumber 123)
               , ("anio", JNumber 2019)
               , ("semestre", JNumber 1)
               , ("nota", JNumber 1)
               ]
           , JObject
               [ ("nombre", JString "Calculo DIV")
               , ("codigo", JNumber 123)
               , ("anio", JNumber 2019)
               , ("semestre", JNumber 2)
               , ("nota", JNumber 7)
               ]
           , JObject
               [ ("nombre", JString "Calculo DIVV")
               , ("codigo", JNumber 124)
               , ("anio", JNumber 2020)
               , ("semestre", JNumber 1)
               , ("nota", JNumber 2)
               ]
           , JObject
               [ ("nombre", JString "Programación 1")
               , ("codigo", JNumber 130)
               , ("anio", JNumber 2019)
               , ("semestre", JNumber 2)
               , ("nota", JNumber 12)
               ]
           , JObject
               [ ("nombre", JString "Programación 2")
               , ("codigo", JNumber 130)
               , ("anio", JNumber 2020)
               , ("semestre", JNumber 1)
               , ("nota", JNumber 0)
               ]
           , JObject
               [ ("nombre", JString "Programación 2")
               , ("codigo", JNumber 130)
               , ("anio", JNumber 2021)
               , ("semestre", JNumber 1)
               , ("nota", JNumber 0)
               ]
           ])
     ])

est 2 =
  (JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)])



{- TESTS -}

tests LookupField
  = assertEq "lookupField"
    (lookupField (jval 1) "number")
    (Just (JNumber 12))

  : assertEq "lookupField"
    (lookupField (mkJObject (obj 2)) "6")
    (Just (JNumber 1))

  : assertEq "lookupField"
    (lookupField (mkJObject (obj 2)) "1")
    Nothing

  : assertEq "lookupField"
    (lookupField (mkJNull ()) "1")
    Nothing

  : assertEq "lookupField"
    (lookupField (mkJBoolean True) "1")
    Nothing

  : assertEq "lookupField"
    (lookupField (mkJArray []) "1")
    Nothing

  : assertEq "lookupField"
    (lookupField (mkJObject (obj 3)) "3")
    (Just (JNumber 2))

  : []

tests LookupFieldObj
  = assertEq "lookupFieldObj"
    (lookupFieldObj (obj 2) "6")
    (Just (JNumber 1))

  : assertEq "lookupFieldObj"
    (lookupFieldObj (obj 2) "1")
    Nothing
    
  : assertEq "lookupFieldObj"
    (lookupFieldObj [("1", TyNull)] "1")
    (Just TyNull)

  : []

tests KeysOf
  = assertEq "keysOf"
    (keysOf (obj 1))
    ["1", "2", "3"]
    
   : assertEq "keysOf"
     (keysOf (obj 2))
     ["4", "6", "5"]
     
   : assertEq "keysOf"
     (keysOf (obj 3))
     ["1", "3", "3"]

     : []

tests ValuesOf
  = assertEq "valuesOf"
    (valuesOf (obj 1))
    (take 3 (repeat (JNumber 1)))

  : assertEq "valuesOf"
    (valuesOf (obj 2) !! 2)
    (JObject (obj 1))

  : assertEq "valuesOf"
    (valuesOf (obj 3))
    (map JNumber [1,1,2])

  : []

tests EntriesOf
  = [assertEq ("entriesOf" ++ show i) (zip (keysOf (obj i)) (valuesOf (obj i)))
                                      (entriesOf (obj i))
    | i <- [1,2,3]]


--para leftJoin (resp. rightJoin) no se testerán claves repetidas en
--el segundo (resp. primer) argumento que no esten ya en otro
--(en ese caso está bien borrar repeticiones o no hacerlo, dado que la
--función está subespecificada).
tests LeftJoin
  = assertEq "leftJoin 1"
       (leftJoin (obj 3)(obj 4))
       [("1",JNumber 1),("3",JNumber 1),("3",JNumber 2),("4",JNull)]

  : assertEq "leftJoin 2"
       (leftJoin (obj 1)(obj 2))
        [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1),
         ("4",JNumber 1),("6",JNumber 1),("5",JObject (obj 1))]

  : assertEq "leftJoin 3"
       (leftJoin (obj 1)(obj 3))
       [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1)]

  : assertEq "leftJoin 4"
       (leftJoin (obj 3)(obj 1))
       [("1",JNumber 1),("3",JNumber 1),("3",JNumber 2),("2",JNumber 1)]

  : assertEq "leftJoin 5"
       (leftJoin (obj 3) [])
       [("1",JNumber 1),("3",JNumber 1),("3",JNumber 2)]

  : assertEq "leftJoin 5"
       (leftJoin [] (obj 1))
       [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1)]
  
  : []

tests RightJoin
  = assertEq "rightJoin 1"
     (rightJoin (obj 3) (obj 4))
     [("1",JNull),("3",JNull),("4",JNull)]

  : assertEq "rightJoin 2"
     (rightJoin (obj 4) (obj 3))
     [("4",JNull),("1",JNumber 1),("3",JNumber 1),("3",JNumber 2)]

  : assertEq "rightJoin 3"
     (rightJoin ([]) (obj 3))
     [("1",JNumber 1),("3",JNumber 1),("3",JNumber 2)]

  : assertEq "rightJoin 4"
     (rightJoin (obj 1) [])
     [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1)]

  : []

tests FilterArray
  = assertEq "filterArray 1"
     (filterArray (isJNull) ((fromJust . fromJArray) (jval 2)))
     [JNull,JNull]

  : assertEq "filterArray 2"
     (filterArray (const False) [])
     []

  : assertEq "filterArray 3"
     (filterArray (const True) ((fromJust . fromJArray) (jval 2)))
     ((fromJust . fromJArray) (jval 2))

  : assertEq "filterArray 4"
     (filterArray (const False) ((fromJust . fromJArray) (jval 2)))
     []

  : []

tests InsertKV
  = assertEq "insertKV 1"
    (insertKV ("4", JNull) (obj 1))
    [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1),("4",JNull)]

  : assertEq "insertKV 2"
     (insertKV ("5", JNull) (obj 2))
     [("4",JNumber 1),("5",JNull),("6",JNumber 1),("5", JObject (obj 1))]

  : assertEq "insertKV 3"
     (insertKV ("1", JNull) (obj 2))
     [("1",JNull),("4",JNumber 1),("6",JNumber 1),("5",JObject (obj 1))]

  : []

tests SortKeys
  = assertEq "sortKeys 1"
     (sortKeys $ obj 1)
     (obj 1)

  : assertEq "sortKeys 2"
     (sortKeys $ obj 3)
     (obj 3)

  : assertEq "sortKeys 3"
     (sortKeys $ obj 2)
     [("4", JNumber 1), ("5", JObject (obj 1)), ("6", JNumber 1) ]

  : []

tests TrJString
  = assertEq "trJString 1"
     (trJString ('_':) (JString "hola"))
     (JString "_hola")

  : assertEq "trJString 2"
     (trJString ('_':) (JBoolean True))
     (JBoolean True)

  : assertEq "trJString 3"
     (trJString ('_':) (JArray []))
     (JArray [])

  : assertEq "trJString 4"
     (trJString ('_':) (JNumber 42))
     (JNumber 42)

  : assertEq "trJString 5"
     (trJString ('_':) JNull)
     JNull

  : assertEq "trJString 6"
    (trJString ('_':) (jval 1))
    (jval 1)

  : []

tests TrJBoolean
  = assertEq "trJBoolean 1"
     (trJBoolean not (JString "hola"))
     (JString "hola")

  : assertEq "trJBoolean 2"
     (trJBoolean not (JBoolean True))
     (JBoolean False)

  : assertEq "trJBoolean 3"
     (trJBoolean not (JArray []))
     (JArray [])

  : assertEq "trJBoolean 4"
     (trJBoolean not (JNumber 42))
     (JNumber 42)

  : assertEq "trJBoolean 5"
     (trJBoolean not JNull)
     JNull

  : assertEq "trJBoolean 6"
     (trJBoolean not (jval 1))
     (jval 1)

  : []

tests TrJNumber
  = assertEq "trJNumber 1"
     (trJNumber (+1) (JString "hola"))
     (JString "hola")
  
  : assertEq "trJNumber 2"
     (trJNumber (+1) (JBoolean True))
     (JBoolean True)

  : assertEq "trJNumber 3"
     (trJNumber (+1) (JArray []))
     (JArray [])

  : assertEq "trJNumber 4"
     (trJNumber (+1) (JNumber 42))
     (JNumber 43)

  : assertEq "trJNumber 5"
     (trJNumber (+1) JNull)
     JNull

  : assertEq "trJNumber 6"
     (trJNumber (+1) (jval 1))
     (jval 1)

  : []

tests TrEveryWhere
  = assertEq "trEveryWhere 1"
     (trEveryWhere (const JNull) (JString "hola"))
     JNull

  : assertEq "trEveryWhere 2"
     (trEveryWhere (const JNull) (JBoolean True))
     JNull

  : assertEq "trEveryWhere 3"
     (trEveryWhere (const JNull) (JArray []))
     (JArray [])

  : assertEq "trEveryWhere 4"
     (trEveryWhere (const JNull) (JNumber 42))
     JNull

  : assertEq "trEveryWhere 5"
     (trEveryWhere (const (JNumber 22)) JNull)
     JNull

  : assertEq "trEveryWhere 6"
     (trEveryWhere (const JNull) (jval 1))
     (JObject [ ( "employees"
                , JArray
                  [ JObject [("firstName", JNull), ("lastName", JNull)]
                  , JObject [("firstName", JNull), ("lastName", JNull)]
                  , JObject [("firstName", JNull), ("lastName", JNull)]
                  ])
              , ("number", JNull)
              , ("boolean", JNull)
              , ("null", JNull)
              ])

  : assertEq "trEveryWhere 7"
     (trEveryWhere (trJString ("_"++)) (jval 1))
     (JObject
       [("employees",
          JArray [
            JObject [("firstName", JString "_John"),
                     ("lastName", JString "_Doe")],
            JObject [("firstName", JString "_Anna"),
                     ("lastName", JString "_Smith")],
            JObject [("firstName", JString "_Peter"),
                     ("lastName", JString "_Jones")]
            ]),
        ("number", JNumber 12),
        ("boolean", JBoolean False),
        ("null", JNull)])

  : assertEq "trEveryWhere 8"
     (trEveryWhere (trJNumber (+1)) (jval 1))
     (JObject
       [("employees",
          JArray [
            JObject [("firstName", JString "John"),
                     ("lastName", JString "Doe")],
            JObject [("firstName", JString "Anna"),
                     ("lastName", JString "Smith")],
            JObject [("firstName", JString "Peter"),
                     ("lastName", JString "Jones")]
            ]),
        ("number", JNumber 13),
        ("boolean", JBoolean False),
        ("null", JNull)])

  : assertEq "trEveryWhere 9"
     (trEveryWhere (trJBoolean not) (jval 1))
     (JObject
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
        ("boolean", JBoolean True),
        ("null", JNull)])
  : []

tests TypeWf
 = assertEq "typeWf 1"
    (typeWf (typ 1))
    True

 : assertEq "typeWf 2"
    (typeWf (typ 2))
    True

 : assertEq "typeWf 3"
    (typeWf $ TyObject (typObj 1))
    True

 : assertEq "typeWf 4"
    (typeWf $ TyObject (typObj 2))
    False
 : assertEq "typeWf 5"
    (typeWf $ TyObject (typObj 3))
    False

 : assertEq "typeWf 6"
    (typeWf $ TyObject (typObj 4))
    False

 : assertEq "typeWf 7"
   (typeWf (TyObject (typObj 2)))
   False

 : assertEq "typeWf 8"
   (typeWf (TyObject []))
   True

 : []

tests ObjectWf
 = assertEq "objectWf 1"
    (objectWf (typObj 1))
    True

 : assertEq "objectWf 2"
    (objectWf (typObj 2))
    True

 : assertEq "objectWf 3"
    (objectWf (typObj 3))
    False

 : assertEq "objectWf 4"
    (objectWf (typObj 4))
    False

 : assertEq "objectWf 5"
    (objectWf [])
    True

 : []

tests TypeOf
  = assertEq "typeOf 1"
     (typeOf (jval 1))
     (Just $ typ 1)

  : assertEq "typeOf 2"
     (typeOf (JObject $ obj 1))
     (Just $ typ 2)

  : assertEq "typeOf 3"
    (typeOf (JObject []))
    (Just $ TyObject [])

  : assertEq "typeOf 4"
     (typeOf (jval 2))
     Nothing
  : assertEq "typeOf 5"
     (typeOf (est 1))
     (Just tyEstudiante)
  : []
    where
        tyEstudiante =
          TyObject [ ("CI",       TyNum),
                     -- notar que 'C'<'a' en el orden de Char
                     ("apellido", TyString),
                     ("cursos",   TyArray tyCurso),
                     ("nombre",   TyString)]
        tyCurso =
          TyObject [("anio",     TyNum),
                    ("codigo",   TyNum),
                    ("nombre",   TyString),
                    ("nota",     TyNum),
                    ("semestre", TyNum)]

tests HasType
  = assertEq "hasType 1"
    (hasType (jval 1) (typ 1)) True

  : assertEq "hasType 2"
    (hasType (jval 2) (TyArray TyNull)) False

  : assertEq "hasType 3"
    (hasType (JArray []) (TyArray TyNull)) False

  :[]


tests ToJSON
  = assertEq "toJSON 1"
     (toJSON (3 :: Integer))
     (JNumber 3)

  : assertEq "toJSON 2"
     (toJSON True)
     (JBoolean True)

  : assertEq "toJSON 3"
     (toJSON ())
     JNull

  : assertEq "toJSON 4"
     (toJSON [2,3,4 :: Integer])
     (JArray [JNumber 2,
              JNumber 3,
              JNumber 4])
     
  : assertEq "toJSON 5"
     (toJSON [True])
     (JArray [JBoolean True])

  : assertEq "toJSON 6"
     (toJSON (Leaf (4 :: Integer)))
     (JObject [("val", JNumber 4)])

  : assertEq "toJSON 7"
     (toJSON (Leaf [True]))
     (JObject [("val", JArray [JBoolean True])])

  : assertEq "toJSON 8"
     (toJSON (Fork (Leaf ())(Leaf ())))
     (JObject [("l", JObject [("val", JNull)]),
               ("r", JObject [("val", JNull)])])

  : assertEq "toJSON 9"
     (toJSON (Fork (Leaf (3 ::Integer))
               (Fork (Fork (Leaf 4)
                       (Leaf 5))
                 (Leaf 0))))
     (JObject
       [ ("l", JObject [("val", JNumber 3)])
       , ( "r"
         , JObject
           [ ( "l"
             , JObject
               [ ("l", JObject [("val", JNumber 4)])
               , ("r", JObject [("val", JNumber 5)])
               ])
           , ("r", JObject [("val", JNumber 0)])
           ])
       ])
  
  : []

tests FromJSON
  = assertEq "fromJSON 1"
     (fromJSONU JNull)
     (Just ())

  : assertEq "fromJSON 2"
     (fromJSONI (JNumber 3))
     (Just 3)

  : assertEq "fromJSON 3"
     (fromJSONLI (JArray []))
     (Just [])

  : assertEq "fromJSON 4"
     (fromJSONLI (JArray [JNumber 1, JNumber 2]))
     (Just [1,2])

  : assertEq "fromJSON 5"
     (fromJSONBTI (JObject
                    [("l", JObject [("val", JNumber 3)]),
                     ("r", JObject [("val", JNumber 4)])]))
     (Just (Fork (Leaf 3) (Leaf 4)))

  : assertEq "fromJSON 6"
     (fromJSONBTI (JObject
                    [("r", JObject [("val", JNumber 4)]),
                     ("l", JObject [("val", JNumber 3)])]))
     (Just (Fork (Leaf 3) (Leaf 4)))

  : assertEq "fromJSON 7"
     (fromJSONBTI (JObject [("val", JNumber 5)]))
     (Just (Leaf 5))

  : assertEq "fromJSON 8"
     (fromJSONBTI (JObject [("v", JNumber 5)]))
     Nothing

  : assertEq "fromJSON 9"
     (fromJSONBTI (JObject
                   [("", JObject [("val", JNumber 4)]),
                    ("l", JObject [("val", JNumber 3)])]))
     Nothing
  
  : []
  where
    fromJSONBTI = fromJSON :: JSON -> Maybe (BinTree Integer)
    fromJSONI   = fromJSON :: JSON -> Maybe Integer
    fromJSONU   = fromJSON :: JSON -> Maybe ()
    fromJSONLI  = fromJSON :: JSON -> Maybe [Integer]

tests EstBienFormado
  = assertEq "estBienFormado 1"
     (estBienFormado (est 1))
     True

  : assertEq "estBienFormado 2"
     (estBienFormado (jval 1))
     False

  : assertEq "estBienFormado 3"
     (estBienFormado (jval 2))
     False

  : assertEq "estBienFormado 4"
     (estBienFormado (est 2))
     False

  : []

tests CI
  = assertEq "ci 1"
     (ci (est 1))
     (Just 12345678)

  : []

tests Aprobados
  = assertEq "aprobados 1"
     (aprobados (est 1))
     (Just
       (JArray
         [ JObject
           [ ("nombre", JString "Calculo DIV")
           , ("codigo", JNumber 123)
           , ("anio", JNumber 2019)
           , ("semestre", JNumber 2)
           , ("nota", JNumber 7)
           ]
         , JObject
           [ ("nombre", JString "Programación 1")
           , ("codigo", JNumber 130)
           , ("anio", JNumber 2019)
           , ("semestre", JNumber 2)
           , ("nota", JNumber 12)
           ]
         ])
     )

  : []

tests EnAnio
  = assertEq "enAnio 1"
    (enAnio 2019 (est 1))
    (Just
      (JArray
        [ JObject
          [ ("nombre", JString "Calculo DIV")
          , ("codigo", JNumber 123)
          , ("anio", JNumber 2019)
          , ("semestre", JNumber 1)
          , ("nota", JNumber 1)
          ]
        , JObject
          [ ("nombre", JString "Calculo DIV")
          , ("codigo", JNumber 123)
          , ("anio", JNumber 2019)
          , ("semestre", JNumber 2)
          , ("nota", JNumber 7)
          ]
        , JObject
          [ ("nombre", JString "Programación 1")
          , ("codigo", JNumber 130)
          , ("anio", JNumber 2019)
          , ("semestre", JNumber 2)
          , ("nota", JNumber 12)
          ]
        ]))
  : []

tests PromedioEscolaridad
  = assertEq "promedioEscolaridad 1"
      (promedioEscolaridad (est 1))
      (Just ((1 + 7 + 2 + 12 + 0 + 0) /6))

  : []

-- test para show (asume instancia de Eq con igualdad estructural para JSON, se
-- puede implementar (al final), o agregar cláusula deriving en AST..)
-- Ojo, si a la llamada a read se le pasa un objeto mal formado vamos
-- a tener error en tiempo de ejecución y se va a abortar la ejecución
-- del resto de los tests. Si tienen la instancia de show mal
-- implementada pero quieren probar el resto, pueden no correr estos
-- tests modificando la definición de allTests
tests Show' =
  map
    (\a ->
       if a == ((read :: String -> JSON) . show) a
         then Ok
         else Fail "show")
    ([jval n | n <- [1, 2]] ++ map JObject [obj n | n <- [1..4]])

  ++ [assertEq "show" (show (JObject [("k", JNull),
                                      ("l l", JArray [JNull, JNull ])]))
                      ("{\"k\" : null, \"l l\" : [null, null]}")

     ]


allTests = concat [tests a | a <- [LookupField .. PromedioEscolaridad]]


main :: IO ()
main =
  case filter (/= Ok) allTests of
    [] -> putStrLn "Todos los tests Ok!"
    l  -> sequence_ $ map f l
      where f (Fail m) = putStrLn $ "Fallo en test: " ++ m


instance Eq JSON where
  (JString s) == (JString t)
    = s == t
  (JNumber n) == (JNumber m)
    = n == m
  (JBoolean b) == (JBoolean c)
    = b == c
  JNull == JNull
    = True
  JObject o == JObject p
    = o == p
  JArray r == JArray s
    = r == s
  _ == _ = False