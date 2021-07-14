module AST where
import Data.List
import Control.Monad
import Data.Char
import ParserCombinators

-- AST para JSON
data JSON
    = JString  String
    | JNumber  JSONNumber
    | JBoolean Bool
    | JNull
    | JObject  (Object JSON)
    | JArray   Array 

type Object a   = [(Key, a)]
type Array      = [JSON]
type Key        = String
type JSONNumber = Integer

class ToJSON a where
    toJSON :: a -> JSON

class FromJSON a where
    fromJSON :: JSON -> Maybe a

-- instancia de Show
instance Show JSON where
    show (JString str) = "\"" ++ str ++ "\"" 
    show (JNumber n) = show n
    show (JBoolean True) = "true"
    show (JBoolean False) = "false"
    show (JNull) =  "null"
    show (JArray a) = "[" ++ (values a) ++ "]"
        where values [] = ""
              values vs = intercalate ", " (map show vs)
    show (JObject o) = "{" ++ (pairsKV o) ++ "}"
        where pairsKV [] = ""
              pairsKV ps = intercalate ", " (map renderPairKV ps)
              renderPairKV (k,v)   = show k ++ " : " ++ show v


-- Instancia de read.
-- Se hace el parsing que se define más adelante.
-- En caso de que el parsing falle 'read' falla en tiempo de ejecución
-- (como en general lo hacen las instancias de Read definidas en el preludio)
instance Read JSON where
    readsPrec _ = \s ->
       let parse = (runP pJSON . fromJustErr . lexer) s
       in case parse of
            ((t,[]):_) -> [(t,"")]
            _  -> error "no parse (ill-formed json value)"
        where fromJustErr (Just a) = a
              fromJustErr _ = error "no parse (fail at lexer)"


-- lee objeto JSON desde un archivo
importJSON :: FilePath -> IO (Maybe JSON)
importJSON f = do
  file <- readFile f
  let lexed  = lexer file
  let parsed = runP pJSON <$> lexed
  case parsed of
    Just ((t,[]):_) -> return (Just t)
    _  -> return Nothing


{- PARSER
 Antes de parsear un objeto JSON, primero se realiza un análisis léxico del
 string de entrada. Se construye una lista de tokens que representan los
 terminales del lenguaje a parsear.

 Luego se implementa el parser a partir de una lista de tokens.
-}

-- conjunto de tokens
data JSONToken
    = TString String
    | TNum Integer
    | TLSqrBr
    | TRSqrBr
    | TRCurlyBr
    | TLCurlyBr
    | TComma
    | TNull
    | TTrue
    | TFalse
    | TColon
    deriving (Show, Eq)

-- primer función aplicada a la entrada.
-- Se separa la entrada en lista de los "potenciales tokens" (no son tokens
-- todavía, son strings a los que luego tratamos de tokenizar con |tokenize|)
cutInput :: String -> [String]
cutInput []
    = []
cutInput ('\"':cs)
    = let (wd, tl) = span (/= '\"') cs
      in ("\""++ wd ++ "\"") : cutInput (tail tl)
cutInput ('t':'r':'u':'e':cs)
    = "true" : cutInput cs
cutInput ('f':'a':'l':'s':'e':cs)
    = "false" : cutInput cs
cutInput ('n':'u':'l':'l':cs)
    = "null" : cutInput cs
cutInput s@(c:cs)
    | isNumChar c = let (wd, tl) = span isNumChar s
                    in if wd /=  []
                       then wd : cutInput tl
                       else [c]:cutInput cs
    | otherwise = [c] : cutInput cs
    where isNumChar a = isDigit a || a == '.'

-- tokenize crea un token (o falla) a partir de una cadena
tokenize :: String -> Maybe JSONToken
tokenize s@('\"':cs) = Just $ TString $ init cs
tokenize "{"         = Just TLCurlyBr
tokenize "}"         = Just TRCurlyBr
tokenize "["         = Just TLSqrBr
tokenize "]"         = Just TRSqrBr
tokenize ","         = Just TComma
tokenize "null"      = Just TNull
tokenize "true"      = Just TTrue
tokenize "false"     = Just TFalse
tokenize ":"         = Just TColon
tokenize s@(c:cs)
         | isDigit c = Just . TNum $ (read s :: Integer)
         | otherwise = Nothing
tokenize _           = Nothing


-- el analizador léxico completo: corta la entrada (aplicando cutInput),
-- borra los espacios innecesarios, tokeniza los potenciales tokens que quedan
-- Si hay algún token mal formado se propaga el fallo.
lexer :: String -> Maybe [JSONToken]
lexer = sequence . map tokenize . filter (not . allWhitespace) . cutInput
    where allWhitespace = foldr ((&&) . isSpace) True


-- Parsers para JSON
-- Toman como entrada una sucesión de tokens.
type JParser = Parser [JSONToken]

-- reconoce un token (pasado como argumento)
pToken :: JSONToken -> JParser JSONToken
pToken t =
    Parser $ \s ->
        case s of
          []      -> []
          (t':ts) -> if   t == t'
                     then [(t, ts)]
                     else []

-- reconoce una cadena
pString :: JParser JSONToken
pString
  = Parser $ \s ->
    case s of
      (js@(TString s):ts) -> [(js, ts)]
      _                   -> []

-- parser principal
pJSON :: JParser JSON
pJSON = pJSTring
    <|> pJNumber
    <|> pJObject
    <|> pJArray
    <|> pJBoolean
    <|> pJNull

-- A partir de ahora, los parsers para cada producción.
pJSTring :: JParser JSON
pJSTring = do
  s <- pString
  return $ (\(TString a) -> (JString a)) s

pJNumber = Parser $ \s ->
  case s of
    (js@(TNum s):ts) -> [(JNumber s, ts)]
    _ -> []

pJObject
   =  (do pToken TLCurlyBr
          f  <- pField
          fs <- pList (pToken TComma >> pField)
          pToken TRCurlyBr
          return $ JObject (f:fs))
  <|> (do pToken TLCurlyBr
          f  <- pField
          pToken TRCurlyBr
          return $ JObject [f])
  <|> (do pToken TLCurlyBr
          pToken TRCurlyBr
          return $ JObject [])

-- aux, para un campo en un objeto
pField = do
  name <- pJSTring
  pToken TColon
  att <- pJSON
  return $ ((\(JString a) -> a) name, att)

pJArray = ( do
  pToken TLSqrBr
  f  <- pJSON
  fs <- pList (pToken TComma >> pJSON)
  pToken TRSqrBr
  return $ JArray (f:fs)
          )
  <|> ( do
  pToken TLSqrBr
  f  <- pJSON
  pToken TRSqrBr
  return $ JArray [f]
      )
  <|> ( do
  pToken TLSqrBr
  pToken TRSqrBr
  return $ JArray []
      )

pJBoolean =
      (pToken TTrue >> return (JBoolean True))
  <|> (pToken TFalse >> return (JBoolean False))

pJNull    = pToken TNull >> return (JNull)


{-
{"employees": [ { "firstName":"John", "lastName":"Doe" },
                { "firstName":"Anna", "lastName":"Smith" },
                { "firstName":"Peter", "lastName":"Jones" }],
 "number" : 12,
 "boolean" : false,
 "null" : null}
-}
exampleObj =
    JObject
    [("employees",
     JArray [
       JObject [("firstName", JString "John"),
                ("lastName", JString "Doe")]
      ,JObject [("firstName", JString "Anna"),
                ("lastName", JString "Smith")]
      ,JObject [("firstName", JString "Peter"),
                ("lastName", JString "Jones")]
      ]),
     ("number", JNumber 12),
     ("boolean", JBoolean False),
     ("null", JNull)]

exampleArr =
    JArray [JNull,
            JObject [("key 1", JNumber 1),
                     ("key 2", JBoolean False)]]
