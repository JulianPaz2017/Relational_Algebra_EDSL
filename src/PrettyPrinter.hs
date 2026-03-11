module PrettyPrinter (ppRelation, ppQuery, ppOperator, ppSchema, ppError, ppEvalError) where

import            Common
import            Text.PrettyPrint.HughesPJ
import            Prelude                   hiding ((<>))
import  qualified Data.Map                  as     Map
import  qualified Data.Set                  as     Set
import            Data.List


--------------------------------------
---- Pretty Printer Esquema (Describe)
--------------------------------------
ppSchema :: Relation -> RelationName -> Doc
ppSchema (Relation _ relationSchema _ _) relationName =
  text (relationName ++ ":") <+>
  vcat (map ppAttributeKey relationSchema)

  where 
    ppDomain :: Domain -> Doc
    ppDomain IntDomain    = text "Int"
    ppDomain StringDomain = text "String"
    ppDomain DoubleDomain = text "Double"

    ppAttributeKey :: (AttributeKey, Domain) -> Doc
    ppAttributeKey (attributeKey, domain) =
      text (attributeKeyToString attributeKey) <>
      text ": " <>
      ppDomain domain


--------------------------------------
---- Pretty Printer Relaciones
--------------------------------------
attributeKeyToString :: AttributeKey -> String
attributeKeyToString (relationName, attributeName) = relationName ++ "." ++ attributeName

ppRelation :: Relation -> Doc
ppRelation (Relation maybeRelationName relationSchema relationInstance _) = 
  let
    -- Nombre de la relación
    relationName  = maybeToString maybeRelationName

    -- Obtenemos las claves de la relación (<rName>, <attrName>)
    attributeKeys = map fst relationSchema

    -- Convertimos las claves a el string que va en la tabla. (<rName>, <attrName>) -> "<rName>.<attrName>"
    headers       = map attributeKeyToString attributeKeys

    -- Obtenemos las instacia en forma de string. Tuple -> (value_1, ..., value_n). Toma el órden de attributesKeys
    rows          = map (tupleToString attributeKeys) (Set.toList relationInstance)

    -- Obtenemos todas las filas de la tabla
    allRows       = headers : rows

    -- Calculamos el largo más grande que toma un valor en cada columna
    columnWidths  = map (maximum . map length) (transpose allRows)

    -- Creamos el string que separa cada fila
    separator = makeSeparator columnWidths

    -- Completamos cada tupla con espacios vacíos de forma que se todos queden alineados.
    headerRow = makeRow headers columnWidths
    dataRows = map (\row -> makeRow row columnWidths) rows

    -- Agregamos un separador entre cada fila
    dataRowsWithSep = punctuate (text "\n" <> separator) dataRows
    
    lastSeparator = if dataRowsWithSep == [] then [] else [separator] 

  in 
    vcat $
      [ text "Relación:" <+> text relationName
       , separator
       , headerRow
       , separator
      ] ++ dataRowsWithSep ++ lastSeparator

  where
    maybeToString :: Maybe RelationName -> String
    maybeToString Nothing  = "<unnamed>" 
    maybeToString (Just s) = s


    tupleToStringAux :: Tuple -> AttributeKey -> String
    tupleToStringAux tuple attributeKey = 
      case Map.lookup attributeKey tuple of
        Nothing              -> "Null"
        Just (IntValue i)    -> show i
        Just (StringValue s) -> s
        Just (DoubleValue d) -> show d


    tupleToString :: [AttributeKey] -> Tuple -> [String]
    tupleToString attrKeys tuple = map (tupleToStringAux tuple) attrKeys


    makeSeparator :: [Int] -> Doc
    makeSeparator widths = 
      let 
        segments = map (\w -> text (replicate (w + 2) '-')) widths
      in 
        text "+" <> hcat (punctuate (char '+') segments) <> char '+'
 

    makeRow :: [String] -> [Int] -> Doc
    makeRow values widths = 
      let 
        paddedValues = zipWith (\s w -> s ++ (replicate (w - length s) ' ')) values widths
        cells = map text paddedValues
      in 
        char '|' <+> hcat (punctuate (text " | ") cells) <+> char '|'



--------------------------------------
---- Pretty Printer Operadores Custom
--------------------------------------
ppOperator :: OperatorName -> CustomOperator -> Doc
ppOperator operatorName (CustomOperator _ variableNames query) =
  text operatorName <> 
  parens (ppArguments variableNames) <>
  char '=' <>
  ppQuery query

  where
    ppArguments :: [VariableName] -> Doc
    ppArguments varNames = hcat (punctuate (text ", ") (map text varNames))



--------------------------------------
---- Pretty Printer Consultas
--------------------------------------
ppValue :: Value -> Doc
ppValue (IntValue i)    = text (show i) 
ppValue (StringValue s) = text s
ppValue (DoubleValue d) = text (show d)


ppPredicate :: Predicate -> Doc
ppPredicate (PAnd predicate predicate') =
  parens (ppPredicate predicate <> text " && " <> ppPredicate predicate')
ppPredicate (POr predicate predicate')  =
  parens (ppPredicate predicate <> text " || " <> ppPredicate predicate')
ppPredicate (PNot predicate)            =
  text "!" <>
  parens (ppPredicate predicate)
ppPredicate (PBool bool)                =
  text (show bool)
ppPredicate (PCmp comparisonOperator atom atom')     =
  parens (ppAtom atom <> ppComparisionOperator comparisonOperator <> ppAtom atom')
  where
    ppComparisionOperator :: ComparisonOperator -> Doc
    ppComparisionOperator Equal          = text "=="
    ppComparisionOperator NotEqual       = text "!="
    ppComparisionOperator LessThan       = text "<"
    ppComparisionOperator LessOrEqual    = text "<="
    ppComparisionOperator GreaterThan    = text ">"
    ppComparisionOperator GreaterOrEqual = text ">="

    ppAtom :: PredicateAtom -> Doc
    ppAtom (ConstantValue value) = 
      ppValue value
    ppAtom (AttributeReference relationName attributeName) = 
      text (attributeKeyToString (relationName, attributeName))


ppQuery :: Query -> Doc
ppQuery (Atomic relationName)         = 
  text relationName
ppQuery (Variable variableName)       = 
  text variableName
ppQuery (Rename relationName query)   = 
  text "rename" <>
  braces (text relationName) <>
  parens (ppQuery query)
ppQuery (RenameA reNamings query)     = 
  text "renameA" <>
  braces (ppReNamings reNamings) <>
  parens (ppQuery query)
  
  where
    ppReNaming :: (AttributeKey, AttributeKey) -> Doc
    ppReNaming (attributeKey, attributeKey') =
      text (attributeKeyToString attributeKey) <>
      text "=" <>
      text (attributeKeyToString attributeKey')

    ppReNamings :: AttributeKeyMap -> Doc
    ppReNamings attributeMap = (hcat (punctuate (text ", ") (map ppReNaming (Map.toList attributeMap))))  
ppQuery (Select predicate query)      =
  text "select" <>
  braces (ppPredicate predicate) <>
  parens (ppQuery query)
ppQuery (Project attributeKeys query) =
  text "project" <>
  braces (hcat (punctuate (text ", ") (map (text . attributeKeyToString) attributeKeys))) <>
  parens (ppQuery query)
ppQuery (Binary binaryOperator query query')   =
  ppBinaryOperator binaryOperator <>
  parens ((ppQuery query) <> (text ", ") <> (ppQuery query'))
  where
    ppBinaryOperator :: BinaryOperator -> Doc
    ppBinaryOperator Union            = text "union"
    ppBinaryOperator Difference       = text "difference"
    ppBinaryOperator CartesianProduct = text "cartesian_product"
ppQuery (Custom operatorName querys)  =
  text operatorName <>
  parens (hcat (punctuate (text ", ") (map ppQuery querys)))



--------------------------------------
---- Pretty Printer Errores
--------------------------------------
ppError :: ContextualError -> Doc
ppError (ContextualError query context evalErr) = 
  ppErrorContext context $+$ text "" $+$ nest 2 (ppQuery query) $+$ text "" $+$ ppEvalError evalErr

ppErrorContext :: ErrorContext -> Doc
ppErrorContext InQuery =
  text "Error al evaluar la consulta"
ppErrorContext (InPredicate predicate) =
  text "Error al evaluar el predicado" $+$ text "" $+$
  nest 2 (ppPredicate predicate) $+$ text "" $+$
  text "Dentro de la consulta"
ppErrorContext InProyect =
  text "Error al proyectar los argumentos dentro de la consulta"
ppErrorContext InRenameAttributes =
  text "Error al renombrar atributos de una relación dentro de la consulta"
ppErrorContext InSubstitution =
  text "Error con el operador custom dentro de la consulta"


ppEvalError :: EvalError -> Doc
ppEvalError (UndefinedRelation relationName) = 
  text "La relación " <+> quotes (text relationName) <+> text "no existe"
ppEvalError (UndefinedAttribute attributeKey) =
  text "El atributo" <+> quotes (text (attributeKeyToString attributeKey)) <+> text "no existe"
ppEvalError (UndefinedOperator operatorName) =
  text "El operador" <+> quotes (text operatorName) <+> text "no existe"
ppEvalError (ArityMismatch operatorName x y) =
  text "El operador" <+> quotes (text operatorName) <+> 
  text "espera" <+> text (show x) <+> text "argumentos, pero recibió " <+> text (show y) <+> text "argumentos"
ppEvalError (UnboundVariable variableName )=
  text "La variable" <+> quotes (text variableName) <+> text "se encuentra libre"
ppEvalError IncompatibleSchemas =
  text "Las relaciones tienen esquemas incompatibles"
ppEvalError (SameRelationName relationName) =
  text "Producto cartesiano entre relaciones con el mismo nombre" <+> parens (text relationName)
ppEvalError (SharedAttributes attributeKey) =
  text "Producto cartesiano entre relaciones con atributos compartidos" <+> 
  parens (text (attributeKeyToString attributeKey))
ppEvalError (DuplicateAttributeName attributeKey) =
  text "El renombramiento genera atributos duplicados" <+> 
  parens (text (attributeKeyToString attributeKey))