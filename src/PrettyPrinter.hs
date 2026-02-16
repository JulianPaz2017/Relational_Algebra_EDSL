module PrettyPrinter (ppRelation, ppQuery, ppOperator, ppSchema) where

import            Common
import            Text.PrettyPrint.HughesPJ
import            Prelude                   hiding ((<>))
import  qualified Data.Map                  as     Map
import  qualified Data.Set                  as     Set
import            Data.List



attributeKeyToString :: AttrKey -> String
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


    tupleToStringAux :: Tuple -> AttrKey -> String
    tupleToStringAux tuple attrKey = 
      case Map.lookup attrKey tuple of
        Nothing              -> "Null"
        Just (IntValue i)    -> show i
        Just (StringValue s) -> s
        Just (DoubleValue d) -> show d


    tupleToString :: [AttrKey] -> Tuple -> [String]
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
ppPredicate (PCmp cmpOp atom atom')     =
  parens (ppAtom atom <> ppCmpOp cmpOp <> ppAtom atom')
  where
    ppCmpOp :: ComparisonOperator -> Doc
    ppCmpOp Equal          = text "=="
    ppCmpOp NotEqual       = text "!="
    ppCmpOp LessThan       = text "<"
    ppCmpOp LessOrEqual    = text "<="
    ppCmpOp GreaterThan    = text ">"
    ppCmpOp GreaterOrEqual = text ">="

    ppAtom :: PredicateAtom -> Doc
    ppAtom (ConstantValue value) = 
      ppValue value
    ppAtom (AttributeReference rName attrName) = 
      text (attributeKeyToString (rName, attrName))


ppQuery :: Query -> Doc
ppQuery (Atomic relationName)         = 
  text relationName
ppQuery (Variable variableName)       = 
  text variableName
ppQuery (Rename relationName query)   = 
  text ("rename_{" ++ relationName ++ "}") <>
  parens (ppQuery query)
ppQuery (RenameA reNamings query)     = 
  text "renameA_{" <>
  (ppReNamings reNamings) <>
  text "}" <>
  parens (ppQuery query)
  
  where
    ppReNamings :: [(AttrKey, AttrKey)] -> Doc 
    ppReNamings []                    = text ""
    ppReNamings [(attrKey, attrKey')] = 
      text (attributeKeyToString attrKey) <>
      text "=" <>
      text (attributeKeyToString attrKey')
    ppReNamings ((attrKey, attrKey'):attrKeys) = 
      text (attributeKeyToString attrKey) <>
      ppReNamings attrKeys
ppQuery (Select predicate query)      =
  text "select_{" <>
  ppPredicate predicate <>
  char '}' <>
  parens (ppQuery query)
ppQuery (Project attributeKeys query) =
  text "project_{" <>
  hcat (punctuate (text ", ") (map (text . attributeKeyToString) attributeKeys)) <>
  char '}' <>
  parens (ppQuery query)
ppQuery (Binary binOp query query')   =
  ppBinaryOperator binOp <>
  parens ((ppQuery query) <> (text ", ") <> (ppQuery query'))
  where
    ppBinaryOperator :: BinaryOperator -> Doc
    ppBinaryOperator Union            = text "union"
    ppBinaryOperator Difference       = text "difference"
    ppBinaryOperator CartesianProduct = text "cartesian_product"
ppQuery (Custom operatorName querys)  =
  text operatorName <>
  parens (hcat (punctuate (text ", ") (map ppQuery querys)))


ppOperator :: OperatorName -> CustomOperator -> Doc
ppOperator operatorName (CustomOperator _ variableNames query) =
  text operatorName <> 
  parens (ppArguments variableNames) <>
  char '=' <>
  ppQuery query

  where
    ppArguments :: [VariableName] -> Doc
    ppArguments varNames = hcat (punctuate (text ", ") (map text varNames))


ppSchema :: Relation -> RelationName -> Doc
ppSchema (Relation _ relationSchema _ _) relationName =
  text (relationName ++ ":") <+>
  vcat (map ppAttrKey relationSchema)

  where 
    ppDomain :: Domain -> Doc
    ppDomain IntDomain    = text "Int"
    ppDomain StringDomain = text "String"
    ppDomain DoubleDomain = text "Double"

    ppAttrKey :: (AttrKey, Domain) -> Doc
    ppAttrKey (attrKey, domain) =
      text (attributeKeyToString attrKey) <>
      text ": " <>
      ppDomain domain

