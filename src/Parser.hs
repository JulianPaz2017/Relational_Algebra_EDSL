module Parser (parserStatements) where 

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language                  ( emptyDef )
import qualified Data.Map                       as Map ( fromList )
import qualified Data.Set                       as Set
import           Common


-------------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t


-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , reservedNames   = ["CREATE", "AS", "DROP", "INSERT", "REMOVE", "DEFINE", "UNDEFINE", "INT", "DOUBLE", "STRING"]
    , reservedOpNames = [ "||",   -- or booleano
                          "&&",   -- and booleano
                          "!" ,   -- negación booleana
                          "==",   -- igualdad de valores
                          "!=",   -- desigualdad de valores
                          "<" ,   -- comparador "menor" de valores
                          ">" ,   -- comparador "mayor" de valores
                          "<=",   -- comparador "menor o igual" de valores
                          ">=",   -- comparador "mayor o igual" de valores 
                          "-"     -- signo negativo en números
                        ]
    }
  )


--------------------------------------
---- Parser compartidos
--------------------------------------

parseRelationName :: Parser RelationName
parseRelationName = identifier lis


parseAttributeName :: Parser AttrName
parseAttributeName = identifier lis


parseOperatorName :: Parser OperatorName
parseOperatorName = identifier lis

parseAttribute :: Parser AttrKey
parseAttribute = 
  do relationName  <- parseRelationName
     _             <- symbol lis "."
     attributeName <- parseAttributeName
     return (relationName, attributeName) 

parseAttributes :: Parser [AttrKey]
parseAttributes = parseAttribute `sepBy` (symbol lis ",")
    


parseValue :: Parser Value
parseValue = try parseDoubleValue <|> try parseIntValue <|> parseStringValue
  where
    parseIntValue :: Parser Value
    parseIntValue = 
      do i <- integer lis
         return (IntValue (fromIntegral i))

    parseDoubleValue :: Parser Value
    parseDoubleValue = 
      do s <- optionMaybe (reservedOp lis "-")
         d <- float lis
         case s of
           Nothing -> return (DoubleValue d)
           Just _  -> return (DoubleValue (-d))
         

    parseStringValue :: Parser Value
    parseStringValue = 
      do s <- stringLiteral lis
         return (StringValue s)


--------------------------------------
---- Parser de predicados
--------------------------------------

parseBool :: Parser Predicate
parseBool = try parseTrue <|> parseFalse
  where
    parseTrue :: Parser Predicate
    parseTrue = do _ <- reserved lis "True"
                   return (PBool True)

    parseFalse :: Parser Predicate
    parseFalse = do _ <- reserved lis "False"
                    return (PBool False)


parseComparisonOperator :: Parser ComparisonOperator
parseComparisonOperator = 
  try parseEqualOperator
  <|>
  try parseNotEqualOperator
  <|>
  try parseLessThanOperator
  <|>
  try parseLessOrEqualOperator
  <|>
  try parseGreaterThanOperator
  <|>
  parseGreaterOrEqualOperator

  where
    parseEqualOperator :: Parser ComparisonOperator
    parseEqualOperator = 
      do _ <- reservedOp lis "=="
         return Equal

    parseNotEqualOperator :: Parser ComparisonOperator
    parseNotEqualOperator = 
      do _ <- reservedOp lis "!="
         return NotEqual

    parseLessThanOperator :: Parser ComparisonOperator
    parseLessThanOperator = 
      do _ <- reservedOp lis "<"
         return LessThan                        

    parseLessOrEqualOperator :: Parser ComparisonOperator
    parseLessOrEqualOperator = 
      do _ <- reservedOp lis "<="
         return LessOrEqual

    parseGreaterThanOperator :: Parser ComparisonOperator
    parseGreaterThanOperator = 
      do _ <- reservedOp lis ">"
         return GreaterThan                        

    parseGreaterOrEqualOperator :: Parser ComparisonOperator
    parseGreaterOrEqualOperator = 
      do _ <- reservedOp lis ">="
         return GreaterOrEqual


parseComparison :: Parser Predicate
parseComparison = 
  do atom  <- try parseConstantValue <|> parseReference
     cmpOp <- parseComparisonOperator
     atom' <- try parseConstantValue <|> parseReference
     return (PCmp cmpOp atom atom')
  where
    parseConstantValue :: Parser PredicateAtom
    parseConstantValue = 
      do value <- parseValue
         return (ConstantValue value)

    parseReference :: Parser PredicateAtom
    parseReference = 
      do relationName  <- parseRelationName
         _             <- symbol lis "."
         attributeName <- parseAttributeName
         return (AttributeReference relationName attributeName)


parseAtomPredicate :: Parser Predicate
parseAtomPredicate = try parseBool <|> try parseComparison <|> (parens lis parsePredicate)


parseUnaryPredicate :: Parser Predicate
parseUnaryPredicate = try parseNot <|> parseAtomPredicate
  where
    parseNot :: Parser Predicate
    parseNot = 
      do _         <- reservedOp lis "!"
         predicate <- parseAtomPredicate
         return (PNot predicate)


parsePredicate :: Parser Predicate
parsePredicate = parseUnaryPredicate `chainl1` binaryPredicateOperator
  where
    andOperator :: Parser (Predicate -> Predicate -> Predicate)
    andOperator = 
      do _ <- reservedOp lis "&&"
         return PAnd

    orOperator :: Parser (Predicate -> Predicate -> Predicate)
    orOperator = 
      do _ <- reservedOp lis "||"
         return POr

    binaryPredicateOperator :: Parser (Predicate -> Predicate -> Predicate)
    binaryPredicateOperator = try andOperator <|> orOperator


--------------------------------------
---- Parser de sentencias
--------------------------------------

parseStatements :: Parser [(EDSLCommand, Int)]
parseStatements = parseStatement `endBy` (symbol lis ";")


parseStatement :: Parser (EDSLCommand, Int) 
parseStatement = 
  do lineNumber <- getPosition
     statement  <- parseStatement'
     return (statement, sourceLine lineNumber)
  where
    parseStatement' :: Parser EDSLCommand
    parseStatement' = 
      try parseCreate 
      <|>
      try parseDrop
      <|>
      try parseDefine
      <|>
      try parseUndefine
      <|>
      try parseInsert
      <|>
      try parseRemove
      <|>
      try parseQueryStatement
      <|>
      return EUnknown


--------------------------------------
---- Parser del DDL
--------------------------------------

parseSchema :: RelationName -> Parser Schema
parseSchema relationName = 
  do _      <- symbol lis "{"
     schema <- (parseSchema' relationName) `sepBy` (symbol lis ",")
     _      <- symbol lis "}"
     return schema

  where
    parseDInt :: Parser Domain
    parseDInt = 
      do _ <- reserved lis "INT"
         return IntDomain

    parseDString :: Parser Domain
    parseDString = 
      do _ <- reserved lis "STRING"
         return StringDomain

    parseDDouble :: Parser Domain
    parseDDouble = 
      do _ <- reserved lis "DOUBLE"
         return DoubleDomain

    parseDomain :: Parser Domain
    parseDomain = 
      try parseDInt 
      <|> 
      try parseDString 
      <|> 
      parseDDouble

    parseSchema' :: RelationName -> Parser (AttrKey, Domain)
    parseSchema' rName = 
      do attributeName <- parseAttributeName
         domain        <- parseDomain
         return ((rName, attributeName), domain)


-- Sintaxis CREATE:
--
-- CREATE <relation_name> {<attr_name_1> <domain_attr_1>, ..., <attr_name_n> <domain_attr_n> } 
-- CREATE <relation_name> AS <query>
--
parseCreate :: Parser EDSLCommand
parseCreate = 
  do _            <- reserved lis "CREATE"
     relationName <- parseRelationName
     try (parseCreate' relationName) <|> (parseCreate'' relationName)
  
  where
    parseCreate' :: RelationName -> Parser EDSLCommand
    parseCreate' rName  = 
      do schema <- parseSchema rName
         let 
           relation = Relation (Just rName) schema Set.empty (length schema)
         return (CreateRelation rName (Left relation))
    
    parseCreate'' :: RelationName -> Parser EDSLCommand
    parseCreate'' rName = 
      do _     <- reserved lis "AS"
         query <- parseQuery []
         return (CreateRelation rName (Right query))


-- Sintaxis DROP:
--
-- DROP <relation_name>
--
parseDrop :: Parser EDSLCommand
parseDrop = 
  do _            <- reserved lis "DROP"
     relationName <- parseRelationName
     return (DropRelation relationName)


-- Sintaxis INSERT:
--
-- INSERT <relation_name> (attr_1, ..., attr_n) {t_1, t_2, ..., t_m}
--
parseInsert :: Parser EDSLCommand
parseInsert = 
  do _            <- reserved lis "INSERT"
     relationName <- parseRelationName
     attributes   <- parens lis parseAttributes
     tuples       <- parseTuples attributes
     return (InsertTuple relationName tuples)
  
  where
    auxilarMessage :: String
    auxilarMessage = "Error al parser las tuplas. Inconsistencia en la cantidad de valores y atributos ingresados\n"

    parseTuple :: [AttrKey] -> Parser Tuple
    parseTuple attrs = 
      do values <- parseValue `sepBy` (symbol lis ",")
         if 
           (length attrs == length values) 
         then 
           return (Map.fromList (zip attrs values))
         else 
           fail auxilarMessage  

    parseTuples :: [AttrKey] -> Parser [Tuple]
    parseTuples attrs = 
      do _      <- symbol lis "{"
         tuples <- (parens lis (parseTuple attrs)) `sepBy` (symbol lis ",")
         _      <- symbol lis "}"
         return tuples


-- Sintaxis REMOVE:
--
-- REMOVE <relation_name> <predicate>
--
parseRemove :: Parser EDSLCommand
parseRemove = 
  do _            <- reserved lis "REMOVE"
     relationName <- parseRelationName
     predicate    <- parsePredicate
     return (RemoveTuple relationName predicate)


-- Sintaxis DEFINE:
--
-- DEFINE <operator_name> (argument_1, ..., argument_n) = <query>
--
parseDefine :: Parser EDSLCommand
parseDefine = 
  do _            <- reserved lis "DEFINE"
     operatorName <- parseOperatorName
     arguments    <- parseOperatorArguments
     _            <- symbol lis "="
     query        <- parseQuery arguments
     return (DefineOperator operatorName (CustomOperator (length arguments) arguments query))
  
  where
    parseArgumentName :: Parser VariableName
    parseArgumentName = identifier lis

    parseOperatorArguments' :: Parser [VariableName]
    parseOperatorArguments' = parseArgumentName `sepBy` (symbol lis ",")

    parseOperatorArguments :: Parser [VariableName]
    parseOperatorArguments = parens lis parseOperatorArguments' 
                                

-- Sintaxis UNDEFINE:
--
-- UNDEFINE <operator_name> 
--
parseUndefine :: Parser EDSLCommand
parseUndefine = 
  do _            <- reserved lis "UNDEFINE"
     operatorName <- parseOperatorName
     return (UndefineOperator operatorName)


--------------------------------------
---- Parser del DML
--------------------------------------
parseOperation :: [VariableName] -> Parser Query
parseOperation variableNames = 
  do operatorName <- parseOperatorName
     case operatorName of
       "select"            -> parseSelect variableNames
       "project"           -> parseProject variableNames
       "rename"            -> parseRename variableNames
       "renameA"           -> parseRenameA variableNames
       "union"             -> parens lis (parseUnion variableNames)
       "difference"        -> parens lis (parseDifference variableNames)
       "cartesian_product" -> parens lis (parseCartesianProduct variableNames)
       _                   -> parens lis (parseCustomOperator variableNames operatorName)

  where
    parseSelect :: [VariableName] -> Parser Query
    parseSelect vn = 
      do _         <- symbol lis "{"
         predicate <- parsePredicate 
         _         <- symbol lis "}"
         relation  <- parens lis (parseQuery vn)
         return (Select predicate relation)

    parseProject :: [VariableName] -> Parser Query
    parseProject vn = 
      do _          <- symbol lis "{"
         attributes <- parseAttributes
         _          <- symbol lis "}"
         relation   <- parens lis (parseQuery vn)
         return (Project attributes relation)

    parseRename :: [VariableName] -> Parser Query
    parseRename vn = 
      do _            <- symbol lis "{"
         relationName <- parseRelationName
         _            <- symbol lis "}"
         relation     <- parens lis (parseQuery vn)
         return (Rename relationName relation)
    
    parseRenameA' :: Parser (AttrKey, AttrKey)
    parseRenameA' = 
      do attrKey  <- parseAttribute
         _        <- symbol lis "="
         attrKey' <- parseAttribute
         return (attrKey, attrKey')

    parseRenameA :: [VariableName] -> Parser Query
    parseRenameA vn = 
      do _            <- symbol lis "{"
         reNamings    <- parseRenameA' `sepBy` (symbol lis ",")
         _            <- symbol lis "}"
         relation     <- parens lis (parseQuery vn)
         return (RenameA reNamings relation)

    parseUnion :: [VariableName] -> Parser Query
    parseUnion vn = 
      do relation  <- parseQuery vn
         _         <- symbol lis ","
         relation' <- parseQuery vn
         return (Binary Union relation relation')

    parseDifference :: [VariableName] -> Parser Query
    parseDifference vn = 
      do relation  <- parseQuery vn
         _         <- symbol lis ","
         relation' <- parseQuery vn
         return (Binary Difference relation relation')
    
    parseCartesianProduct :: [VariableName] -> Parser Query
    parseCartesianProduct vn = 
      do relation  <- parseQuery vn
         _         <- symbol lis ","
         relation' <- parseQuery vn
         return (Binary CartesianProduct relation relation')

    parseCustomOperator :: [VariableName] -> OperatorName -> Parser Query
    parseCustomOperator vn opName = 
      do arguments <- (parseQuery vn) `sepBy` (symbol lis ",")
         return (Custom opName arguments) 


parseAtomic :: [VariableName] -> Parser Query
parseAtomic variableNames = 
  do x <- identifier lis
     case elem x variableNames of
       True  -> return (Variable x)
       False -> return (Atomic x)


parseQuery :: [VariableName] -> Parser Query
parseQuery variableNames = 
  try (parseOperation variableNames) 
  <|> 
  parseAtomic variableNames 


parseQueryStatement :: Parser EDSLCommand
parseQueryStatement = 
  do query <- parseQuery []
     return (QueryStatement query)


--------------------------------------
---- Función de parseo
--------------------------------------

parserStatements :: SourceName -> String -> ParseResult [(EDSLCommand, Int)] 
parserStatements fileName fileContent = 
  case parse (totParser parseStatements) fileName fileContent of
    Left err           -> Failed (show err)
    Right edslCommands -> Ok edslCommands