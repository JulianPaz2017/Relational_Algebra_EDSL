module Eval (evalQuery, evalPredicate) where

import Common
import qualified Data.Map as Map 
import qualified Data.Set as Set

isAttr :: AttrKey -> [(AttrKey, Domain)] -> (Bool, Maybe Domain)
isAttr _ []                      = (False, Nothing)
isAttr (relationName, attName) (((relationName', attName'),attDom):xs) = 
  if 
    (relationName == relationName') && (attName == attName')
  then
    (True, (Just attDom))
  else
    (isAttr (relationName, attName) xs)

checkSchemas :: Schema -> Schema -> Bool
checkSchemas [] []         = True
checkSchemas _  []         = False
checkSchemas [] _          = False
checkSchemas (x:xs) (y:ys) = (checkAttrs x y) && (checkSchemas xs ys)
  where
    checkAttrs :: (AttrKey, Domain) -> (AttrKey, Domain) -> Bool
    checkAttrs (_, relationDomain) (_, relationDomain') = relationDomain == relationDomain'


modifyTuple :: Map.Map AttrKey AttrKey -> AttrKey -> AttrKey
modifyTuple attrMapping attrKey = 
  let 
    Just (relationName, attrName) = Map.lookup attrKey attrMapping
  in
    (relationName, attrName)


checkCartesianProduct :: Relation -> Relation -> (Bool, String)
checkCartesianProduct (Relation maybeRelationName  relationSchema  _ _) 
                      (Relation maybeRelationName' relationSchema' _ _) = 
  if 
    checkRelationNames maybeRelationName maybeRelationName'
  then 
    checkRelationAttr (map fst relationSchema) (map fst relationSchema')
  else
    (False, "Producto cartesiano de dos relaciones con el mismo nombre\n")
  
  where
    checkRelationNames :: Maybe RelationName -> Maybe RelationName -> Bool
    checkRelationNames (Just relationName) (Just relationName') = not (relationName == relationName')
    checkRelationNames _ _ = True

    checkRelationAttr :: [AttrKey] -> [AttrKey] -> (Bool, String)
    checkRelationAttr []       _       = (True, "")
    checkRelationAttr (ak:aks) attrKey = 
      if
        (elem ak attrKey)
      then
        (False, "Producto cartesiano de dos relaciones con atributos compartidos\n")
      else
        checkRelationAttr aks attrKey


evalQuery :: State -> Query -> Either String Relation
evalQuery (S _ relations _) (Atomic relationName) =
  case Map.lookup relationName relations of
    Nothing       -> Left ("Error en la consulta: no existe la relación '" ++ relationName ++ "'\n")
    Just relation -> Right relation

evalQuery (S _ relations _) (Variable variableName) =
  case Map.lookup variableName relations of
    Nothing       -> Left ("Error en la consulta: La variable '" ++ variableName ++ "' se encuentra libre\n")
    Just relation -> Right relation

evalQuery state (Rename relationName query) =
  do relation <- evalQuery state query
     return (renameRelation relationName relation)
  where
    renameSchema :: RelationName -> RelationName -> (AttrKey, Domain) -> (AttrKey, Domain)
    renameSchema rName rName' ((rName'', attName), domain) = 
      if 
        rName'' == rName' 
      then 
        ((rName, attName), domain)
      else 
        ((rName'', attName), domain)

    renameTuple :: RelationName -> RelationName -> Tuple -> Tuple
    renameTuple rName rName' tuple = Map.mapKeys (\k@(n, attName) -> if n == rName' then (rName, attName) else k) tuple 

    renameRelation :: RelationName -> Relation -> Relation
    renameRelation _ r@(Relation Nothing _ _ _) = r
    renameRelation newName (Relation (Just oldName) relationSchema relationInstance relationArity) = 
      let 
        newSchema   = map (renameSchema newName oldName) relationSchema
        newInstance = Set.map (renameTuple newName oldName) relationInstance
      in
        (Relation (Just newName) newSchema newInstance relationArity)

evalQuery state (Select predicate query) =
  do relation          <- evalQuery state query
     predicateFunction <- evalPredicate relation predicate
     return (filterRelation predicateFunction relation)

  where
    filterRelation :: (Tuple -> Bool) -> Relation -> Relation
    filterRelation f (Relation rName rSchema rInstance rArity) = 
      let 
        newInstance = Set.filter f rInstance
      in
        Relation rName rSchema newInstance rArity

evalQuery state (Project attrKeys query) =
  do relation <- evalQuery state query
     projectRelation attrKeys relation

  where
    isSubset :: [AttrKey] -> Schema -> (Bool, Schema)
    isSubset []     _       = (True, [])
    isSubset (x:xs) rSchema = 
      case (isAttr x rSchema) of
        (True, Just attrDomain) -> 
          case (isSubset xs rSchema) of
            (True, ys) -> (True, ((x, attrDomain):ys))
            _          -> (False, [])
        
        _                  -> (False, [])

    projectRelation :: [AttrKey] -> Relation -> Either String Relation 
    projectRelation newAttrs (Relation relationName relationSchema relationInstance _) = 
      case (isSubset newAttrs relationSchema) of
        (True, newSchema) -> 
          let 
            newInstance = Set.map (\t -> Map.restrictKeys t (Set.fromList newAttrs)) relationInstance
          in
            Right (Relation relationName newSchema newInstance (length newSchema))
                                                                    
        _                 -> 
          Left "Error en la consulta: Se proyectaron atributos que no pertenecen a la relación\n"                                                                    

evalQuery state (Binary Union query query') =
  do relation  <- evalQuery state query
     relation' <- evalQuery state query'
     joinRelations relation relation'

  where
    joinRelations :: Relation -> Relation -> Either String Relation
    joinRelations (Relation _  relationSchema  relationInstance  relationArity) 
                  (Relation _  relationSchema' relationInstance' relationArity') =
      if
        (relationArity == relationArity') && (checkSchemas relationSchema relationSchema')
      then 
        do let
             attrMapping        = Map.fromList $ zip (map fst relationSchema') (map fst relationSchema)
             relationInstance'' = Set.map (\t -> Map.mapKeys (modifyTuple attrMapping) t) relationInstance'
           Right (Relation Nothing relationSchema (Set.union relationInstance relationInstance'') relationArity)
      else
        Left "Error en la consulta: Unión de dos relaciones con esquemas distintos\n"     

evalQuery state (Binary Difference query query') =
  do relation  <- evalQuery state query
     relation' <- evalQuery state query'
     differenceRelations relation relation'

  where
    differenceRelations :: Relation -> Relation -> Either String Relation
    differenceRelations (Relation _  relationSchema  relationInstance  relationArity) 
                        (Relation _  relationSchema' relationInstance' relationArity') =
      if
        (relationArity == relationArity') && (checkSchemas relationSchema relationSchema')
      then 
        do let
             attrMapping        = Map.fromList $ zip (map fst relationSchema') (map fst relationSchema)
             relationInstance'' = Set.map (\t -> Map.mapKeys (modifyTuple attrMapping) t) relationInstance'
           Right (Relation Nothing relationSchema (Set.difference relationInstance relationInstance'') relationArity)
      else
        Left "Error en la consulta: Diferencia de dos relaciones con esquemas distintos\n"  

evalQuery state (Binary CartesianProduct query query') = 
  do relation  <- evalQuery state query
     relation' <- evalQuery state query'
     cartesianProductRelation relation relation'
  where
    cartesianProductRelation :: Relation -> Relation -> Either String Relation 
    cartesianProductRelation r@(Relation  _  relationSchema  relationInstance  relationArity) 
                             r'@(Relation _  relationSchema' relationInstance' relationArity') =

      case checkCartesianProduct r r' of
        (False, err) -> Left ("Error en la consulta: " ++ err)
        (True, _)    -> 
          let  
            newSchema   = relationSchema ++ relationSchema'
            newInstance = Set.fromList [ Map.union tR tS | tR <- Set.toList relationInstance, tS <- Set.toList relationInstance']
          in
            Right (Relation Nothing newSchema newInstance (relationArity + relationArity'))

evalQuery state@(S lastFile relations operators) (Custom operatorName queries) =
  case Map.lookup operatorName operators of
    Nothing -> 
      Left ("Error en la consulta: El operador '" ++ operatorName ++ "' no está definido\n")

    Just (CustomOperator operatorArity variableNames query) ->
      case operatorArity - (length queries) of
        0 -> case mapM (evalQuery state) queries of
               Left err -> Left err
               Right relations' ->
                 do let state' = S lastFile (Map.union (Map.fromList (zip variableNames relations')) relations) operators
                    evalQuery state' query

        n -> let 
               auxError = if (n < 0) then "mas" else "menos"
             in
               Left ("Error en la consulta: El operador '" ++ operatorName ++ "' recibió " ++ auxError ++ "argumentos de lo esperado \n")


evalPredicate :: Relation -> Predicate -> Either String (Tuple -> Bool)
evalPredicate relation (PAnd predicate predicate') = 
  do p  <- evalPredicate relation predicate  
     p' <- evalPredicate relation predicate' 
     return (\t -> (p t) && (p' t))

evalPredicate relation (POr  predicate predicate') = 
  do p  <- evalPredicate relation predicate  
     p' <- evalPredicate relation predicate'
     return (\t -> (p t) || (p' t))

evalPredicate relation (PNot predicate)            = 
  do p  <- evalPredicate relation predicate
     return (\t -> not (p t))

evalPredicate _        (PBool bool)                =
  return (\_ -> bool)

evalPredicate relation (PCmp cmpOp predicateAtom predicateAtom')   = 
  do atom  <- evalAtom predicateAtom  relation
     atom' <- evalAtom predicateAtom' relation
     case cmpOp of
       Equal          -> return (\t -> (atom t) == (atom' t))
       NotEqual       -> return (\t -> (atom t) /= (atom' t))
       LessThan       -> return (\t -> (atom t) <  (atom' t))
       LessOrEqual    -> return (\t -> (atom t) <= (atom' t))
       GreaterThan    -> return (\t -> (atom t) >  (atom' t))
       GreaterOrEqual -> return (\t -> (atom t) >= (atom' t))

  where
    evalAtom :: PredicateAtom -> Relation -> Either String (Tuple -> Value)
    evalAtom (ConstantValue value)  _ = 
      Right (\_ -> value)
    evalAtom (AttributeReference relationName attrName) (Relation _ rSchema _ _) = 
      if 
        fst (isAttr (relationName, attrName) rSchema)
      then
        Right (\t ->  (Map.!) t (relationName, attrName))
      else
        Left "Error en el predicado: Uno de los atributos ingresados no existe\n"
