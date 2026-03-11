module Eval (evalQuery, evalPredicate) where

import Common
import qualified Data.Map as Map 
import qualified Data.Set as Set

renameTuple :: (AttributeKey -> Bool) -> (AttributeKey -> AttributeKey) -> Tuple -> Tuple
renameTuple predicate f tuple = 
  Map.mapKeys (\attributeKey -> if (predicate attributeKey) then (f attributeKey) else attributeKey) tuple 


findFirstMatch :: AttributeName -> Schema -> Maybe AttributeKey
findFirstMatch _ [] = Nothing
findFirstMatch attribute (((relationName, attributeName), _):rest) =
  if 
    attributeName == attribute
  then 
    Just (relationName, attributeName)
  else 
    findFirstMatch attribute rest

checkAttributeKeys :: AttributeKey -> AttributeKey -> Bool
checkAttributeKeys (relationName, attributeName) (relationName', attributeName') =
  (relationName == relationName') && (attributeName == attributeName')

isAttribute :: Schema -> AttributeKey -> Bool
isAttribute [] _ = False
isAttribute ((attributeKey, _):rest) attributeKey' =
  (checkAttributeKeys attributeKey attributeKey') || (isAttribute rest attributeKey')


checkSchemas :: Schema -> Schema -> Bool
checkSchemas [] []         = True
checkSchemas _  []         = False
checkSchemas [] _          = False
checkSchemas (x:xs) (y:ys) = (checkAttributes x y) && (checkSchemas xs ys)
  where
    checkAttributes :: (AttributeKey, Domain) -> (AttributeKey, Domain) -> Bool
    checkAttributes (_, relationDomain) (_, relationDomain') = relationDomain == relationDomain'


modifyTuple :: Map.Map AttributeKey AttributeKey -> AttributeKey -> AttributeKey
modifyTuple attributeMapping attributeKey = 
  let 
    Just (relationName, attributeName) = Map.lookup attributeKey attributeMapping
  in
    (relationName, attributeName)


checkCartesianProduct :: Relation -> Relation -> Either EvalError Bool
checkCartesianProduct (Relation maybeRelationName  relationSchema  _ _) 
                      (Relation maybeRelationName' relationSchema' _ _) = 
  if 
    checkRelationNames maybeRelationName maybeRelationName'
  then 
    checkRelationAttributes (map fst relationSchema) (map fst relationSchema')
  else
    Left (SameRelationName (obtainRelationName maybeRelationName))
  
  where
    obtainRelationName :: Maybe RelationName -> RelationName
    obtainRelationName Nothing             = error "invalid\n"
    obtainRelationName (Just relationName) = relationName

    checkRelationNames :: Maybe RelationName -> Maybe RelationName -> Bool
    checkRelationNames (Just relationName) (Just relationName') = not (relationName == relationName')
    checkRelationNames _ _ = True

    checkRelationAttributes :: [AttributeKey] -> [AttributeKey] -> Either EvalError Bool
    checkRelationAttributes []       _       = Right True
    checkRelationAttributes (attributeKey:attributeKeys) attributeKey' = 
      if
        (elem attributeKey attributeKey')
      then
        Left (SharedAttributes attributeKey)
      else
        checkRelationAttributes attributeKeys attributeKey'


--------------------------------------
---- Evaluador de consultas
--------------------------------------
evalQuery :: State -> Query -> EvalResult Relation
evalQuery (S _ relations _) query@(Atomic relationName) =
  case Map.lookup relationName relations of
    Nothing       -> Left (ContextualError query InQuery (UndefinedRelation relationName))
    Just relation -> return relation

evalQuery (S _ relations _) query@(Variable variableName) =
  case Map.lookup variableName relations of
    Nothing       -> Left (ContextualError query InQuery (UnboundVariable variableName))
    Just relation -> return relation

evalQuery state (Rename relationName query) =
  do relation <- evalQuery state query
     return (renameRelation relationName relation)
  where
    renameSchema :: RelationName -> RelationName -> (AttributeKey, Domain) -> (AttributeKey, Domain)
    renameSchema rName rName' ((rName'', attributeName), domain) = 
      if 
        rName'' == rName' 
      then 
        ((rName, attributeName), domain)
      else 
        ((rName'', attributeName), domain)

    renameRelation :: RelationName -> Relation -> Relation
    renameRelation _ r@(Relation Nothing _ _ _) = r
    renameRelation newName (Relation (Just oldName) relationSchema relationInstance relationArity) = 
      let 
        newSchema   = map (renameSchema newName oldName) relationSchema
        newInstance = Set.map (renameTuple (\(rName, _) -> rName == oldName) (\(_, attributeName) -> (newName, attributeName))) relationInstance
      in
        (Relation (Just newName) newSchema newInstance relationArity)

evalQuery state query@(RenameA attributesMap query') =
  do relation <- evalQuery state query'
     case (renameAttributes relation attributesMap) of
       Right relation' ->
         return relation'
       Left err ->
         Left (ContextualError query InRenameAttributes err)

  where
    checkRenamings :: Schema -> Either EvalError Schema
    checkRenamings [] = return []
    checkRenamings ((attributeKey, domain):rest) =
      if
        isAttribute rest attributeKey
      then
        Left (DuplicateAttributeName attributeKey)
      else do rest' <- checkRenamings rest
              return ((attributeKey, domain):rest')           

    renameSchema' :: AttributeKeyMap -> (AttributeKey, Domain) -> (AttributeKey, Domain)
    renameSchema' attrMap (attributeKey, domain) =
      case Map.lookup attributeKey attrMap of
        Just attributeKey' -> 
          (attributeKey', domain)
        Nothing -> 
          (attributeKey, domain)

    checkAttributeMap :: Schema -> AttributeKey -> Either EvalError AttributeKey
    checkAttributeMap schema attributeKey = 
      if 
        isAttribute schema attributeKey
      then
        return attributeKey
      else
        Left (UndefinedAttribute attributeKey)

    renameSchema :: Schema -> AttributeKeyMap -> Either EvalError Schema
    renameSchema schema attrMap =
      do _ <- mapM (checkAttributeMap schema) (Map.keys attrMap)
         let schema' = map (renameSchema' attrMap) schema
         checkRenamings schema'
          

    renameAttributes :: Relation -> AttributeKeyMap -> Either EvalError Relation
    renameAttributes (Relation relationName relationSchema relationInstance relationArity) attrMap =
      do newSchema <- renameSchema relationSchema attrMap
         let newInstance = Set.map (renameTuple (\_ -> True) (\attrKey -> Map.findWithDefault attrKey attrKey attrMap)) relationInstance
         return (Relation relationName newSchema newInstance relationArity)

evalQuery state query@(Select predicate query') =
  do relation <- evalQuery state query'
     case (evalPredicate relation predicate) of 
       Right predicateFunction ->
         return (filterRelation predicateFunction relation)
      
       Left err ->
         Left (ContextualError query (InPredicate predicate) err)
  where
    filterRelation :: (Tuple -> Bool) -> Relation -> Relation
    filterRelation f (Relation _ rSchema rInstance rArity) = 
      let 
        newInstance = Set.filter f rInstance
      in
        Relation Nothing rSchema newInstance rArity

evalQuery state query@(Project attributeKeys query') =
  do relation <- evalQuery state query'
     case (projectRelation attributeKeys relation) of
       Right relation' -> 
         return relation'
       
       Left err ->
         Left (ContextualError query InProyect err)

  where
    projectRelation' :: Schema -> AttributeKey -> Either EvalError (AttributeKey, Domain)
    projectRelation' [] attributeKey = Left (UndefinedAttribute attributeKey)
    projectRelation' ((attributeKey, domain):rest) attributeKey' = 
      if
        checkAttributeKeys attributeKey attributeKey'
      then
        return (attributeKey', domain)
      else
        projectRelation' rest attributeKey'

    projectRelation :: [AttributeKey] -> Relation -> Either EvalError Relation
    projectRelation newAttibutes (Relation _ relationSchema relationInstance _) = 
      do newSchema <- mapM (projectRelation' relationSchema) newAttibutes   
         let newInstance = Set.map (\t -> Map.restrictKeys t (Set.fromList newAttibutes)) relationInstance    
         return (Relation Nothing newSchema newInstance (length newSchema))                      

evalQuery state query@(Binary Union query' query'') =
  do relation  <- evalQuery state query'
     relation' <- evalQuery state query''
     joinRelations relation relation'

  where
    joinRelations :: Relation -> Relation -> EvalResult Relation
    joinRelations (Relation _  relationSchema  relationInstance  relationArity) 
                  (Relation _  relationSchema' relationInstance' relationArity') =
      if
        (relationArity == relationArity') && (checkSchemas relationSchema relationSchema')
      then 
        do let
             attrMapping        = Map.fromList $ zip (map fst relationSchema') (map fst relationSchema)
             relationInstance'' = Set.map (\t -> Map.mapKeys (modifyTuple attrMapping) t) relationInstance'
           return (Relation Nothing relationSchema (Set.union relationInstance relationInstance'') relationArity)
      else
        Left (ContextualError query InQuery IncompatibleSchemas)

evalQuery state query@(Binary Difference query' query'') =
  do relation  <- evalQuery state query'
     relation' <- evalQuery state query''
     differenceRelations relation relation'

  where
    differenceRelations :: Relation -> Relation -> EvalResult Relation
    differenceRelations (Relation _  relationSchema  relationInstance  relationArity) 
                        (Relation _  relationSchema' relationInstance' relationArity') =
      if
        (relationArity == relationArity') && (checkSchemas relationSchema relationSchema')
      then 
        do let
             attrMapping        = Map.fromList $ zip (map fst relationSchema') (map fst relationSchema)
             relationInstance'' = Set.map (\t -> Map.mapKeys (modifyTuple attrMapping) t) relationInstance'
           return (Relation Nothing relationSchema (Set.difference relationInstance relationInstance'') relationArity)
      else
        Left (ContextualError query InQuery IncompatibleSchemas)

evalQuery state query@(Binary CartesianProduct query' query'') = 
  do relation  <- evalQuery state query'
     relation' <- evalQuery state query''
     cartesianProductRelation relation relation'
  where
    cartesianProductRelation :: Relation -> Relation -> EvalResult Relation 
    cartesianProductRelation r@(Relation  _  relationSchema  relationInstance  relationArity) 
                             r'@(Relation _  relationSchema' relationInstance' relationArity') =

      case (checkCartesianProduct r r') of
        Left err -> 
          Left (ContextualError query InQuery err)

        Right True -> 
          let  
            newSchema   = relationSchema ++ relationSchema'
            newInstance = Set.fromList [ Map.union tR tS | tR <- Set.toList relationInstance, tS <- Set.toList relationInstance']
          in
            return (Relation Nothing newSchema newInstance (relationArity + relationArity'))

evalQuery state@(S lastFile relations operators) query@(Custom operatorName queries) =
  case Map.lookup operatorName operators of
    Nothing -> 
      Left (ContextualError query InQuery (UndefinedOperator operatorName))
  
    Just (CustomOperator operatorArity variableNames query') ->
      case operatorArity - (length queries) of
        0 -> do relations' <- mapM (evalQuery state) queries
                let state' = S lastFile (Map.union (Map.fromList (zip variableNames relations')) relations) operators
                case (substituteQuery state' query') of
                  Right query'' -> 
                    evalQuery state' query''
                
                  Left err ->
                    Left (ContextualError query InSubstitution err)

        _ -> 
          Left (ContextualError query InQuery (ArityMismatch operatorName operatorArity (length queries)))

  where
    substituteQuery :: State -> Query -> Either EvalError Query
    substituteQuery _ (Atomic relationName) =
      return (Atomic relationName)

    substituteQuery _ (Variable variableName) =
      return (Variable variableName)

    substituteQuery s (Rename relationName q) =
      do q' <- substituteQuery s q
         return (Rename relationName q')

    substituteQuery s (RenameA attributeKeyMap q) =
      do q'               <- substituteQuery s q
         case (evalQuery s q') of
           Right relation ->
             do attributeKeyMap' <- substituteAttributeKeyMap relation attributeKeyMap
                return (RenameA attributeKeyMap' q')
          
           Left (ContextualError _ _ err) -> 
             Left err

    substituteQuery s (Select predicate q) =
      do q'         <- substituteQuery s q
         case (evalQuery s q') of
           Right relation ->
             do predicate' <- substitutePredicate relation predicate
                return (Select predicate' q')
          
           Left (ContextualError _ _ err) -> 
             Left err

    substituteQuery s (Project attributeKeys q) =
      do q'             <- substituteQuery s q
         case (evalQuery s q') of
           Right relation ->
             do attributeKeys' <- mapM (substituteAttributeKey relation) attributeKeys
                return (Project attributeKeys' q')
          
           Left (ContextualError _ _ err) -> 
             Left err   
    
    substituteQuery s (Binary binaryOp q1 q2) =
      do q1' <- substituteQuery s q1
         q2' <- substituteQuery s q2
         return (Binary binaryOp q1' q2')

    substituteQuery s (Custom opName qs) =
      do qs' <- mapM (substituteQuery s) qs
         return (Custom opName qs')


    substituteAttributeKey :: Relation -> AttributeKey -> Either EvalError AttributeKey
    substituteAttributeKey (Relation _ schema _ _) attribute@(_, attributeName) = 
      case findFirstMatch attributeName schema of
        Just (relationName, _) -> 
          return (relationName, attributeName)

        Nothing -> 
          Left (UndefinedAttribute attribute)


    substituteAttributeKeyMap :: Relation -> AttributeKeyMap -> Either EvalError AttributeKeyMap
    substituteAttributeKeyMap relation attrKeyMap = 
      do keys' <- mapM (substituteAttributeKey relation) (Map.keys attrKeyMap)
         let values = Map.elems attrKeyMap
         return (Map.fromList (zip keys' values))


    substitutePredicate :: Relation -> Predicate -> Either EvalError Predicate
    substitutePredicate relation (PAnd p1 p2) =
      do p1' <- substitutePredicate relation p1
         p2' <- substitutePredicate relation p2
         return (PAnd p1' p2')

    substitutePredicate relation (POr p1 p2) =
      do p1' <- substitutePredicate relation p1
         p2' <- substitutePredicate relation p2
         return (POr p1' p2')

    substitutePredicate relation (PNot p) =
      do p' <- substitutePredicate relation p
         return (PNot p')  
    
    substitutePredicate _ (PBool b) =
      return (PBool b)

    substitutePredicate relation (PCmp cmpOp atom1 atom2) =
      do atom1' <- substituteAtom relation atom1
         atom2' <- substituteAtom relation atom2
         return (PCmp cmpOp atom1' atom2')


    substituteAtom :: Relation -> PredicateAtom -> Either EvalError PredicateAtom
    substituteAtom _ (ConstantValue value) = 
      return (ConstantValue value)

    substituteAtom (Relation _ schema _ _) (AttributeReference relationName attributeName) =
      case findFirstMatch attributeName schema of
        Just (relationName', _) -> 
          return (AttributeReference relationName' attributeName)
        Nothing -> 
          Left (UndefinedAttribute (relationName, attributeName))


--------------------------------------
---- Evaluador de predicados
--------------------------------------
evalPredicate :: Relation -> Predicate -> Either EvalError (Tuple -> Bool)
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

evalPredicate relation (PCmp comparisionOperator predicateAtom predicateAtom') = 
  do atom  <- evalAtom predicateAtom  relation
     atom' <- evalAtom predicateAtom' relation
     case comparisionOperator of
       Equal          -> return (\t -> (atom t) == (atom' t))
       NotEqual       -> return (\t -> (atom t) /= (atom' t))
       LessThan       -> return (\t -> (atom t) <  (atom' t))
       LessOrEqual    -> return (\t -> (atom t) <= (atom' t))
       GreaterThan    -> return (\t -> (atom t) >  (atom' t))
       GreaterOrEqual -> return (\t -> (atom t) >= (atom' t))

  where
    evalAtom :: PredicateAtom -> Relation -> Either EvalError (Tuple -> Value)
    evalAtom (ConstantValue value) _ = 
      return (\_ -> value)
    evalAtom (AttributeReference relationName attributeName) (Relation _ relationSchema _ _) =
      let
        attribute = (relationName, attributeName)
      in
        if
          isAttribute relationSchema attribute
        then
          return (\t ->  (Map.!) t attribute)
        else
          Left (UndefinedAttribute attribute)

