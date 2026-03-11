{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Map                      as Map
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Data.Set                      as Set
import           Control.Monad.Trans
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render )
                                                

import           Common
import           PrettyPrinter
import           Parser
import           Eval
---------------------
--- Interpreter
---------------------

iname, iprompt :: String
iname           = "Álgebra Relacional"
iprompt         = "RA> "


iCommands :: [([String], String, (String -> ICommand), String)]
iCommands =
      [([":list_relations"]    , ""               ,(const ListRelations) ,"Ver los nombres de las relaciones en scope")
      ,
       ([":list_operators"]    , ""               ,(const ListOperators) ,"Ver los nombres de los operadores en scope")
      ,
       ([":load"]              ,"<file>"          ,CompileFile           ,"Cargar un programa desde un archivo")
      ,
       ([":reload"]            ,"<file>"          ,(const RecompileFile) ,"Volver a cargar el último archivo")
      ,
       ([":print_relation"]    ,"<relation_name>" ,PrintRelation         ,"Imprime la instancia actual de una relación")
      ,
       ([":print_operator"]    ,"<operator_name>" ,PrintOperator         ,"Imprime la definición de un operador")
      ,
       ([":describe_relation"] ,"<relation_name>" ,DescribeRelation      ,"Describe el esquema de una relación")
      ,
       ([":quit"]              ,""                ,(const Quit)          ,"Salir del intérprete")
      , 
       ([":help", ":?"]        ,""                ,(const Help)          ,"Mostrar esta lista de comandos")
      ]


ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing


main :: IO ()
main = runInputT defaultSettings main'


main' :: InputT IO ()
main' = interpreter (S "" Map.empty Map.empty)


interpreter :: State -> InputT IO ()
interpreter state = 
  do lift $ putStrLn (auxiliarMessage)
     interpreter' state

  where
    auxiliarMessage :: String
    auxiliarMessage = "Intérprete de " 
                      ++ iname 
                      ++ ".\n" 
                      ++ "Escriba :help para recibir ayuda."
    
    interpreter' :: State -> InputT IO ()
    interpreter' st = 
      do mx <- MC.catch (getInputLine iprompt) (lift . ioExceptionCatcher)
         case mx of
           Nothing    -> return ()
           Just ""    -> interpreter' st
           Just input -> 
            do command <- interpretInput input
               result  <- handleCommand st command
               case result of
                 Nothing  -> return ()
                 Just st' -> interpreter' st'


interpretInput :: String -> InputT IO ICommand
interpretInput input = 
  if 
    isPrefixOf ":" input
  then 
    interpretCommand input
  else 
    interpretQuery input 


interpretQuery :: String -> InputT IO ICommand
interpretQuery input = 
  do parseResult <- parseIO "" parserStatements input
     case parseResult of
       Nothing             -> return (EvalStatements [])
       Just edlsStatements -> return (EvalStatements edlsStatements)


interpretCommand :: String -> InputT IO ICommand
interpretCommand input = 
  do 
    let 
      (cmd, t') = break isSpace input
      t         = dropWhile isSpace t'
      -- Vemos con que comandos matchea
      matching = filter (\(cs, _, _, _) -> any (isPrefixOf cmd) cs) iCommands

    case matching of
      [] -> 
        do lift $ putStrLn (unknownCommandMessage cmd)
           return IUnknown

      [(_, _, f, _)] -> return (f t)

      _ -> 
        do lift $ putStrLn (ambiguousCommandMessage matching)
           return IUnknown
  where
    unknownCommandMessage :: String -> String
    unknownCommandMessage command = "Comando desconocido '" 
                                    ++ command 
                                    ++ "'. Escriba :? para recibir ayuda."

    ambiguousCommandMessage :: [([String], String, (String -> ICommand), String)] -> String
    ambiguousCommandMessage matchedCommands = "Comando ambigüo, podría ser "
                                              ++ concat (intersperse ", " [ head cs | (cs, _, _, _) <- matchedCommands ])
                                              ++ "."


handleCommand :: State -> ICommand -> InputT IO (Maybe State)
handleCommand _ Quit = 
  do lift $ (putStrLn closeInterpreterMsg)
     return Nothing
  where
    closeInterpreterMsg :: String
    closeInterpreterMsg = "Cerrando intérprete"
handleCommand state Help = 
  do lift $ (putStrLn helpMessage)
     return (Just state)
  where
    helpMessageAuxFunction :: ([String], String, (String -> ICommand), String) -> String
    helpMessageAuxFunction (commads, arguments, _, description) =
      let 
        c' = (map (++ if null arguments then "" else " " ++ arguments) commads)
        ct = concat (intersperse ", " c')
      in
        "\t" ++ ct ++ replicate ((36 - length ct) `max` 2) ' ' ++ description                                                           

    helpMessage :: String
    helpMessage = "Lista de comandos: \n"
                  ++ unlines (map helpMessageAuxFunction iCommands)
handleCommand state@(S _ relations _) ListRelations = 
  do lift $ putStrLn (printDefinedRelations (Map.keys relations))
     return (Just state)

  where 
    printDefinedRelations :: [RelationName] -> String
    printDefinedRelations rns = "Lista de relaciones definidas:\n" 
                                ++ unlines (map ("\t" ++ ) rns) 
handleCommand state@(S _ _ operators) ListOperators = 
  do lift $ putStrLn (printDefinedOperators (Map.keys operators))
     return (Just state)

  where
    printDefinedOperators :: [OperatorName] -> String
    printDefinedOperators ons = "Lista de operadores definidos:\n"
                                ++ unlines (map ("\t" ++ ) ons)
handleCommand state@(S lastFile _ _) RecompileFile  = 
  do result <- compileFile state lastFile
     case result of
       Nothing -> return (Just state)

       Just state' -> 
         return (Just state')  
handleCommand state (CompileFile fileName)          = 
  do result <- compileFile state fileName
     case result of
       Nothing ->
         return (Just state)

       Just state' -> 
         return (Just (state' {lfile = fileName}))
handleCommand state@(S _ relations _) (PrintRelation relationName) = 
  do case Map.lookup relationName relations of
       Nothing       -> lift $ putStrLn "Relación inexistente"
       Just relation -> lift $ putStrLn (render (ppRelation relation))                   
     return (Just state)
handleCommand state@(S _ _ operators) (PrintOperator operatorName) =
  do case Map.lookup operatorName operators of
       Nothing       -> lift $ putStrLn "Operador inexistente"
       Just operator -> lift $ putStrLn (render (ppOperator operatorName operator))
     return (Just state)
handleCommand state@(S _ relations _) (DescribeRelation relationName) = 
  do case Map.lookup relationName relations of
       Nothing       -> lift $ putStrLn "Relación inexistente"
       Just relation -> lift $ putStrLn (render (ppSchema relation relationName))
     return (Just state)
handleCommand state (EvalStatements statements) = (handleEDLSStatements state statements)
handleCommand state IUnknown = return (Just state)


compileFile :: State -> FileName -> InputT IO (Maybe State)
compileFile state fileName = 
  do lift $ putStrLn ("Abriendo " ++ fileName ++ "...")
     let 
       fileName' = reverse (dropWhile isSpace (reverse fileName))
     
     fileContent    <- lift $ MC.catch (readFile ("Tests/" ++ fileName')) (errorFunction fileName)
     parseResult    <- parseIO fileName' parserStatements fileContent
 
     case parseResult of
       Nothing             -> 
         return (Just state)

       Just edlsStatements -> 
         handleEDLSStatements state edlsStatements
  where
    errorFunction :: String -> IOException -> IO String
    errorFunction fName err = 
      do let 
           err' = show err
         hPutStr stderr  ("No se pudo abrir el archivo " ++ fName ++ ": " ++ err' ++ "\n")
         return ""


parseIO :: String -> (String -> String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO fileName parser fileContent = 
  lift $ case parser fileName fileContent of 
           Failed err -> 
             do putStrLn (fileName ++ ":" ++ err)
                return Nothing

           Ok result  -> 
             return (Just result)


handleEDLSStatements :: State -> [(EDSLCommand, Int)] -> InputT IO (Maybe State)
handleEDLSStatements state []                                                = return (Just state)
handleEDLSStatements state@(S lastFile relations operators) (edlsCommand:cs) =
  case edlsCommand of
    ((CreateRelation relationName (Left relation)), _) -> 
      let 
        relations' = Map.insert relationName relation relations
        state'     = (S lastFile relations' operators)
      in
        (handleEDLSStatements state' cs)

    ((CreateRelation relationName (Right query)), line) -> 
      case evalQuery state query of
        Left err -> 
          do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Consulta inválida. " ++ (render (ppError err)) ++ "\n")
             handleEDLSStatements state cs
        
        Right relation ->
          do let 
               relations' = Map.insert relationName relation relations
               state'     = (S lastFile relations' operators)
             handleEDLSStatements state' cs
    
    ((DropRelation relationName), line) -> 
      case Map.lookup relationName relations of
        Nothing -> 
          do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Relación inexistente\n")
             handleEDLSStatements state cs

        _       ->
          do let 
               relations' = Map.delete relationName relations
               state'     = (S lastFile relations' operators)
             handleEDLSStatements state' cs
    
    ((InsertTuple relationName tuples), line) -> 
      case Map.lookup relationName relations of
        Nothing -> 
          do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Relación inexistente\n")
             handleEDLSStatements state cs

        Just (Relation rName rSchema rInstance rArity) ->
          do let 
               rInstance' = Set.union rInstance (Set.fromList tuples)
               relation'  = Relation rName rSchema rInstance' rArity
               relations' = Map.insert relationName relation' relations
               state'     = S lastFile relations' operators
             handleEDLSStatements state' cs

    ((RemoveTuple relationName predicate), line) ->
      case Map.lookup relationName relations of
        Nothing ->
          do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Relación inexistente\n")
             handleEDLSStatements state cs
        
        Just r@(Relation rName rSchema rInstance rArity) ->
          case (evalPredicate r predicate) of 
            Left err -> 
              do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Predicado inválido." ++ (render (ppEvalError err)))
                 handleEDLSStatements state cs

            Right predicateFunction ->
              do let
                   rInstance' = Set.filter (\t -> not (predicateFunction t)) rInstance
                   relation'  = Relation rName rSchema rInstance' rArity
                   relations' = Map.insert relationName relation' relations
                   state'     = (S lastFile relations' operators)
             
                 handleEDLSStatements state' cs
    
    ((DefineOperator operatorName operator), _) ->
      do let 
           operators' = Map.insert operatorName operator operators
           state'     = (S lastFile relations operators')
         handleEDLSStatements state' cs
    
    ((UndefineOperator operatorName), line) -> 
      case Map.lookup operatorName operators of
        Nothing -> 
          do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Operador inexistente\n")
             handleEDLSStatements state cs

        _ ->
          do let
               operators' = Map.delete operatorName operators
               state'     = (S lastFile relations operators')
             handleEDLSStatements state' cs

    ((QueryStatement query), line) ->
      do case (evalQuery state query) of
           Left err       -> lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": " ++ (render (ppError err)) ++ "\n")              
           Right relation -> lift $ putStrLn (render (ppRelation relation))
         handleEDLSStatements state cs
        
    (EUnknown, line) -> 
      do lift $ putStrLn ("Error en la línea " ++ (show line) ++ ": Comando desconocido\n")
         handleEDLSStatements state cs
      

