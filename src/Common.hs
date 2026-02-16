module Common where
  
  import qualified Data.Map as Map
  import qualified Data.Set as Set

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)


  -------------------------------------------
  -- RESULTADOS DE PARSEAR 
  -------------------------------------------
  data ParseResult a = Failed String | Ok a deriving Show


  -------------------------------------------
  -- TIPOS COMPARTIDOS
  -------------------------------------------
  -- Sinonimos de tipos
  type RelationName = String                   -- Nombre de las relaciones
  type OperatorName = String                   -- Nombre de los operadores
  type AttrName     = String                   -- Nombre de los atributos
  type VariableName = String                   -- Nombre para las variables de los operadores custom
  type AttrKey      = (RelationName, AttrName) -- Claves para el esquema



  ------------------------------------
  -- ENTORNOS
  ------------------------------------
  type RelationsEnvironment = Map.Map RelationName Relation

  type OperatorsEnvironment = Map.Map OperatorName CustomOperator 

  data State = S
    { lfile   :: String
    ,     -- Ultimo archivo cargado (para hacer "reload")
    rs :: RelationsEnvironment  
          -- Entorno con las relaciones definidas
    ,
    ops :: OperatorsEnvironment
          -- Entorno con los operadores definidos
    }



  -------------------------------------------
  -- OPERACIONES CUSTOM
  -------------------------------------------
  -- Aridad de un operador
  type OperatorArity = Int

  -- Operadores custom o definidos por el usuario
  data CustomOperator = CustomOperator OperatorArity [VariableName] Query deriving (Show)



  -------------------------------------------
  -- CONSULTAS DEL ÁLGEBRA RELACIONAL
  -------------------------------------------
  -- Operadores binarios en relaciones
  data BinaryOperator =  Union
                       | Difference
                       | CartesianProduct
                       deriving (Show)


  -- Comparadores en predicados 
  data ComparisonOperator =  Equal
                           | NotEqual 
                           | LessThan 
                           | LessOrEqual 
                           | GreaterThan 
                           | GreaterOrEqual
                           deriving (Show)
   

  -- Definición de una átomo en predicados
  data PredicateAtom =  ConstantValue       Value
                      | AttributeReference  RelationName AttrName
                      deriving (Show)


  -- Definición de un predicado
  data Predicate =  PAnd  Predicate          Predicate
                  | POr   Predicate          Predicate
                  | PNot  Predicate
                  | PBool Bool
                  | PCmp  ComparisonOperator PredicateAtom PredicateAtom
                  deriving (Show)


  -- Definición de una consulta del álgebra relacional
  data Query =  Atomic   RelationName
              | Variable VariableName
              | Rename   RelationName         Query
              | Select   Predicate            Query
              | Binary   BinaryOperator       Query Query
              | Project  [AttrKey]            Query
              | RenameA  [(AttrKey, AttrKey)] Query
              | Custom   OperatorName         [Query]
              deriving (Show)


  -------------------------------------------
  -- RELACIONES DEL ÁLGEBRA RELACIONAL
  -------------------------------------------
  -- Aridad de una relación
  type RelationArity = Int

  -- Definición de los posibles dominios que puede tener un atributo
  data Domain =  IntDomain
               | StringDomain
               | DoubleDomain
               deriving (Eq, Ord, Show)


  -- Definición de los valores que puede tomar una tupla de una relación
  data Value =  IntValue    Int
              | StringValue String
              | DoubleValue Double
              deriving (Eq, Ord, Show)


  -- Sinonimos de tipos
  -- Esquema
  type Schema    = [(AttrKey, Domain)]

  -- Tuplas
  type Tuple     = Map.Map (RelationName, AttrName) Value 

  -- Instancia
  type Instance  = Set.Set Tuple


  -- Definición de una relación
  data Relation = Relation (Maybe RelationName) Schema Instance RelationArity    deriving (Show)



  -------------------------------------------
  -- COMANDOS DEL EDLS
  -------------------------------------------
  data EDSLCommand =  CreateRelation   RelationName (Either Relation Query)
                    | DropRelation     RelationName
                    | InsertTuple      RelationName [Tuple]
                    | RemoveTuple      RelationName Predicate
                    | DefineOperator   OperatorName CustomOperator
                    | UndefineOperator OperatorName 
                    | QueryStatement   Query
                    | EUnknown
                    deriving (Show)



  -------------------------------------------
  -- COMANDOS DEL INTÉRPRETE
  -------------------------------------------
  type FileName = String

  data ICommand =  Help
                 | Quit
                 | RecompileFile 
                 | ListRelations
                 | ListOperators
                 | IUnknown
                 | CompileFile      FileName
                 | PrintRelation    RelationName
                 | PrintOperator    OperatorName
                 | DescribeRelation RelationName
                 | EvalStatements   [(EDSLCommand, Int)]
                 deriving (Show)