# TP-FINAL-ALP

## Introducción
Hola! Este README es un documento complementario al PDF de la consigna del TP. Se recomienda primero leer el PDF, ya que el objetivo de este documento es aclarar algunos detalles técnicos que van a ser útiles para resolver el trabajo.

### Stack
Para este TP vamos a usar [**Stack**](https://docs.haskellstack.org/), una herramienta sencilla para desarrollar proyectos en Haskell. Stack tiene muchas utilidades, pero ahora nos vamos a concentrar sus funciones básicas.

Antes que nada, puede que tengas que instalarlo. En [1](https://docs.haskellstack.org/en/stable/README/#how-to-install) hay guías de instalación para distintas plataformas.

Stack se encarga de instalar la versión correcta de GHC, instalar los paquetes necesarios y compilar el proyecto. Para las primeras dos, basta con abrir una terminal en el directorio `Relational_Algebra_EDSL` y ejecutar:
```
stack setup
```
Esto puede demorar un rato porque se encarga de descargar e instalar la verisón correcta de GHC. Este comando solo se debería tener que ejecutar una única vez. Al terminar esto, está todo listo para compilar el proyecto, que se hace con:
```
stack build
```
Este es el comando que van a tener que usar para compilar el proyecto cada vez que lo modifiquen.

### Estructura del código
La estructura  del proyecto es la siguiente:
```
.
├── app
│   └── Main.hs
├── src
│   ├── Common.hs
│   ├── Eval.hs
│   ├── Parser.hs
│   └── PrettyPrinter.hs
├── Tests
│   ├── 01_setup.ra
│   ├── 02_basic_operations.ra
│   ├── 03_cartesian_and_compositions.ra
│   ├── 04_custom_operators.ra
│   ├── 05_rename_attributes.ra
│   ├── 06_edge_cases.ra
│   ├── 07_complex_queries.ra
│   ├── 08_ddl_and_remove.ra
│   └── 09_errors.ra
├── README.md
├── Setup.hs
├── TP-FINAL-ALP.cabal
├── package.yaml
├── stack.yaml
├── stack.yaml.lock
├── CHANGELOG.md
└── LICENSE
```

### ¿Cómo ejecutarlo?

Una vez compilado el proyecto, se puede correr el ejecutable definido en `app/Main.hs` sobre un archivo `.lis` haciendo:
```
stack exec TP-FINAL-ALP-exe -- PATH_TO_SOURCE
```

Por ejemplo, para imprimir el programa `01_setup.ra` del directorio `Tests`, se debería ejecutar:
```
stack exec TP-FINAL-ALP-exe -- Tests/01_setup.ra
```

### ¿Y GHCi?
Si quieren usar `GHCi` para probar alguna de las funciones que definieron, pueden iniciar una sesión del intérprete con todos los módulos del directorio `src` ya cargados haciendo:
```
stack ghci
```

### Modo de uso del intérprete
El usuario puede interactuar con el intérprete mediante el uso de comandos. Los comandos son:

  - `:help` o `:?`: Muestra por pantalla un mensaje de ayuda al usuario. Este mensaje contiene información sobre los comandos disponibles 
    y cómo usarlos.
  - `:quit`: Sale del intérprete.
  - `:load <file_name>`: Carga el archivo `<file_name>` en el intérprete. La extención del archivo debe ser `.ra`.
  - `:reload`: Carga el último archivo cargado por el usuario.  
  - `:list_relations`: Muestra los nombres de las relaciones en dentro del entorno del intérprete.
  - `:list_operators`: Muestra los nombres de los operadores dentro del entorno del intérprete.
  - `:print_relation <relation_name>`: Imprime la relación `<relation_name>` por pantalla. La relación debe estar en el entorno del intérprete.
  - `:print_operator <operator_name>`: Imprime el operador `<operator_name>` por pantalla. El operador debe estar en el entorno del intérprete.
  - `:describe_relation <relation_name>`: Describe el esquema de una relación. La relación debe estar en el entorno del intérprete.
  
El usuario tambien puede interactuar con el intérprete escribiendo sentencias del EDSL separadas por ';'. Por ejemplo:

```
CREATE nombre_tabla {col1 tipo1, col2 tipo2, ...};
```