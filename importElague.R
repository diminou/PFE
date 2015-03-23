# Importation du graphe élagué obtenu par le script R requetesExpension.R

### Recuperation des variables environnement ###
PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)

### Installation des packages ###
#source(paste(getwd(), "utils.R", sep="/"))


### Importation des packages ###
library(RNeo4j)
library(compiler)
library(R.oo)
library(Rcpp)
library(tm)
library(SnowballC)
library(RTextTools)

### Compilation et importation de import.cpp ###
sourceCpp(paste(PFE, "import.cpp", sep="/"))
getBracketedExps("sdf<asdfasdfa>sdfg<asdf>asda.")
getResource("http://blabla.xxx/resource/Catégorie:testé")
print(outputString())
print(sendStringVector())

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

### ALERTE! cette ligne va effacer toutes les donnees dans Neo4j ###
### Initialisation de la base de donnees ###
#clear(db)


#######################TSET#########################
# testFile <- "/home/divanov/.pfe/test"
# conn <- file(testFile, open = "r", encoding = "utf-8")
# line <- readLines(conn, 1)
# line2 <- readLines(conn, 1)
# line3 <- readLines(conn, 1)
# Encoding(line3)
# Encoding(line) <- "utf-8"
# Encoding(line2) <- "latin-1"
# enc2utf8(line3)
# print(line)
# iconv(line3, from = "ISO-8859-1", to = "UTF-8", toRaw = T)
# iconv(line3, from = "UTF-8", to = "ISO-8859-1", toRaw = T)
# line2 <- enc2utf8(iconv(line2, from = "UTF-8", to = "ISO-8859-1"))
# print(line2)
# print(line3)
# print(toSpace(line, "'"))
# print(wordStem(line, language = "french"))
# close(conn)

####################################################

# Définition des chemins des fichiers .csv
#art_cat_csv <- paste(HOME, "/.pfe/article_categories_fr.csv", sep = "/")

category_relations_csv <- paste(HOME, ".pfe/skos_categories_fr.csv", sep = "/")

metier_du_batiment <- paste(HOME, ".pfe/Métier_du_bâtiment", sep = "/")
service_public <- paste(HOME, ".pfe/Service_public", sep = "/")
type_de_commerce <- paste(HOME, ".pfe/Type_de_commerce", sep = "/")


# Importation de la base de données:
query = "CREATE CONSTRAINT ON (a:article) ASSERT a.title IS UNIQUE"
cypher(db, query)

query = "CREATE CONSTRAINT ON (c:category) ASSERT c.label IS UNIQUE"
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
              LOAD CSV WITH HEADERS FROM \"file:", category_relations_csv, "\" AS row
              MERGE (c0:category {label: row.cat0})
              MERGE (c1:category {label: row.cat1})",sep = "")
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
              LOAD CSV WITH HEADERS FROM \"file:", category_relations_csv, "\" AS row
              MATCH (c0:category {label: row.cat0})
              MATCH (c1:category {label: row.cat1})
              MERGE (c0)-[r:relates]->(c1)
              ON CREATE SET r.type = row.link",sep = "")
cypher(db, query)


query = "CREATE CONSTRAINT ON (w:word) ASSERT w.stem IS UNIQUE"
cypher(db, query)


buildQuery <- function(categoryPath, filename) {
  query <- paste("using periodic commit 1000 ",
                 "load csv with headers from \"file:",
                 paste(categoryPath, filename, sep = "/"),
                 "\" as row ",
                 "match (cat:category {label: row.label}) ",
                 "merge (a:article {title: row.title}) ",
                 "merge (cat)<-[:is_under]-(a) ",
                 "merge (w:word {stem: row.stem}) ",
                 "merge (a)-[r:contains]->(w) ",
                 "set  r.count = row.count ",
                 sep = '')
  return(query)
}

importTrimmedGraph <- function(categoryPath) {
  vectFiles <- list.files(categoryPath)
  
  queries <- sapply(vectFiles, buildQuery, categoryPath = categoryPath)
  sapply(queries, function(x){ print(x)
                               cypher(db, x)
                               print("done")})
}

importTrimmedGraph(type_de_commerce)
importTrimmedGraph(metier_du_batiment)
importTrimmedGraph(service_public)



