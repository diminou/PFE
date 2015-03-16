# Importation du graphe élagué obtenu par le script R requetesExpension.R

### Recuperation des variables environnement ###
PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)

### Installation des packages ###
source(paste(getwd(), "utils.R", sep="/"))


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
art_cat_csv <- paste(HOME, "/.pfe/article_categories_fr.csv", sep = "/")

category_relations_csv <- paste(HOME, ".pfe/skos_categories_fr.csv", sep = "/")

metier_du_batiment <- paste(HOME, ".pfe/Metier_du_batiment", sep = "/")
service_public <- paste(HOME, ".pfe/Service_public", sep = "/")
type_de_commerce <- paste(HOME, ".pfe/Type_de_commerce", sep = "/")


# Importation de la base de données:
query = "CREATE CONSTRAINT ON (a:article) ASSERT a.title IS UNIQUE"
cypher(db, query)

query = "CREATE CONSTRAINT ON (c:category) ASSERT c.label IS UNIQUE"
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
              LOAD CSV WITH HEADERS FROM \"file:", art_cat_csv,"\" AS row
              MERGE (:article {title:row.article})", sep = "")
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
              LOAD CSV WITH HEADERS FROM \"file:", art_cat_csv, "\" AS row
              MERGE (:category {label:row.categorie})", sep = "")
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
              LOAD CSV WITH HEADERS FROM \"file:", art_cat_csv, "\" AS row
              MATCH (a:article {title: row.article})
              MATCH (cat:category {label: row.categorie})
              MERGE (a)-[:is_under]->(cat)", sep = "")
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
              LOAD CSV WITH HEADERS FROM \"file:", category_relations_csv, "\" AS row
              MATCH (c0:category {label: row.cat0})
              MATCH (c1:category {label: row.cat1})
              MERGE (c0)-[r:relates {type:row.link}]->(c1)", sep = "")
cypher(db, query)

query = "CREATE CONSTRAINT ON (w:word) ASSERT w.stem IS UNIQUE"
cypher(db, query)

saveBddPart <- function(categoriePath,fileName,part){
  path = paste(paste(paste(categoriePath,fileName,sep="/"),part,sep=""),".csv",sep="")
  query = paste("USING PERIODIC COMMIT 1000
                LOAD CSV WITH HEADERS FROM \"file:", path, "\" AS row
                MERGE (:word {stem:row.stem})", sep = "")
  cypher(db, query)
  print(paste(path," imported"))
}

saveBddPartBis <- function(categoriePath,fileName,part){
  path = paste(paste(paste(categoriePath,fileName,sep="/"),part,sep=""),".csv",sep="")
  query = paste("USING PERIODIC COMMIT 1000
                LOAD CSV WITH HEADERS FROM \"file:", path, "\" AS row
                MATCH (a:article {title: row.a.title})
                MATCH (w:word {stem: row.word})
                MERGE (a)-[r:contains {count:row.count}]->(w)", sep = "")
  cypher(db, query)
  print(paste(path," imported"))
}

# Catégorie Métier_du_bâtiment :
saveBddPart(metier_du_batiment,"Metier_du_batiment",0)
saveBddPartBis(metiers_du_batiment,"Metier_du_batiment",0)

# Catégorie Type_de_commerce :
for(i in 0:20){
  saveBddPart(type_de_commerce,"Type_de_commerce",i)
}
for(i in 0:20){
  saveBddPartBis(type_de_commerce,"Type_de_commerce",i)
}

# Catégorie Service_public :
for(i in 0:2){
  saveBddPart(service_public,"Service_public",i)
}
for(i in 0:2){
  saveBddPartBis(service_public,"Service_public",i)
}

