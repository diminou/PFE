### Recuperation des variables environnement ###
PFE<-Sys.getenv("PFE")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)


### Importation des packages ###
library(RNeo4j)
library(compiler)
library(R.oo)
library(Rcpp)
library(tm)
library(SnowballC)
library(RTextTools)
source(paste(PFE, "ESA.R", sep = "/"))

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

retrieveShortestPath <- function(title1, title2) {
  query <- paste("match (a:article {title:'", escapeApostrophes(title1),
                 "'}), (b:article {title:'",
                 escapeApostroples(title2),
                 "'}) match p = allShortestPaths((a)-[:linked *]-(b)) return nodes(p).code")
  return(cypher(db, query))
}

retrieveMostPertinentPath <- function(title1, title2) {
  query <- paste("match (a:article {title:'", escapeApostrophes(title1),
                 "'}), (b:article {title:'",
                 escapeApostroples(title2),
                 "'}) match p (a)-[:linked *1..4]-(b) return p, reduce(acc = 1.0, r in relationships(p) | acc * tofloat(r.pertinence)) as weight order by weight desc")
  return(cypher(db, query))
}
