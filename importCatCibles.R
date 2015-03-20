# insersion des catégories cibles sous la forme d'un nouveau noeud appelé 'target'
PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)

library(RNeo4j)

source(paste(PFE, "ESA.R", sep="/"))

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

## Suppression des categories cibles deja importees
clear <- function() {
  query <- "match (t:target)-[r]-()
            with t, r delete t, r"
  cyper(db, query)
}
clear()

### nombre de liens a garder entre la categorie cible et les articles
globalLinkNum <- 20

### fraction de pertinence sommaire a garder entre la categorie cibles et les articles
globalLinkQuant <- 0.05



getCategoriesCibles <- function(){
  filePath <- paste(HOME,".pfe/categoriesCibles.csv",sep="/")
  data <- read.csv(filePath)
  Code_rubrique_AN9 <- data$Code_rubrique_AN9
  Label_Categorie_cible <-  data$Lib_rubrique_AN8#paste(paste(data$Lib_rubrique_AN8,data$LibSegment30,sep=" "), "",sep=" ")#data$LibSegment5
  return(categoriesCibles <- data.frame(Code_rubrique_AN9,Label_Categorie_cible))
}
categoriesCibles <- getCategoriesCibles()

query = "CREATE CONSTRAINT ON (t:target) ASSERT t.code IS UNIQUE"
cypher(db, query)

getFilteredESA <- function(query) {
  resultsESA <- cos_sim_req_doc(query)
  tabResults <- data.frame(cbind(resultsESA[[1]], resultsESA[[2]]))
  tabResults <- tabResults[tabResults[,2] != Inf,]
  tabResults[, 2] <- as.numeric(tabResults[,2])
  total <- sum(tabResults[,2])
  tabResults[, 3] <- cumsum(tabResults[, 2])
  return(tabResults[tabResults[, 3] <= total * globalLinkQuant, c(1, 2)])
}

addCatCible <- function(label,code){
  resultsESA <- getFilteredESA(label)
  query = paste(paste("merge (:target {code:\"",code,sep=""),"\"})",sep="")
  cypher(db, query)
  for(i in 1:dim(resultsESA)[1]){
    query = paste("MATCH (a:article {title:\"",resultsESA[i, 1],"\"})
              MATCH (t:target {code:\"",code,"\"})
              MERGE (a)-[r:linked]->(t) ",
              "on create set r.pertinence = '",
              resultsESA[i, 2],
              "'",
              sep = "")
    
    tryCatch(cypher(db, query))
  }
}
for(i in 1:dim(categoriesCibles)[1]){
  tryCatch(addCatCible(as.vector(categoriesCibles$Label_Categorie_cible[i]),categoriesCibles$Code_rubrique_AN9[i]))
}


