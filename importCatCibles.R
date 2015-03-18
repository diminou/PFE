# insersion des catégories cibles sous la forme d'un nouveau noeud appelé 'target'
PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)

library(RNeo4j)

source(paste(getwd(), "ESA.R", sep="/"))

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

getCategoriesCibles <- function(){
  filePath <- paste(HOME,".pfe/categoriesCibles.csv",sep="/")
  data <- read.csv(filePath)
  Code_rubrique_AN9 <- data$Code_rubrique_AN9
  Label_Categorie_cible <- paste(paste(data$Lib_rubrique_AN8,data$LibSegment30,sep=" "), "",sep=" ")#data$LibSegment5
  return(categoriesCibles <- data.frame(Code_rubrique_AN9,Label_Categorie_cible))
}
categoriesCibles <- getCategoriesCibles()

query = "CREATE CONSTRAINT ON (t:target) ASSERT t.code IS UNIQUE"
cypher(db, query)

addCatCible <- function(label,code){
  resultsESA <- cos_sim_req_doc(label)
  query = paste(paste("merge (:target {code:\"",code,sep=""),"\"})",sep="")
  cypher(db, query)
  
  for(i in 1:length(resultsESA[[1]])){
    query = paste("MATCH (a:article {title:\"",resultsESA[[1]][i],"\"})
              MATCH (t:target {code:\"",code,"\"})
              MERGE (a)-[r:linked]->(t) ",
              "on create set r.pertinence = '",
              resultsESA[[2]][i],
              "'",
              sep = "")
    cypher(db, query)
  }
}
for(i in 1:dim(categoriesCibles)[1]){
  tryCatch(addCatCible(as.vector(categoriesCibles$Label_Categorie_cible[i]),categoriesCibles$Code_rubrique_AN9[i]))
}


