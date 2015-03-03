PFE<-Sys.getenv("PFE")
source(paste(PFE, "requetesNeo4j.R", sep = "/"))
# Libaiies utiles pour les fction utilsées venant de requetesNEo4j.R et import.R 
library(tm)
library(SnowballC)
library(RTextTools)
library(RNeo4j)
#install.packages("devtools")
# devtools::install_github("nicolewhite/RNeo4j")

db = startGraph("127.0.0.1:7474/db/data/")
#  tf time frequency du mot dans le doc, nb nb de doc, df document frequency du mot
TFIDF <- function(tf, nb, df){
  return (tf *log(nb/df))
}

# tfidf : tfidf de la fct pcdte, r : nb de mot dans le doc (= longueur du doc)
TFIDF_norm <- function(tfidf, r){
 return (tfidf/sqrt(r *(tfidf*tfidf)))
}



sim_cos <- function(v1, v2){
  num <- 0
  part1 <-  0
  part2 <-  0
  for(i in 1:length(v1)){
    num= num + v1[i]*v2[i]
    part1 = part1 + v1[i]*v1[i]
    part2 = part2 + v2[i]*v2[i]
  }
  
  denom = sqrt(part1)*sqrt(part2)
  res= (num /denom)
  return (res)
}



# nb de doc 
nbDocs <- function(){
  q = "match(a:article) return count(a)"
  return(cypher(db, q)[1,1])
  
}

# retourne le vecteur semantique du mot mot (vecteur numérique)
MotSemantInterp <- function(mot){
  ArtFrWor=getArticlesFromWord(mot)
  n = nrow(ArtFrWor)
  df = n 
  nDocs = nbDocs()
  s <- rep(0,n)
  for(i in 1:n){
    tf = 1 + log(as.numeric(ArtFrWor[i,2])) 
    s[i] <- TFIDF(tf, nDocs, df)
  }
  return(s)
}



