PFE<-Sys.getenv("PFE")
source(paste(PFE, "requetesNeo4j.R", sep = "/"))
# install.packages("devtools")
# devtools::install_github("nicolewhite/RNeo4j")
# Libaiies utiles pour les fction utilsées venant de requetesNEo4j.R et import.R 
library(tm)
library(SnowballC)
library(RTextTools)
library(RNeo4j)


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

nombreDoc <- nbDocs()

# retourne la liste(nom des doc dans lequel le mot est présent, vecteur semantique du mot) 
MotSemantInterp <- function(mot){
  ArtFrWor=getArticlesFromWord(wordStem(mot, language = "french"))
  n = nrow(ArtFrWor)
  df = n 
  nDocs = nombreDoc
  s <- rep(0,n)
  nom <- rep("", n)
  for(i in 1:n){
    tf = 1 + log(as.numeric(ArtFrWor[i,2])) 
    nom[i] <- ArtFrWor[i,1]
    s[i] <- TFIDF(tf, nDocs, df)
  }
  
# On trie les 2 vecteurs par ordre de score décroissant  
  ordre <- order(s, decreasing = TRUE)
  s <- s[ordre]
  nom <- nom[ordre]


  listeNomScore <- PrepSlidWind(vecTrie = s,nomTrie = nom, length = 100, pourcent = 0.05)

  # ce qu'on faisait sans la sliding window
#   listeNomScore <- list(nom, s)
  return(listeNomScore)
}


# Application de la fonction MotSemantInterp a une requete (après traitement de cette dernière)
InterpSemRequete <- function(req){
  req <- removePunctuation(req)
  req <- tolower(req)
  req <- removeWords(req, stopwords("french"))
  req <- stripWhitespace(req)
  words <- unlist(strsplit(req, "\\s"))
  words <- wordStem(words, language = "french")

  lis <- lapply(words,MotSemantInterp )

  return(lis)
}

# retourne true ssi la sliding window garde la séquence (<5% highest score), sinon retourne false
SlidingWindow <- function(seq, highScore, length,  pourcent){
  bool = FALSE
  diff = seq[1] -seq[length] 
  print(diff)
  print(highScore)
  print(pourcent)
  if(diff < (pourcent * highScore)){
    bool= TRUE
  }else {
    bool = FALSE
  }
  return (bool)
}

# on prépare les données pour la méthode de sliding window et on boucle en faisant varier glisser notre fenetre
# renvoit la liste(nom, score) ppour les scores "pertinents" sans grand saut
PrepSlidWind <- function(vecTrie, nomTrie, length, pourcent){
  
  if(length> length(vecTrie)){
    lis <- list(vecTrie, nomTrie)
  }else {
    highScore = vecTrie[1]
    bool = TRUE
    i =1
    resNom <- nomTrie[1]
    res <-  vecTrie[1]
    max <- length(vecTrie)
    while(bool== TRUE){
      seq <- vecTrie[(1 +i) :(length+i)]
      bool <- SlidingWindow(seq,highScore, length, pourcent)
      if(bool== TRUE){
        res[i+1] <- vecTrie[i+1]
        resNom[i+1] <- nomTrie[i+1]
      }
      
      if(i == max -length ){
        bool = FALSE
      }
      i <- i+1
    }
    lis <- list(resNom, res)
  }

  
  return(lis)
}




# PrepSlidWind(c(17,16,15,14,3,2,1), c("17","16","15","14","3","2","1"), 2, 0.5)
 
 

