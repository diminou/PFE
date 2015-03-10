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
#   print("tf")
#   print(tf)
#   print("nb")
#   print(nb)
#   print("df")
#   print(df)
  return (tf *log(nb/df))
}

# tfidf : tfidf de la fct pcdte, r : nb de mot dans le doc (= longueur du doc)
TFIDF_norm <- function(tfidf, r){
 return (tfidf/sqrt(r *(tfidf*tfidf)))
}


carre <- function(x){
  return(x*x)
}


# calcul la similarité coinus enter 2 vecteurs v1 et v2 de même longueur (on prendra le tfidf dans noter cas.)
sim_cos <- function(v1, v2){
  num <- 0
  part1 <-  0
  part2 <-  0

  num = sum(v1,v2)
  part1 = sum(carre(v1))
  part2 = sum(carre(v2))
  
  denom = (sqrt(part1)*sqrt(part2))
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






firstCol <- function(datafr) {
 print(nrow(datafr))
 print(class(datafr))
  return(datafr[1])
}


getArtNamesFromWord <- function(word) {
  return (firstCol(getArticlesFromWord(word)))
}



# retourne la liste (set) des documents associés a une requete (sous la forme d'un vecteur)
setDocReq <- function(req){
  req <- removePunctuation(req)
  req <- tolower(req)
  req <- removeWords(req, stopwords("french"))
  req <- stripWhitespace(req)
  words <- unlist(strsplit(req, "\\s"))
  words <- wordStem(words, language = "french")
 
  wordsUnique <- unique(words)
  print(getArticlesFromWord(wordsUnique[1]))
  print(wordsUnique[1])
  print(getArtNamesFromWord(wordsUnique[1]))
  listeDocUnique <-  unique(lapply(wordsUnique, getArtNamesFromWord))
  

#   listeDocUnique <- c(getArticlesFromWord(words[1])[,1])
#   if(length(wordsUnique)>1){
#     for(i in 2:length(wordsUnique)){
#       tempo <- c(getArticlesFromWord(words[i])[,1])
#       listeDocUnique <- union(listeDocUnique, tempo)
#     }
#   }
  
  return(listeDocUnique)
}


# calcul le TFIDF entre un mot stematisé et une requete
TFIDF_req <- function(word, req){
  req <- removePunctuation(req)
  req <- tolower(req)
  req <- removeWords(req, stopwords("french"))
  req <- stripWhitespace(req)
  words <- unlist(strsplit(req, "\\s"))
  words <- wordStem(words, language = "french")
  
  tf <-0 # nb de fois du word dans req
  for(i in 1:length(words)){
    if(words[i]==word){
      tf <- tf+1
    }
  }
#   tf <- tf/length(words)
  if(tf >0){
    tf <- 1 + log(tf)
  }
  
  nDocs = nombreDoc
  ArtFrWor=getArticlesFromWord(wordStem(word, language = "french"))
  df = nrow(ArtFrWor)

  res =0
  res =TFIDF(tf, nDocs, df)

  
  return(res)
}


# calcul le TF IDF enter un mot stematisé et un document.
TFIDF_doc <- function(word, doc){
  
#   wfa <- getWordsFromArticle(doc)
#   n <- nrow(wfa)
#   tf <- count de word dasn doc/nb de mot dans le doc
#   tf <- getLinkFromArticleWord(doc, word)/n

  tf <- getLinkFromArticleWord(doc, word)
#   print("TF")  
#   print(tf)
#   print(word)
  if(!is.null(tf)){
    if(tf >0){
      tf <- 1 + log(tf)
    }
  }else{
    tf <-0
  }
  
  nDocs = nombreDoc
  ArtFrWor=getArticlesFromWord(wordStem(word, language = "french"))
  df = nrow(ArtFrWor)
  
  res =0
  res =TFIDF(tf, nDocs, df)
  
  return(res)
}




# Calcule la similarité cosinus entre une requete et un document
cosSim_req_1doc <- function(req, nomDoc){
  req <- removePunctuation(req)
  req <- tolower(req)
  req <- removeWords(req, stopwords("french"))
  req <- stripWhitespace(req)
  words <- unlist(strsplit(req, "\\s"))
  words <- wordStem(words, language = "french")
  
  wordsUnique <- unique(words)
  
  
#   vectReq <- sapply(wordsUnique, TFIDF_req, req = req)
  
  vectReq <- c(TFIDF_req(wordsUnique[1], req))
  if(length(wordsUnique)>1){
    for(i in 2:length(wordsUnique)){
      vectReq <- c(vectReq,TFIDF_req(wordsUnique[i], req))
    }
  }

#   vectDoc <- sapply(wordsUnique,TFIDF_doc, doc = nomDoc)

  vectDoc <- c(TFIDF_doc(wordsUnique[1], nomDoc))
  if(length(wordsUnique)>1){
    for(j in 2:length(wordsUnique)){
      vectDoc <- c(vectDoc, TFIDF_doc(wordsUnique[j], nomDoc))
    } 
  }
  
#   print("taille req")
#   print(length(wordsUnique))
#   
#   print("test vectDOc et vectReq même longueur")
#   print(length(vectDoc)==length(vectReq))
  
  res <- sim_cos(vectReq,vectDoc)
#   print("vectReq, vectDoc")
#   print(vectReq)
#   print(vectDoc)
  return(res)
}


# Calcule la cosinus similarité entre une requete et tous les documents qui lui sont associés.
# retourne une liste(nom de documents associé, score) ordonnée
cos_sim_req_doc <- function(req){
  listeDoc <- setDocReq(req)
  
#   nomDoc <- listeDoc
#   score <- sapply(listeDoc,cosSim_req_1doc, req = req)
  
  nomDoc <- c(listeDoc[1])
  score <- c(cosSim_req_1doc(req, listeDoc[1]))
  
  if(length(listeDoc)>1){
    for(i in 2:length(listeDoc)){
      nomDoc <- c(nomDoc,listeDoc[i])
      score <- c(score, cosSim_req_1doc(req, listeDoc[i]))
    }
  }
  
  
  ordre <- order(score, decreasing = T)
  nomDoc <- nomDoc[ordre]
  score <- score[ordre]
  
  listeDocScore <- list(nomDoc,score)
  
  return(listeDocScore)
}



# PrepSlidWind(c(17,16,15,14,3,2,1), c("17","16","15","14","3","2","1"), 2, 0.5)
 
 

