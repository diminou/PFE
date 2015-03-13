PFE<-Sys.getenv("PFE")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
source(paste(PFE, "requetesNeo4j.R", sep = "/"))
# install.packages("devtools")
# devtools::install_github("nicolewhite/RNeo4j")
# Libaiies utiles pour les fction utilsées venant de requetesNEo4j.R et import.R 
library(tm)
library(SnowballC)
library(RTextTools)
library(RNeo4j)
library(Rcpp)


sourceCpp(paste(PFE, "CalculScoreCategorie.cpp", sep="/"))


db = startGraph("127.0.0.1:7474/db/data/")


#  Ces calculs sont mnt fait en cpp
#  tf time frequency du mot dans le doc, nb nb de doc, df document frequency du mot
# TFIDF <- function(tf, nb, df){
#   return (tf *log(nb/df))
# }

# tfidf : tfidf de la fct pcdte, r : nb de mot dans le doc (= longueur du doc)
TFIDF_norm <- function(tfidf, r){
 return (tfidf/sqrt(r *(tfidf*tfidf)))
}


#  Ces calculs sont mnt fait en cpp
# carre <- function(x){
#   return(x*x)
# }
# 
# 
# # calcul la similarité coinus enter 2 vecteurs v1 et v2 de même longueur (on prendra le tfidf dans noter cas.)
# sim_cos2 <- function(v1, v2){
#   num <- 0
#   part1 <-  0
#   part2 <-  0
# 
#   num = sum(v1,v2)
#   part1 = sum(carre(v1))
#   part2 = sum(carre(v2))
#   
#   denom = (sqrt(part1)*sqrt(part2))
#   res= (num /denom)
#   return (res)
# }

# nb de doc 
nbDocs <- function(){
  q = "match(a:article) return count(a)"
  return(cypher(db, q)[1,1]) 
}
nombreDoc <- nbDocs()



# Old fashion way 

# # retourne la liste(nom des doc dans lequel le mot est présent, vecteur semantique du mot) 
# MotSemantInterp <- function(mot){
#   ArtFrWor=getArticlesFromWord(wordStem(mot, language = "french"))
#   n = nrow(ArtFrWor)
#   df = n 
#   nDocs = nombreDoc
#   s <- rep(0,n)
#   nom <- rep("", n)
#   for(i in 1:n){
#     tf = 1 + log(as.numeric(ArtFrWor[i,2])) 
#     nom[i] <- ArtFrWor[i,1]
#     s[i] <- TFIDF(tf, nDocs, df)
#   }
#   
# # On trie les 2 vecteurs par ordre de score décroissant  
#   ordre <- order(s, decreasing = TRUE)
#   s <- s[ordre]
#   nom <- nom[ordre]
# 
# 
#   listeNomScore <- PrepSlidWind(vecTrie = s,nomTrie = nom, length = 100, pourcent = 0.05)
# 
#   # ce qu'on faisait sans la sliding window
# #   listeNomScore <- list(nom, s)
#   return(listeNomScore)
# }
# 
# 
# 
# # Application de la fonction MotSemantInterp a une requete (après traitement de cette dernière)
# InterpSemRequete <- function(req){
#   req <- removePunctuation(req)
#   req <- tolower(req)
#   req <- removeWords(req, stopwords("french"))
#   req <- stripWhitespace(req)
#   words <- unlist(strsplit(req, "\\s"))
#   words <- wordStem(words, language = "french")
# 
#   lis <- lapply(words,MotSemantInterp )
#   return(lis)
# }
# 
# # retourne true ssi la sliding window garde la séquence (<5% highest score), sinon retourne false
# SlidingWindow <- function(seq, highScore, length,  pourcent){
#   bool = FALSE
#   diff = seq[1] -seq[length] 
#   print(diff)
#   print(highScore)
#   print(pourcent)
#   if(diff < (pourcent * highScore)){
#     bool= TRUE
#   }else {
#     bool = FALSE
#   }
#   return (bool)
# }
# 
# # on prépare les données pour la méthode de sliding window et on boucle en faisant varier glisser notre fenetre
# # renvoit la liste(nom, score) ppour les scores "pertinents" sans grand saut
# PrepSlidWind <- function(vecTrie, nomTrie, length, pourcent){
#   
#   if(length> length(vecTrie)){
#     lis <- list(vecTrie, nomTrie)
#   }else {
#     highScore = vecTrie[1]
#     bool = TRUE
#     i =1
#     resNom <- nomTrie[1]
#     res <-  vecTrie[1]
#     max <- length(vecTrie)
#     while(bool== TRUE){
#       seq <- vecTrie[(1 +i) :(length+i)]
#       bool <- SlidingWindow(seq,highScore, length, pourcent)
#       if(bool== TRUE){
#         res[i+1] <- vecTrie[i+1]
#         resNom[i+1] <- nomTrie[i+1]
#       }
#       
#       if(i == max -length ){
#         bool = FALSE
#       }
#       i <- i+1
#     }
#     lis <- list(resNom, res)
#   }
# 
#   
#   return(lis)
# }



firstCol <- function(datafr) {
#   print(nrow(datafr))
#   print(ncol(datafr))
  if(nrow(datafr)>0){
    res <- datafr[,1]
  }else{
    res <- NULL
  }
  return(res)
}


getArtNamesFromWord <- function(word) {
  return (firstCol(getArticlesFromWord(fixEncoding(word))))
}


laReunion <- function(vect){
  return (vect)
}


fixEncoding <- function(string){
  return(enc2utf8(string))
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

  tempo <-  sapply(wordsUnique,getArtNamesFromWord) # renvoit un vect de string
  
  vect <- NULL
  for(i in 1:length(tempo)){
#     print(tempo[[i]]) 
    if(!is.null(tempo[[i]])){
      vect <- union(vect, tempo[[i]])
    }

  }

  listeDocUnique <- vect

  print("reunion terminée")

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
  df <- getDocFreq(wordStem(word, language = "french"))

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

#   print("doc")
#   print(doc)
#   print("word")
#   print(word)
  tf <- getLinkFromArticleWord(doc, word)

  if(!is.null(tf)){
    if(tf >0){
      tf <- 1 + log(tf)
    }
  }else{
    tf <-0
  }
  
  nDocs = nombreDoc
  df <- getDocFreq(wordStem(word, language = "french"))

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

  vectReq <- sapply(wordsUnique, TFIDF_req, req = req)
  vectDoc <- sapply(wordsUnique,TFIDF_doc, doc = nomDoc)
  res <- sim_cos(vectReq,vectDoc)

  return(res)
}


# Calcule la cosinus similarité entre une requete et tous les documents qui lui sont associés.
# retourne une liste(nom de documents associé, score) ordonnée
cos_sim_req_doc <- function(req){
  listeDoc <- setDocReq(req)
  
  nomDoc <- listeDoc
#   print("listeDoc")
#   print(listeDoc)
#   print("fin liste doc")
  score <- sapply(listeDoc,cosSim_req_1doc, req = req)
#   print("h")
  
#   print("plus qu'a ordonné")
  ordre <- order(score, decreasing = T)
  nomDoc <- nomDoc[ordre]
  score <- score[ordre]
  
  listeDocScore <- list(nomDoc,score)

  return(listeDocScore)
}

# t1 <- Sys.time()
cos_sim_req_doc("boulanger sdlfjsfljp")
# t2 <- Sys.time()
# difftime(t2,t1)

CategoriesFromReq <- function(req){
  listeDocReq <- cos_sim_req_doc(req)

  # On prend les 20% les meilleurs résultats:
  quantile <- 80*length(listeDocReq[[1]])/100
  listeNomTempo <- listeDocReq[[1]][1:quantile]
  listeScoreTempo <- listeDocReq[[2]][1:quantile]

  l <- lapply(listeNomTempo,getCategoriesFromArticle)
  listeScore <- NULL

#   print(length(l))
#   if(length(l)>0){
    for(j in 1:length(l)){
      print(l[j])
      tempp <- unlist(l[j])
      scoreTempo <- rep(listeScoreTempo[j][[1]], length(tempp))
      listeScore <- c(listeScore, scoreTempo)
    }
    
    res <- unlist(l)
#   }else{
#     listeScore <- NULL
#     res <- NULL
#   }
# print("c")
  listeCatScore <- list(res, listeScore)
# print("d")
  return(listeCatScore)
}

# CategoriesFromReq("pain")
# CategoriesFromReq("boulanger")

getSetCateg <- function(vect){
  return(unique(vect))
}


CalculScoreCat <- function(liste){
  setCat <- getSetCateg(liste[[1]])
  vectNom <- liste[[1]]
  vectScore <- liste[[2]]

#   std::vector<std::string> setCat, std::vector<std::string> vectNom, std::vector<int> vectScore
  setScore <- testonsRcpp(setCat,vectNom,vectScore)
  ordre <- order(setScore, decreasing = TRUE)
  setCat <- setCat[ordre]
  setScore <- setScore[ordre]

  listeCatScore <- list(setCat, setScore)

  return (listeCatScore)
}



# CalculScoreCat(CategoriesFromReq("sdpofi"))


# cos_sim_req_doc("boulanger levure")
# cos_sim_req_doc("boulanger")
# cos_sim_req_doc("levure")

# getCategoriesFromArticle(wordStem("sandwiche", language = "french"))
 

