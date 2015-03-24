PFE<-Sys.getenv("PFE")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
source(paste(PFE, "ESA.R", sep = "/"))
source(paste(PFE, "requetesNeo4j.R", sep = "/"))



db = startGraph("127.0.0.1:7474/db/data/")

# pourcent entre 0 et 1
Top50Article <- function(req, pourcent){
  listeDocReq <- cos_sim_req_doc(req)
  

  listeNomTempo <- listeDocReq[[1]]
  listeScoreTempo <- listeDocReq[[2]]
  S <- sum(listeScoreTempo)
  somme =0
  listeNomFinal <- NULL
  listeScoreFinal <- NULL
  scoreSeuil <- 0
  if(is.null(listeScoreTempo)){
    l <- NULL
  }else{
    for(i in 1:length(listeNomTempo)){
      if(somme < (pourcent*S)){
        somme <- somme + listeScoreTempo[[i]]
        listeNomFinal <- c(listeNomFinal, listeNomTempo[i])
        listeScoreFinal  <- c(listeScoreFinal, listeScoreTempo[[i]])
        scoreSeuil <- listeScoreTempo[[i]]
      }else{
        if(listeScoreTempo[[i]]==scoreSeuil){
          listeNomFinal <- c(listeNomFinal, listeNomTempo[i])
          listeScoreFinal  <- c(listeScoreFinal, listeScoreTempo[[i]])
        }
      }
    }
    l <- list(listeNomFinal, listeScoreFinal)
  }
 
  return(l)
}

# cos_sim_req_doc("lrurposidposdi")

# CategoriesFromReq("fuzifuzdifsdilfjsdl")
# Top50Article("boulanger pain", 0.5)


# match (a:article {title:"Croque-monsieur"}), (b:article {title:"Jambon-beurre"})
# match p = allShortestPaths((a)-[:is_under|relates *]-(b))
# with  p as shortestPath
# unwind nodes(shortestPath) as pp
# return distinct(pp.label)
# match p = allShortestPaths((a)-[:is_under|relates *]-(b))
retrieveMostPertinentPath2 <- function(title1, title2, pertinenceProduct){
  query <- paste("match (a:article {title:'", escapeApostrophes(title1),
                 "'}), (b:article {title:'",
                 escapeApostrophes(title2),
                 "'})
                 match p = allShortestPaths((a)-[:is_under|relates *]-(b))
                 with  p as shortestPath
                 unwind nodes(shortestPath) as pp
                 return distinct(pp.label)", sep = "")
  result <- cypher(db,query)
  if(is.null(result)) {
    return(NULL)
  }
  perti <- rep(pertinenceProduct, nrow(result))
  t1 <- rep(title1, nrow(result))
  t2 <- rep(title2, nrow(result))
  result <- cbind(result, perti,t1,t2)
  return(na.omit(result))
}
# retrieveMostPertinentPath("Sandwich","Pizza", 0.5)

pushBack2 <- function(lst, elt) {
  result <- lst
  result[[length(lst) + 1]] <- elt
  return(result)
}

getAllCats2 <- function(grid) {
  if(is.null(grid)){
    return(NULL)
  }else{
    catFrames <- list()
    for(i in 1:dim(grid)[1]) {
      catFrames <- pushBack2(catFrames, retrieveMostPertinentPath2(grid[i, 1], grid[i, 2], grid[i, 3]))
    }
    return(catFrames)
  }
}



getSortedCats2 <- function(catsList) {
  if(is.null(catsList)){
    return(NULL)
  }else{
    datamap <- Reduce(function(x, y) rbind(x, y), catsList )
    result <- tryCatch({(aggregate(datamap[, 2], by = list(datamap[, 1]), FUN = sum))},
                       error = function(e) { return(NULL)})
    #   result <- tryCatch({(aggregate(datamap[, 2], by = list(datamap[, 1]), FUN = sum))}
    if(is.null(result)){
      return(NULL)
    }
    
    return(result[order(-result[, 2]), ])
  }
 
}



adaptEsa2 <- function(esaList) {
  if(is.null(esaList)){
    result <- NULL
  }else{
    result <- data.frame(cbind(esaList[[1]], esaList[[2]]), stringsAsFactors = F)
    result[, 2] <- as.numeric(result[, 2])
    result[, 1] <- as.character(result[, 1])
  }
  return(result)
}

makeAllPairsfromESA2 <- function(dataframe) {
  if(is.null(dataframe)){
    grid <- NULL
  }else{
    ids <- unique(dataframe[,1])
    grid <- expand.grid(ids, ids)
    grid[, 1] <- as.character(grid [, 1])
    grid[, 2] <- as.character(grid [, 2])
    grid <- grid[grid[, 1] > grid[, 2], ]
    
    grid[, 3] <- rep(0, dim(grid)[1])
    for(i in 1:dim(grid)[1]){
      
      var1 = grid[i,1]
      var2 = grid[i,2]
      grid[i, 3] <- dataframe[dataframe[,1]==var1, 2] * dataframe[dataframe[,1]==var2, 2]
    }
  }
  return(grid)
}

naOmitAvecException <- function(param){
  if(is.null(param)){
    return(NULL)
  }else{
    return(na.omit(param))
  }
}

bestCategorie2 <- function(query) {
  
  result <- tryCatch({return(getSortedCats2(getAllCats2(naOmitAvecException(makeAllPairsfromESA2(adaptEsa2(Top50Article(query, 0.5))))))[1, 1])},
                     error = function(e){return (NULL)})
#   result <- getSortedCats(getAllCats(na.omit(makeAllPairsfromESA(adaptEsa(Top50Article(query, 0.5))))))[1, 1]
  return(result)
}



fixEncodinCat <- function(string){
  return(enc2utf8(iconv(string, from = "UTF-8", to = "ISO-8859-1")))
}

# adaptEsa(Top50Article("boulanger pain", 0.5))[, 2]
# getSortedCats2(getAllCats2(naOmitAvecException(makeAllPairsfromESA2(adaptEsa2(Top50Article("idsjqspoidpoqsid", 0.5))))))
# cos_sim_req_doc("boulanger pain")
# bestCategorie2("fioqsjdfskfjsdklfjsdklfj")
# fixEncodinCat(getBestCatCode("boulanger pain"))

# class(unlist(as.list(ff[[170]][1])))

# adaptEsa(Top50Article("boulanger sandwich", 0.5))[,1]




getSetCat2 <- function(liste){
  setCat <- NULL;
  for(i in 1:length(liste)){
    setCat <- union(setCat, unlist(as.list(liste[[i]][1])))
  }
  return(setCat)
}
# getSetCat(getAllCats(na.omit(makeAllPairsfromESA(adaptEsa(Top50Article("boulanger sandwich", 0.5))))))



getListeArt2 <- function(liste, setCat){
  if(is.null(liste) || is.null(setCat)){
    return (NULL)
  }else{
  listeTempo <- list()
  listeScore <- list()

  for(k in 1:length(setCat)){
    vectArtCat <- NULL
    vectScoreTemp <- NULL
    for(i in 1:length(liste)){
       l <- unlist(as.list(liste[[i]][1]))
       lscore <- unlist(as.list(liste[[i]][2]))
       l2 <- unlist(as.list(liste[[i]][3]))
       l3 <- unlist(as.list(liste[[i]][4]))
       for(j in 1:length(l)){
         if(setCat[k]==l[j]){
           vectArtTempo <- c(l2[j], l3[j])
           testTempo <- vectArtCat
           vectArtCat <- union(vectArtCat, vectArtTempo)
           if(!(length(testTempo)==length(vectArtCat))){
             if(length(testTempo)==0){
               vectScoreTemp <- c(vectScoreTemp,lscore[j], lscore[j])
             }else{
               vectScoreTemp <- c(vectScoreTemp,lscore[j])
             }
             
           }
           
           
         }
       }
    }
    listeScore <- pushBack2(listeScore, vectScoreTemp)
    listeTempo  <- pushBack2(listeTempo, vectArtCat)
  }
  listeRes <- list(listeTempo,listeScore)
  return(listeRes)
  }
}

listeCatArtFinale2 <- function(liste){
  s <- getSetCat2(liste)
  l <- getListeArt2(liste,s)
  listeRes <- list(s, l)
  return(listeRes)
}
  
# listeCatArtFinale(getAllCats(na.omit(makeAllPairsfromESA(adaptEsa(Top50Article("boulanger pain", 0.5))))))




# head(listeCatArtFinale2(getAllCats2(na.omit(makeAllPairsfromESA2(adaptEsa2(Top50Article("boulanger pain", 0.5)))))))

ArtFromBestCat <- function(query){
  listeArtBestCat <- NULL
  listeScoreArt <- NULL
  listeCatArt <- listeCatArtFinale2(getAllCats2(naOmitAvecException(makeAllPairsfromESA2(adaptEsa2(Top50Article(query, 0.5))))))
  bestCat <-  bestCategorie2(query)
  if(is.null(listeCatArt) || is.null(bestCat)){
    return(list(NULL, NULL))
  }else{

  for(i in 1:length(listeCatArt[[1]])){
    if(listeCatArt[[1]][i]==bestCat){
      listeArtBestCat <- listeCatArt[[2]][[1]][[i]]
      listeScoreArt <- listeCatArt[[2]][[2]][[i]]
    }
  }
  listeRes <- list(listeArtBestCat, listeScoreArt)
  return(listeRes)
  }
}

# system.time(ArtFromBestCat("boulanger pain"))

ArtFromBestCat("pain boulanger")
cos_sim_req_doc("sdfsfsdfsdfs")


# catArtScore <- function(liste){
#   listeNom <- liste[[1]]
#   listeScore <- liste[[2]]
#   
#   setCateg <-  unique(unlist(lapply(listeNom,getCategoriesFromArticle)))
#   setCateg <- unlist(lapply(setCateg, escapeApostrophes))
# 
#   vectScoreCat <- NULL
#   for(i in 1:length(setCateg)){
#     score <- 0
#     for(j in 1: length(listeNom)){
#       l <- unlist(lapply(getCategoriesFromArticle(listeNom[j]), escapeApostrophes))
#       for(k in 1: length(l)){
#         if(l[k]==setCateg[i]){
#           score <- score + listeScore[j]
#         }
#       }
#     }
#     vectScoreCat <- c(vectScoreCat, score)
#   }
# 
#   lordre <- order(vectScoreCat, decreasing = T)
#   vectScoreCat <- vectScoreCat[lordre]
#   setCateg <- setCateg[lordre]
# 
#   listeFinale <- list(setCateg, vectScoreCat)
#   print(ArtFromCat(listeNom, setCateg[1]))
#   return(listeFinale)
# }
# # getArticlesFromCategory("Restaurant_à_thème")
# 
# # system.time(catArtScore(Top50Article("boulanger pain", 0.5)))
#  
# 
# 
# ArtFromCat <- function(listeNomArt, cat){
#   vectArticle <- NULL
#   for(i in 1:length(listeNomArt)){
#     l <- unlist(lapply(getCategoriesFromArticle(listeNomArt[i]), escapeApostrophes))
#     for(j in 1:length(l)){
#       if(l[j]==cat){
#         vectArticle <- c(vectArticle, listeNomArt[i])
#       }
#     }
#   }
#   
#   return(vectArticle)
# }
# 
# 
# zrz <- function(liste, listeArt2Base, pourcent){
#   listeCateg <- liste[[1]]
#   listeScore <- liste[[2]]
#   S <- sum(listeScore)
#   nbArtVect <- NULL
#   CategOk <- NULL
#   for(i in 1:length(listeScore)){
#     if(listeScore[[i]]> pourcent*S){
#       nbArt <- length(ArtFromCat(listeArt2Base, listeCateg[i]))
#       nbArtVect <- c(nbArtVect, nbArt)
#       CategOk <- c(CategOk, listeCateg[i])
#     }
#   }
#   
#   CategOptimal <- NULL
#   if(is.null(CategOk)){
#      # On l'a dans le cul on doit passer au sur categ
#     
#   }else{
#     for(j in 1:length(CategOk)){
#       CategOptimal <- CategOk[which.min(nbArtVect)]
#     }
#     
#   }
#   
#   vectArtCatOpti <- ArtFromCat(listeArt2Base, CategOptimal)
#   return(vectArtCatOpti)
# }

# CalculScoreCat <- function(liste){
#   setCat <- getSetCateg(liste[[1]])
#   vectNom <- liste[[1]]
#   vectScore <- liste[[2]]
#   
#   #   std::vector<std::string> setCat, std::vector<std::string> vectNom, std::vector<int> vectScore
#   setScore <- testonsRcpp(setCat,vectNom,vectScore)
#   ordre <- order(setScore, decreasing = TRUE)
#   setCat <- setCat[ordre]
#   setScore <- setScore[ordre]
#   
#   listeCatScore <- list(setCat, setScore)
#   
#   return (listeCatScore)
# }


# CalculScoreCat(CategoriesFromReq("sdpofi"))


# cos_sim_req_doc("boulanger levure")
# cos_sim_req_doc("boulanger")
# cos_sim_req_doc("levure")

# getCategoriesFromArticle(wordStem("sandwich", language = "french"))

