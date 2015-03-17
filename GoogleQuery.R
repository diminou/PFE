PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")

sourceCpp(paste(PFE, "import.cpp", sep="/"))


source(paste(PFE, "ESA.R", sep = "/"))

library(utils)
library(RCurl)
library(rjson)

GetGoogleResults <- function(keyword, service) {       

  base_url <- "http://ajax.googleapis.com/ajax/services/search/"
  keyword <- gsub(" ", "+", keyword)
  query <- paste(base_url, service, "?v=1.0&q=", keyword, sep="")

  query <- paste(query, "&start=", 0, sep="")
  results <- fromJSON(getURL(query))
  return(results)
}



collage <- function(String){
  
  vectStr <- unlist(strsplit(String,"_"))
  res <- NULL
  if(length(vectStr)>1){
    for(i in 1:(length(vectStr)-1)){
      
      res <- paste(res, vectStr[i]," ", sep="")
    }
  }
  
  
  res <- paste(res,  vectStr[length(vectStr)], sep="")
  return(res)
}


titreGoogleFromReq <- function(req){
  google <- GetGoogleResults(paste("site:fr.wikipedia.org " , req), "web")
  vectGoogle <- NULL
  for(i in 1:length(google$responseData$results)){
    tempo <- collage(getResource(URLdecode(URLdecode(google$responseData$results[[i]]$url))))
    vectGoogle <- union(vectGoogle,tempo) 
  }
  return(vectGoogle)
}

# titreGoogleFromReq("achat pièce 4x4")
# cos_sim_req_doc("achat pièce 4x4")

positionReelle <- function(liste){
  res <- NULL
  vectTitre <- liste[[1]]
  vectScore <- liste[[2]]
  vectPos <- NULL
  scoreCourant <- 0
  posCourant <- 0
  
  for(i in 1:length(vectTitre)){
    if(vectScore[i]==scoreCourant){
      vectPos[i] = posCourant
    }else{
      scoreCourant = vectScore[i]
      posCourant = i
      vectPos[i] = i      
    }
  }
  
  return(vectPos)
}


comparaisonListeStr <- function(listeStr1, listeStr2, posReelle){
  
  vect <- NULL
  posListe1 <- NULL
  posListe2 <- NULL
  
  for(i in 1:length(listeStr2)){
    for(j in 1:length(listeStr1)){
      if(listeStr2[i]==listeStr1[j]){
        vect <- union(vect,listeStr1[j])
        posListe1 <- union(posListe1,j)
        posListe2 <- c(posListe2,posReelle[i])
      }      
    }
  }
  
  l <- list(vect, posListe1, posListe2)
  return (l)
}

comparaison <- function(req){
  
  listeStr1 <- titreGoogleFromReq(req)
  resESA <- cos_sim_req_doc(req)
  listeScore <- resESA[[2]]
  
  listeStr2 <- resESA[[1]]
  
  
  
  vectPosReelle <- positionReelle(resESA)
#   print("listeStr2")
#   print(listeStr2)
#   print("listeScore")
#   print(listeScore)
#   print("vectPosReelle")
#   print(vectPosReelle)
  
  listeStr2 <- unlist(lapply(listeStr2, collage))
  l <- comparaisonListeStr(listeStr1, listeStr2, vectPosReelle)
  return(l)
}

# comparaison("boulanger")


# titreGoogleFromReq("boulanger")
t1 <- Sys.time()
cos_sim_req_doc("4x4 pièce auto moto")
t2 <- Sys.time()

difftime(t2,t1)
