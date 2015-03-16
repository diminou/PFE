source(paste(PFE, "ESA.R", sep = "/"))

source(paste(PFE, "requeteWiki.R", sep = "/"))



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

comparaison <- function(req){
  
  listeStr1 <- listeTitresRequete(req)
  resESA <- cos_sim_req_doc(req)
  listeScore <- resESA[[2]]

  listeStr2 <- resESA[[1]]
  
 
  
  vectPosReelle <- positionReelle(resESA)
  print("listeStr2")
  print(listeStr2)
  print("listeScore")
  print(listeScore)
  print("vectPosReelle")
  print(vectPosReelle)
  
  listeStr2 <- unlist(lapply(listeStr2, collage))
#   positionReelle
  l <- comparaisonListeStr(listeStr1, listeStr2, vectPosReelle)
  return(l)
}


comparaison("boulanger")


# temo <-  cos_sim_req_doc("boulanger")[[2]]
# temo
# temo[64][[1]]
# print(listeScore[1][[1]])


