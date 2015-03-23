#recuperer liste Bing
#recuperer liste ESA
#comparer les 2 listes

PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")


source(paste(PFE, "ESA.R", sep = "/"))
source(paste(PFE, "bonjour.R", sep = "/"))

library(utils)
library(RCurl)
library(rjson)


# Collage des "_" pour un apply
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

# Remplacer "sep" par " " :
replaceSeparatorBySpace <- function(StringEntree,Separator){
  
  vectStr <- unlist(strsplit(StringEntree,Separator))
  res <- NULL
  if(length(vectStr)>1){
    for(i in 1:(length(vectStr)-1)){
      
      res <- paste(res, vectStr[i]," ", sep="")
    }
  }
  
  
  res <- paste(res,  vectStr[length(vectStr)], sep="")
  return(res)
}

# Supprimer "String" :
removeString <- function(StringEntree,Separator){
  
  vectStr <- unlist(strsplit(StringEntree,Separator))
  res <- NULL
  if(length(vectStr)>1){
    for(i in 1:(length(vectStr)-1)){
      
      res <- paste(res, vectStr[i],"", sep="")
    }
  }
  
  
  res <- paste(res,  vectStr[length(vectStr)], sep="")
  return(res)
}

ReqResAmbiguBing <- readLines(paste(HOME, ".pfe/Requetes/ReqResAmbiguBing.txt", sep = "/"),encoding="UTF-8")
ReqResNonAmbiguBing <- readLines(paste(HOME, ".pfe/Requetes/ReqResNonAmbiguBing.txt", sep = "/"),encoding="UTF-8")



# Recupérer les requêtes
recupRequetesBing <- function(ListReqRes){
  
  res <- NULL
  
  for (i in 1:length(ListReqRes)){
    vectStr <- unlist(strsplit(ListReqRes[i],":"))
    res[i]=replaceSeparatorBySpace(vectStr[1],"\\+")
  }
  
  return(res)
}

ReqAmbigu <- recupRequetesBing(ReqResAmbiguBing)
ReqNonAmbigu <- recupRequetesBing(ReqResNonAmbiguBing)



# Récupérer les résultats
recupResultatsBing <- function(ListReqRes){
  
  res <- NULL
  
  for (i in 1:length(ListReqRes)){
    line <- removeString(ListReqRes[i],"Catégorie:")
    vectLine <- unlist(strsplit(line,":"))
    vectRes <- unlist(strsplit(vectLine[2],";"))
    resTemp <- NULL
    for (j in 1:length(vectRes)){
      resTemp[j] <- vectRes[j]
    }
    res[[i]] <- resTemp
  
  }
  
  return(res)
}

ResAmbigu <- recupResultatsBing(ReqResAmbiguBing)
ResNonAmbigu <- recupResultatsBing(ReqResNonAmbiguBing)




# Position réelle pour l'ESA
positionReelle <- function(liste){
  res <- NULL
  vectTitre <- liste[[1]]
  vectScore <- liste[[2]]
  vectPos <- NULL
  scoreCourant <- 0
  posCourant <- 0
  
  if(!is.null(vectScore)){
    for(i in 1:length(vectTitre)){
      
      if(vectScore[i]==scoreCourant){
        vectPos[i] = posCourant
      }else{
        scoreCourant = vectScore[i]
        posCourant = i
        vectPos[i] = i      
      }
    }
  }

    
  
  
  return(vectPos)
}

# Comparer 2 listes de String
comparaisonListeStr <- function(listeStr1, listeStr2, posReelle){
  
  vect <- NULL
  posListe1 <- NULL
  posListe2 <- NULL
  
 
  for(i in 1:length(listeStr2)){
    for(j in 1:length(listeStr1)){
      if(is.null(listeStr1[j])||is.null(listeStr2[i])){
      }
      else{
        if(listeStr2[i]==listeStr1[j]){
          vect <- union(vect,listeStr1[j])
          posListe1 <- union(posListe1,j)
          posListe2 <- c(posListe2,posReelle[i])
        }      
      }
    }
  }
  
  l <- list(vect, posListe1, posListe2)
  return (l)
}

# Comparaison 2 listes de String sans pos
comparaisonListeStr <- function(listeStr1, listeStr2, posReelle){
  
  vect <- NULL
  posListe1 <- NULL
  posListe2 <- NULL
  
  
  for(i in 1:length(listeStr2)){
    for(j in 1:length(listeStr1)){
      if(is.null(listeStr1[j])||is.null(listeStr2[i])){
      }
      else{
        if(listeStr2[i]==listeStr1[j]){
          vect <- union(vect,listeStr1[j])
          posListe1 <- union(posListe1,j)
          posListe2 <- c(posListe2,posReelle[i])
        }      
      }
    }
  }
  
  l <- list(vect, posListe1, posListe2)
  return (l)
}

# Comparer pour une requête
comparaison <- function(ListResBing,resESA){
  
  listeScoreESA <- resESA[[2]]
  listeResESA <- resESA[[1]]
  vectPosReelle <- positionReelle(resESA)
  
  ListResBing <- unlist(lapply(ListResBing, collage))
  listeResESA <- unlist(lapply(listeResESA, collage))
  l <- comparaisonListeStr(listeResESA, ListResBing, vectPosReelle)
  return(l)
}


# Comparaison finale pour les requetes ambigus

comparaisonFinaleESA1Ambigu <- function(){
  res <- NULL
  count <- 0
  for (i in 1:length(ReqAmbigu)){
    listResBing <- ResAmbigu[[i]]
#    print(listResBing)
    resESA <- cos_sim_req_doc(ReqAmbigu[i])
#    print(resESA)
    res[[i]] <- comparaison(listResBing,resESA)
    if(is.null(res[[i]][[1]])){
    }
    else{
      count=count+1
      print("$$$$$$ JE T'AI TROUVE ! $$$$$$")
      print(res[[i]])
    }
  print(ReqAmbigu[i])
  print(paste(count,"/",i,sep=""))
  }  
}
comparaisonFinaleESA1Ambigu()

comparaisonFinaleESA2Ambigu <- function(){
  res <- NULL
  count <- 0
  for (i in 1:length(ReqAmbigu)){
    listResBing <- ResAmbigu[[i]]
    #    print(listResBing)
    resESA <- ArtFromBestCat(ReqAmbigu[i])
    #    print(resESA)
    res[[i]] <- comparaison(listResBing,resESA)
    if(is.null(res[[i]][[1]])){
    }
    else{
      count=count+1
      print("$$$$$$ JE T'AI TROUVE ! $$$$$$")
      print(res[[i]])
    }
    print(ReqAmbigu[i])
    print(paste(count,"/",i,sep=""))
  }  
}
ArtFromBestCat("boulangerie")

comparaisonFinaleNonAmbigu <- function(){
  res <- NULL
  count <- 0
  for (i in 1:length(ResNonAmbigu)){
        #print(ReqNonAmbigu[i])
    listResBing <- ResNonAmbigu[[i]]
        #print(listResBing)
    resESA <- ArtFromBestCat(ReqNonAmbigu[i])
#    resESA <- cos_sim_req_doc(ReqNonAmbigu[i])
        #print(resESA)
    res[[i]] <- comparaison(listResBing,resESA)
        #print(res[[i]])
    if(is.null(res[[i]][[1]])){
    }
    else{
      count=count+1
      print("$$$$$$ JE T'AI TROUVE ! $$$$$$")
    }
    print(ReqNonAmbigu[i])
    print(paste(count,"/",i,sep=""))
  }  
}
comparaisonFinaleNonAmbigu()



