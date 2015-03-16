source(paste(PFE, "ESA.R", sep = "/"))

source(paste(PFE, "requeteWiki.R", sep = "/"))

listeTitresRequete() # wiki

cos_sim_req_doc() # ESA


comparaisonListeStr <- function(listeStr1, listeStr2){
  
  vect <- NULL
  posListe1 <- NULL
  posListe2 <- NULL
  
  for(i in 1:length(listeStr2)){
    for(j in 1:length(listeStr1)){
      if(listeStr2[i]==listeStr1[j]){
        vect <- union(vect,listeStr1[j])
        posListe1 <- union(posListe1,j)
        posListe2 <- union(posListe2,i)
      }      
    }
  }
  
  l <- list(vect, posListe1, posListe2)

  return (l)
}


comparaison <- function(req){
  
  listeStr1 <- listeTitresRequete(req)
  listeStr2 <- cos_sim_req_doc(req)[[1]]
  l <- comparaisonListeStr(listeStr1, listeStr2)
  return(l)
}
listeTitresRequete("boulanger")

# comparaisonListeStr(c("a","b","c"), c("f","a","e"))
comparaison("boulanger")

cos_sim_req_doc("ioefieuriozeur")

