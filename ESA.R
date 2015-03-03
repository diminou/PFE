




#  tf time frequency du mot dans le doc, nb nb de doc, df document frequency du mot
TFIDF <- function(tf, nb, df){
  return (tf *log(nb/df))
}

# tfidf : tfidf de la fct pcdte, r : nb de mot dans le doc (= longueur du doc)
TFIDF_norm <- function(tfidf, r){
 return (tfidf/sqrt(r *(tfidf*tfidf)))
}



sim_cos <- function(v1, v2){
  num=0
  part1= 0
  part2 = 0
  for(i=1:length(v1)){
    num= num + v1[i]*v2[i]
    part1 = part1 + v1[i]*v1[i]
    part2 = part2 + v2[i]*v2[i]
  }
  
  denom = sqrt(part1)*sqrt(part2)
  res= num /denom
  return (res)
}



# nb de doc 
n <- 2 

# retourne le vecteur semantique du mot mot (vecteur numérique)
MotSemantInterp <- function(mot, listeDoc){
  nb = 2 # pour l'exemple
  s <- rep(0,n)
  df # récupérer à partir du mot depuis code eve
  for(i=1:n){
    tf # récupéré à partir du doc depuis code eve
    s[i] <- TFIDF(tf, nb, df)
  }
  return s
}



