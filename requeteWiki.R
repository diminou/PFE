# Il s'agit du code R pour le requetage sur Wikipedia

# INSTALLATION DES PACKAGES
# Si devtools ne fonctionne pas, vous devrez télécharger Rtools
install.packages("devtools")
install.packages("RCurl")
install.packages("XML")
install.packages("WikipediR")

# LOADING DES LIBRARIES
library(devtools)
#install_github("Ironholds/WikipediR")
library(WikipediR)
library(RCurl)
library(XML)


#################################################################################


# PARTIE TEST MDR
# xml.url <- "http://en.wikipedia.org/w/api.php?action=query&generator=search&gsrsearch=calvino&format=xml&gsrprop=snippet&prop=info&inprop=url"
# xmlfile <- xmlParse(xml.url)
# xmltop = xmlRoot(xmlfile)
# xmltop
# xmlName(xmltop)
# xmlSize(xmltop)
# xmltop[[3]][[1]][[1]]
# class(xmltop[[3]][[1]][[1]])
# 
# xmlGetAttr(xmltop[[3]][[1]][[1]],"title")



#################################################################################



# FONCTIONS IMPLEMENTEES

# Fonction qui retourne la liste des titres associés à la recherche Wikipedia
listeTitresRequete <-function(requete){
  vector <- c("","","","","")
  i<-1
  xml.url <- paste("http://fr.wikipedia.org/w/api.php?action=query&generator=search&gsrsearch=",requete)
  xml.url <- paste(xml.url,"&format=xml&gsrprop=snippet&prop=info&inprop=url")
  xmlfile <- xmlParse(xml.url)
  xmltop = xmlRoot(xmlfile)
  
  while (!is.null(xmltop[[3]][[1]][[i]])){
    vector[i]=xmlGetAttr(xmltop[[3]][[1]][[i]],"title")
    i=i+1
  }
  return (vector)
  
}
listeTitresRequete("tennis")




