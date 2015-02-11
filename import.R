PFE<-Sys.getenv("PFE")
setwd(PFE)
source(paste(getwd(), "utils.R", sep="/"))
install.packages("stringr")
install.packages("R.oo")

library(RNeo4j)
library(compiler)
library(R.oo)

db = startGraph("127.0.0.1:7474/db/data/")

clear(db)

HOME<-Sys.getenv("HOME")
fileName <- ".pfe/article_categories_fr.ttl"

has_angle_brackets <- cmpfun( function( string ) {
  return ( grepl ("<", string, fixed = T) & grepl(">", string, fixed = T))
})

longer_than_4 <- cmpfun( function( string ){
  return ( nchar(string) > 4 )
})

non_empty_string <- cmpfun( function( string ){
  return ( nchar(string) > 0 )
})

harvest4 <- cmpfun(function(string_array) {
  return(Filter(longer_than_4, string_array))
})

harvest <- cmpfun(function(string_array) {
  return(Filter(non_empty_string, string_array))
})

has_fixed_pattern <- cmpfun( function(pattern, string) {
  return(grepl(pattern, string, fixed = T))
})

is_category_resource <- cmpfun(function(string) {
  return(has_fixed_pattern("resource/Catégorie:", string))
})

is_article <- cmpfun(function(string) {
  return((!has_fixed_pattern("resource/Catégorie:", string))&has_fixed_pattern("resource/", string))
})



splitTripleXml <- cmpfun( function(line) {
  parts <- harvest4(strsplit(line, "[<]|[>]")[[1]])
  if(length(parts) < 3) {
    throw("invalid line")
  }
  return(parts)
})

extract_article <- cmpfun(function(string) {
  parts <- splitTripleXml(string)
  part <- Filter(is_article, parts)
  elements <- harvest(strsplit(part, "/")[[1]])
  return (tail(elements, 1))
})

extract_category <- cmpfun(function(string){
  parts <- splitTripleXml(string)
  part <- head(Filter(is_article, parts), 1)
  elements <- harvest(strsplit(part, "/")[[1]])
  atoms <- harvest(strsplit(tail(elements, 1), ":")[[1]])
  return(tail(atoms, 1))
})

conn <- file(paste(HOME, fileName, sep = "/"), open = "r")
line <- readLines(conn, 1)
line <- readLines(conn, 1)
parts <- harvest4(strsplit(line, "[<]|[>]")[[1]])
print(parts)
print(length(parts[[1]]))
print(extract_article(line))
print(extract_category(line))
ca <- getOrCreateNode(db, "article", title = extract_article(line))
cc <- getOrCreateNode(db, "category", label = extract_category(line))

addConstraint(db, "article", "title")
addConstraint(db, "category", "label")
createRel(ca, "is_under", cc )
close(conn)


query = "MATCH (p:category) 
         WHERE p.label = \"Algèbre_linéaire\"
         RETURN p"

res <- getNodes(db, query)

conn <- file(paste(HOME, fileName, sep = "/"), open = "r")
while(length(line <- readLines(conn, 1)) > 0 ){
  possibleError <- tryCatch({
    article_name <- extract_article(line)
    category_name <- extract_category(line)
  }, error=function(e) e)
  if(!inherits(possibleError, "error")) {
    ca <- getOrCreateNode(db, "article", title = article_name)
    cc <- getOrCreateNode(db, "category", label = category_name)
    createRel(ca, "is_under", cc)
  }
}
close(conn)