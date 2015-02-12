PFE<-Sys.getenv("PFE")
setwd(PFE)
source(paste(getwd(), "utils.R", sep="/"))
install.packages("stringr")
install.packages("R.oo")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

library(RNeo4j)
library(compiler)
library(R.oo)
library(Rcpp)

sourceCpp(paste(PFE, "import.cpp", sep="/"))
getBracketedExps("sdf<asdfasdfa>sdfg<asdf>asda.")
getResource("http://blabla.xxx/resource/Catégorie:testé")
print(outputString())

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
  part <- tail(Filter(is_category_resource, parts), 1)
  elements <- harvest(strsplit(part, "/")[[1]])
  atoms <- harvest(strsplit(tail(elements, 1), ":")[[1]])
  print("atoms :")
  print(atoms)
  return(tail(atoms, 1))
})

conn <- file(paste(HOME, fileName, sep = "/"), open = "r")
addConstraint(db, "article", "title")
addConstraint(db, "category", "label")
line <- readLines(conn, 1)
line <- readLines(conn, 1)
query1 <- extractArticleMQ(line)
query2 <- extractCategoryMQ(line)
query3 <- extractIs_underMQ(line)
print(query1)
print(query2)
print(query3)
cypher(db, query1)
cypher(db, query2)
cypher(db, query3)
#ca <- createNode(db, "article", title = extractArticle(line))
#cc <- createNode(db, "category", label = extractCategory(line))
#createRel(ca, "is_under", cc )
close(conn)




query = "MATCH (p:category) 
         WHERE p.label = \"Algèbre_linéaire\"
         RETURN p"
query = "merge (cat:ct {name:'numbers'})"

cypher(db, query)

res <- getNodes(db, query)

conn <- file(paste(HOME, fileName, sep = "/"), open = "r")
counter100 <- 0
counter <- 0
while(length(line <- readLines(conn, 1)) > 0 ){
    tryCatch({
      query1 <- extractArticleMQ(line)
      query2 <- extractCategoryMQ(line)
      query3 <- extractIs_underMQ(line)
      cypher(db, query1)
      cypher(db, query2)
      cypher(db, query3)
    }, error=function(e){})
    counter <- counter + 1
    if(counter%/%100 > counter100){
      print(counter)
      counter100 <- counter %/% 100
    }
}
close(conn)
