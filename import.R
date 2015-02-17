PFE<-Sys.getenv("PFE")
setwd(PFE)
source(paste(getwd(), "utils.R", sep="/"))
install.packages("stringr")
install.packages("R.oo")
install.packages("tm")
install.packages("SnowballC")
install.packages("RTextTools")

Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

library(RNeo4j)
library(compiler)
library(R.oo)
library(Rcpp)
library(tm)
library(SnowballC)
library(RTextTools)

sourceCpp(paste(PFE, "import.cpp", sep="/"))
getBracketedExps("sdf<asdfasdfa>sdfg<asdf>asda.")
getResource("http://blabla.xxx/resource/Catégorie:testé")
print(outputString())

db = startGraph("127.0.0.1:7474/db/data/")

clear(db)

HOME<-Sys.getenv("HOME")
fileName <- ".pfe/article_categories_fr.ttl"
csv_a_c_path <- paste(HOME, ".pfe/ModifiedData/article_categories_fr.csv", sep = "/")

category_relations <- paste(HOME, ".pfe/skos_categories_fr.ttl", sep = "/")
category_relations_csv <- paste(HOME, ".pfe/skos_categories_fr.csv", sep = "/")
categoryLinksToCsv(category_relations, category_relations_csv)


toSpace <- content_transformer ( function ( x, pattern ) gsub (pattern, " ", x))
testSource <- VCorpus(VectorSource(c("Ce n'est qu'un test.", "Ceci est un autre test.")), readerControl = list(language = "french"))
stopwords("french")
testSource <- tm_map(testSource, toSpace, "'")
testSource <- tm_map(testSource, removePunctuation)
testSource <- tm_map(testSource, stripWhitespace)
testSource <- tm_map(testSource, content_transformer(tolower))
testSource <- tm_map(testSource, removeWords, stopwords("french"))
stemDocument(testSource[[1]], language="french")
toSpace(VectorSource("blabla d'Argagnan blabla2."), "'")

wordStem("Par exemple, ceci n'est qu'un test. Proliferation armistice proposition .", language = "french")
acceptCharVect(unlist(strsplit(removePunctuation("bla, blabla, exclamation!
                           Encore une ligne, bla."), "\\s")))


inspect(testSource)

toSpace("Par exemple, ceci n'est qu'un test.", "\'")

getTransformations()

tmm <- tm_map(testSource, stemDocument)
inspect(tmm)
inspect(DocumentTermMatrix(testSource))

conn <- file(category_relations, open = "r")
line <- readLines(conn, 1)
print(catLinkLine(line))
line <- readLines(conn, 1)
print(catLinkLine(line))
line <- readLines(conn, 1)
print(catLinkLine(line))
line <- readLines(conn, 1)
print(catLinkLine(line))
line <- readLines(conn, 1)
print(catLinkLine(line))
close(conn)


query = "MATCH (p:category) 
         WHERE p.label = \"Algèbre_linéaire\"
         RETURN p"
query = "merge (cat:ct {name:'numbers'})"

query = "CREATE CONSTRAINT ON (a:article) ASSERT a.title IS UNIQUE"
cypher(db, query)

query = "CREATE CONSTRAINT ON (c:category) ASSERT c.label IS UNIQUE"
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", csv_a_c_path,"\" AS row
         MERGE (:article {title:row.article})", sep = "")

cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", csv_a_c_path, "\" AS row
         MERGE (:category {label:row.categorie})", sep = "")

cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", csv_a_c_path, "\" AS row
         MATCH (a:article {title: row.article})
         MATCH (cat:category {label: row.categorie})
         MERGE (a)-[:is_under]->(cat)", sep = "")

cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", category_relations_csv, "\" AS row
         MATCH (c0:category {label: row.cat0})
         MATCH (c1:category {label: row.cat1})
         MERGE (c0)-[r:relates {type:row.link}]->(c1)", sep = "")

cypher(db, query)


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
