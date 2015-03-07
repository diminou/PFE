
### Recuperation des variables environnement ###
PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)

### Installation des packages ###
source(paste(getwd(), "utils.R", sep="/"))


### Importation des packages ###
library(RNeo4j)
library(compiler)
library(R.oo)
library(Rcpp)
library(tm)
library(SnowballC)
library(RTextTools)

### Compilation et importation de import.cpp ###
sourceCpp(paste(PFE, "import.cpp", sep="/"))
getBracketedExps("sdf<asdfasdfa>sdfg<asdf>asda.")
getResource("http://blabla.xxx/resource/Catégorie:testé")
print(outputString())
print(sendStringVector())

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

### ALERTE! cette ligne va effacer toutes les donnees dans Neo4j ###
### Initialisation de la base de donnees ###
clear(db)


#######################TSET#########################
testFile <- "/home/divanov/.pfe/test"
conn <- file(testFile, open = "r", encoding = "utf-8")
line <- readLines(conn, 1)
line2 <- readLines(conn, 1)
line3 <- readLines(conn, 1)
Encoding(line3)
Encoding(line) <- "utf-8"
Encoding(line2) <- "latin-1"
enc2utf8(line3)
print(line)
iconv(line3, from = "ISO-8859-1", to = "UTF-8", toRaw = T)
iconv(line3, from = "UTF-8", to = "ISO-8859-1", toRaw = T)
line2 <- enc2utf8(iconv(line2, from = "UTF-8", to = "ISO-8859-1"))
print(line2)
print(line3)
print(toSpace(line, "'"))
print(wordStem(line, language = "french"))
close(conn)

####################################################

fixEncoding <- function(string){
  return(iconv(string, from = "UTF-8", to = "ISO-8859-1"))
}

### Definition des chemins  et transformation en CSV###
### excepte short_abstracts ###
fileName <- ".pfe/article_categories_fr.ttl"
#csv_a_c_path <- paste(HOME, ".pfe/ModifiedData/article_categories_fr.csv", sep = "/")

art_cat <- paste(HOME, "/.pfe/article_categories_fr.ttl", sep = "/")
art_cat_csv <- paste(HOME, "/.pfe/article_categories_fr.csv", sep = "/")
artCatToCsv(art_cat, art_cat_csv)

category_relations <- paste(HOME, ".pfe/skos_categories_fr.ttl", sep = "/")
category_relations_csv <- paste(HOME, ".pfe/skos_categories_fr.csv", sep = "/")
categoryLinksToCsv(category_relations, category_relations_csv)

short_abstracts <- paste(HOME, ".pfe/short_abstracts_fr.ttl", sep = "/")
short_abstracts_csv <- paste(HOME, ".pfe/short_abstracts_fr.csv", sep = "/")
printHead(short_abstracts, 15)

### Petit test de lecture ###
conn <- file(short_abstracts, open = "r")
line <- readLines(conn, 1)
print(extractArticle(line))
print(parseAbstract(line))
line <- readLines(conn, 1)
print(parseAbstract(line))
print(extractArticle(line))
close(conn)


### Test de text mining avec tm ###
toSpace <- function ( x, pattern ) gsub (pattern, " ", x)
testSource <- VCorpus(VectorSource(c("Ce n'est qu'un test.", "Ceci est un autre test.")), readerControl = list(language = "french"))
stopwords("french")
testSource <- tm_map(testSource, toSpace, "'")
testSource <- tm_map(testSource, removePunctuation)
testSource <- tm_map(testSource, stripWhitespace)
testSource <- tm_map(testSource, content_transformer(tolower))
testSource <- tm_map(testSource, removeWords, stopwords("french"))
stemDocument(testSource[[1]], language="french")
toSpace(VectorSource("blabla d'Argagnan blabla2."), "'")

wordStem("Par exemple, ceci n'est qu'un test. Proliferation armistice é à ï .", language = "french")
wrds <- unlist(strsplit(stripWhitespace(removePunctuation("bla, blabla, exclamation!
                           Encore une ligne, bla.")), "\\s"))

makeAbstractCsv("Ablation", c("un", "deux", "deux"))
inspect(testSource)

toSpace("Par exemple, ceci n'est qu'un test.", "'")
removeWords("Par exemple, ceci n'est qu'un test.", stopwords("french"))
wordStem(c("exemple", "absolutisme"), language = "french")

getTransformations()

### NE PAS EXECUTER ###
### Ecriture de short abstracts en CSV ###
# write("article, word, count", short_abstracts_csv, sep = "\n", append = T, encoding = "utf-8")
# abstractToCsv <- function(inpath, outpath) {
#   conn <- file(inpath, open = "r", encoding = "utf-8")
#   write("article,word,count", outpath, sep = "\n", append = F)
#   counter100 <- 0
#   counter <- 0
#   while(length(line <- readLines(conn, 1)) > 0 ){
#     tryCatch({
#       title <- extractArticle(line)
#       content <- parseAbstract(line)
#       if(length(title) > 0  && length(content) > 0) {
#         content <- toSpace(content, "'")
#         content <- removePunctuation(content)
#         content <- tolower(content)
#         content <- removeWords(content, stopwords("french"))
#         content <- stripWhitespace(content)
#         words <- unlist(strsplit(content, "\\s"))
#         words <- wordStem(words, language = "french")
#         chunk <- makeAbstractCsv(title, words)
#         if(chunk != ""){
#           write(chunk, outpath, sep = "", append = T)
#         }
#         
#       }
#     }, error=function(e){print("eeeeeeeeeeeeeeeeeeeee")
#                          print(e)})
#     counter <- counter + 1
#     if(counter%/%100 > counter100){
#       print(counter)
#       counter100 <- counter %/% 100
#     }
#   }
#   close(conn)
# }
# 
# ### NE PAS EXECUTER ###
#  abstractToCsv(short_abstracts, short_abstracts_csv)

### Transformation de short_abstracts en csv par parties ###
abstractToCsvInParts <- function(inpath, outpath, lengthPart) {
  dir = paste(HOME,"/.pfe/short_abstracts_fr",sep="")
  dir.create(dir)
  outpath <-paste(dir,"short_abstracts_fr",sep="/")
  conn <- file(inpath, open = "r")
  counter100 <- 0
  counter <- 0
  counterLengthPart <- 0
  counterParts <- 0
  write("article,word,count", paste(paste(outpath,counterParts,sep=""),".csv",sep=""), sep = "\n",append=F)
  while(length(line <- readLines(conn, 1)) > 0 ){
    tryCatch({
      title <- extractArticle(line)
      content <- parseAbstract(line)
      if(length(title) > 0  && length(content) > 0) {
        content <- toSpace(content, "'")
        content <- removePunctuation(content)
        content <- tolower(content)
        content <- removeWords(content, stopwords("french"))
        content <- stripWhitespace(content)
        words <- unlist(strsplit(content, "\\s"))
        words <- wordStem(words, language = "french")
        chunk <- fixEncoding(makeAbstractCsv(title, words))
        #Encoding(chunk) <- "utf-8"
        if(!is.null(chunk)){
          if(chunk != ""){
            write(chunk,  paste(paste(outpath,counterParts,sep=""),".csv",sep=""), sep = "", append = T)
          }
        }
      }
    }, error=function(e){print("eeeeeeeeeeeeeeeeeeeee")
                  print(e)})
    counter <- counter + 1
    counterLengthPart <- counterLengthPart + 1
    if(counter%/%100 > counter100){
      print(counter)
      counter100 <- counter %/% 100
    }
    if(counterLengthPart == lengthPart){
      counterLengthPart <- 0
      counterParts <- counterParts + 1
      write("article,word,count", paste(paste(outpath,counterParts,sep=""),".csv",sep=""), sep = "\n", append=F )
    }
  }
  close(conn)
}

### Appel de la transformation de abstractToCsvInParts ###
abstractToCsvInParts(short_abstracts, short_abstracts_csv, 30000)

### TESTS { ###
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
### } /TESTS ###

query = "MATCH (p:category) 
         WHERE p.label = \"Algèbre_linéaire\"
         RETURN p"
query = "merge (cat:ct {name:'numbers'})"

query = "CREATE CONSTRAINT ON (a:article) ASSERT a.title IS UNIQUE"
cypher(db, query)

query = "CREATE CONSTRAINT ON (c:category) ASSERT c.label IS UNIQUE"
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", art_cat_csv,"\" AS row
         MERGE (:article {title:row.article})", sep = "")
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", art_cat_csv, "\" AS row
         MERGE (:category {label:row.categorie})", sep = "")
cypher(db, query)

query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", art_cat_csv, "\" AS row
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

query = "CREATE CONSTRAINT ON (w:word) ASSERT w.stem IS UNIQUE"
cypher(db, query)

### NE PAS EXECUTER ###
#query = paste("USING PERIODIC COMMIT 1000
#         LOAD CSV WITH HEADERS FROM \"file:", short_abstracts_csv, "\" AS row
#         MERGE (:word {stem:row.word})", sep = "")
#cypher(db, query)


saveBddPart <- function(part){
  dir = paste(HOME,"/.pfe/short_abstracts_fr",sep="")
  outpath <-paste(dir,"short_abstracts_fr",sep="/")
  
  query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", paste(paste(outpath,i,sep=""),".csv",sep=""), "\" AS row
         MERGE (:word {stem:row.word})", sep = "")
  cypher(db, query)
  print(paste(file," imported"))
}
for(i in 0:37){
  saveBddPart(i)
}

saveBddBisPart <- function(part){
  dir = paste(HOME,"/.pfe/short_abstracts_fr",sep="")
  outpath <-paste(dir,"short_abstracts_fr",sep="/")
  file <- paste(paste(outpath,i,sep=""),".csv",sep="")
    query = paste("USING PERIODIC COMMIT 1000
         LOAD CSV WITH HEADERS FROM \"file:", file, "\" AS row
         MATCH (a:article {title: row.article})
         MATCH (w:word {stem: row.word})
         MERGE (a)-[r:contains {count:row.count}]->(w)", sep = "")
  cypher(db, query)
  print(paste(file," imported"))
}


### Execution decoupee afin de controler le nombre de fichiers a importer ###
### HASHTAG SAUCE ###
for(i in 0:6){
  saveBddBisPart(i)
}
for(i in 7:13){
  saveBddBisPart(i)
}
for(i in 14:20){
  saveBddBisPart(i)
}
for(i in 21:27){
  saveBddBisPart(i)
}
for(i in 28:34){
  saveBddBisPart(i)
}
for(i in 34:37){ # vérifier qu'il y a bien 37 fichiers créés
  saveBddBisPart(i)
}

# has_angle_brackets <- cmpfun( function( string ) {
#   return ( grepl ("<", string, fixed = T) & grepl(">", string, fixed = T))
# })
# 
# longer_than_4 <- cmpfun( function( string ){
#   return ( nchar(string) > 4 )
# })
# 
# non_empty_string <- cmpfun( function( string ){
#   return ( nchar(string) > 0 )
# })
# 
# harvest4 <- cmpfun(function(string_array) {
#   return(Filter(longer_than_4, string_array))
# })
# 
# harvest <- cmpfun(function(string_array) {
#   return(Filter(non_empty_string, string_array))
# })
# 
# has_fixed_pattern <- cmpfun( function(pattern, string) {
#   return(grepl(pattern, string, fixed = T))
# })
# 
# is_category_resource <- cmpfun(function(string) {
#   return(has_fixed_pattern("resource/Catégorie:", string))
# })
# 
# is_article <- cmpfun(function(string) {
#   return((!has_fixed_pattern("resource/Catégorie:", string))&has_fixed_pattern("resource/", string))
# })
# 
# 
# 
# splitTripleXml <- cmpfun( function(line) {
#   parts <- harvest4(strsplit(line, "[<]|[>]")[[1]])
#   if(length(parts) < 3) {
#     throw("invalid line")
#   }
#   return(parts)
# })
# 
# extract_article <- cmpfun(function(string) {
#   parts <- splitTripleXml(string)
#   part <- Filter(is_article, parts)
#   elements <- harvest(strsplit(part, "/")[[1]])
#   return (tail(elements, 1))
# })
# 
# extract_category <- cmpfun(function(string){
#   parts <- splitTripleXml(string)
#   part <- tail(Filter(is_category_resource, parts), 1)
#   elements <- harvest(strsplit(part, "/")[[1]])
#   atoms <- harvest(strsplit(tail(elements, 1), ":")[[1]])
#   print("atoms :")
#   print(atoms)
#   return(tail(atoms, 1))
# })
# 
# conn <- file(paste(HOME, fileName, sep = "/"), open = "r")
# addConstraint(db, "article", "title")
# addConstraint(db, "category", "label")
# line <- readLines(conn, 1)
# line <- readLines(conn, 1)
# query1 <- extractArticleMQ(line)
# query2 <- extractCategoryMQ(line)
# query3 <- extractIs_underMQ(line)
# print(query1)
# print(query2)
# print(query3)
# cypher(db, query1)
# cypher(db, query2)
# cypher(db, query3)
# #ca <- createNode(db, "article", title = extractArticle(line))
# #cc <- createNode(db, "category", label = extractCategory(line))
# #createRel(ca, "is_under", cc )
# close(conn)
# 
# 
# 
# 
# 
# conn <- file(paste(HOME, fileName, sep = "/"), open = "r")
# counter100 <- 0
# counter <- 0
# while(length(line <- readLines(conn, 1)) > 0 ){
#     tryCatch({
#       query1 <- extractArticleMQ(line)
#       query2 <- extractCategoryMQ(line)
#       query3 <- extractIs_underMQ(line)
#       cypher(db, query1)
#       cypher(db, query2)
#       cypher(db, query3)
#     }, error=function(e){})
#     counter <- counter + 1
#     if(counter%/%100 > counter100){
#       print(counter)
#       counter100 <- counter %/% 100
#     }
# }
# close(conn)


