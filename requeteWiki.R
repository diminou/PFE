# Il s'agit du code R pour le requetage sur Wikipedia

install.packages("WikipediR")
install.packages("testthat")

library(WikipediR)
library(testthat)



# update wikis.RData, which is an .RData file included in WikipediR containing the list of 'active' Wikimedia wikis
RDataRebuild();
# 
categories_in_page <- function(language = NULL, project = NULL, domain = NULL,
                               pages, properties = c("sortkey","timestamp","hidden"),
                               limit = 50, show_hidden = FALSE, clean_response = FALSE,
                               ...){
  
  #Check, construct URL
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  pages <- handle_limits(pages, 50)
  if(show_hidden){
    show_hidden <- "hidden"
  } else {
    show_hidden <- "!hidden"
  }
  url <- url_gen(language, project, domain,
                 paste0("&action=query&prop=categories&clprop=", properties,
                        "&clshow=", show_hidden, "&cllimit=", limit,
                        "&titles=",pages))
  
  #Retrieve, check, return
  content <- query(url, "pagecats", clean_response, ...)
  page_names <- names(unlist(content))
  missing_pages <- sum(grepl(x = page_names, pattern = "missing"))
  if(missing_pages){
    warning("This request contained ",missing_pages," invalid page title(s)", call. = FALSE)
  }
  return(content)
}

pages_in_category <- function(language = NULL, project = NULL, domain = NULL, categories,
                              properties = c("title","ids","sortkey","sortkeyprefix","type","timestamp"),
                              type = c("page","subcat","file"), clean_response = FALSE,
                              ...){
  
  #Format and check
  categories <- gsub(x = categories, pattern = "^", replacement = "Category:")
  categories <- handle_limits(categories, 50)
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  type <- match.arg(type, several.ok = TRUE)
  type <- paste(type, collapse = "|")
  
  #Construct URL
  url <- url_gen(language, project, domain, "&action=query&list=categorymembers&cmtitle=",
                 categories, "&cmprop=", properties, "&cmtype=",type)
  
  #Query and return
  content <- query(url, "catpages", clean_response, ...)
  return(content)
}

#Checks for invalid revision IDs, warns if they're found.
invalid_revs <- function(parsed_response){
  if(!is.null(parsed_response$query$badrevids)){
    warning("This request contained ",length(parsed_response$query$badrevids)," invalid revisionID(s)", call. = FALSE)
  }
  return(invisible())
}

random_page <- function(language = NULL, project = NULL, domain = NULL,
                        namespaces = NULL, as_wikitext = FALSE,
                        clean_response = FALSE, ...){
  
  
  url <- url_gen(language, project, domain, "&action=query&list=random&rnlimit=1")
  if(!is.null(namespaces)){
    url <- paste0(url, "&rnnamespace=", paste(namespaces, collapse = "|"))
  }
  page <- query(url, NULL, FALSE)$query$random[[1]]$title
  content <- page_content(language = language, project = project, domain = domain,
                          page_name = page, as_wikitext = as_wikitext, clean_response = clean_response, ...)
  return(content)
}
page_content <- function(language = NULL, project = NULL, domain = NULL,
                         page_name, page_id = NULL, as_wikitext = FALSE, clean_response = FALSE, ...){
  
  #Format and construct URL.
  if(as_wikitext){
    properties <- "wikitext|revid"
  } else {
    properties <- "text|revid"
  }
  properties <- paste(properties, collapse = "|")
  url <- url_gen(language, project, domain, "&action=parse&prop=", properties)
  if(!is.null(page_id)){
    page_id <- handle_limits(page_id, 1)
    url <- paste0(url, "&pageid=", page_id)
  } else {
    page_name <- handle_limits(page_name, 1)
    url <- paste0(url, "&page=", page_name)
  }
  
  #Run  
  content <- query(url, "pcontent", clean_response, ...)
  
  #Return
  return(content)
}

revision_content <- function(language = NULL, project = NULL, domain = NULL,
                             revisions, properties = c("content","ids","flags","timestamp",
                                                       "user","userid","size",
                                                       "sha1","contentmodel","comment",
                                                       "parsedcomment","tags"),
                             clean_response = FALSE, ...){
  
  #Format, construct URL.
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  revisions <- handle_limits(revisions, 50)
  url <- url_gen(language, project, domain,
                 "&rvcontentformat=text/x-wiki&action=query&prop=revisions&rvprop=",
                 properties, "&revids=",revisions)
  
  #Run
  content <- query(url, "rcontent", clean_response, ...)
  
  #Check for invalid RevIDs
  invalid_revs(content)
  
  #Return
  return(content)
}

handle_limits <- function(parameters, limit){
  if(length(parameters) > limit){
    warning("This option accepts ", limit, " values; you have provided ", length(parameters),
            ". Only the first ", limit, " will be returned.", call. = FALSE)
    parameters <- paste(parameters[1:limit], collapse = "|")
  } else {
    parameters <- paste(parameters, collapse = "|")
  }
  return(parameters)
}

revision_diff <- function(language = NULL, project = NULL, domain = NULL,
                          revisions, properties = c("ids","flags","timestamp","user","userid","size",
                                                    "sha1","contentmodel","comment","parsedcomment",
                                                    "tags","flagged"),
                          direction, clean_response = FALSE, ...){
  
  #Check and construct URL
  direction <- match.arg(direction, c("prev","next","cur"))
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  revisions <- handle_limits(revisions, 50)
  url <- url_gen(language, project, domain, "&action=query&prop=revisions&rvprop=",
                 properties, "&rvdiffto=", direction, "&rvcontentformat=text/css&revids=",
                 revisions)
  
  #Retrieve the content, check for invalid RevIDs and uncached diffs,
  #return.
  content <- query(url, "rdiff", clean_response, ...)
  invalid_revs(content)
  if(sum(grepl(x = names(unlist(content)), pattern = "diff.notcached"))){
    warning("This request contained uncached diffs; these will not be returned", call. = FALSE)
  }
  return(content)
}
url_gen <- function(language, project, domain = NULL, ...){
  
  if(is.null(domain)){
    #Commons and Wikispecies have different URL formats, so those have to be handled in a hinky way.
    if(project %in% c("commons","species")){
      url <- paste0(project, ".wikimedia.org/w/api.php?format=json", ...)
    } else {
      url <- paste0(language, "." ,project, ".org/w/api.php?format=json", ...)
    }
  } else {
    url <- paste0(domain,"/w/api.php?format=json", ...)
  }
  
  #Return
  return(url)
}

random_page()


expect_true({categories_in_page("en","wikipedia", page = "Barack Obama");TRUE})
