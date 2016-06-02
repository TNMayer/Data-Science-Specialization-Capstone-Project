library(stringr)
library(stringi)
library(tm)
library(data.table)
library(qdap)

###############################################################
# load the needed data
###############################################################

load("data/fDT1.RData")
load("data/fDT2.RData")
load("data/fDT3.RData")

setkey(fDT1, terms)
setkey(fDT2, firstname, secname)
setkey(fDT3, firsttriname, sectriname, tritriname)

preprocessString <- function(vector) {
  vector <- gsub("[´’‘]", "'", vector)
  # replace contractions from the string
  vector <- qdap::replace_contraction(vector, replace = '')
  # remove 's ending
  vector <- gsub("'s", '', vector)
  # remove the preceding c
  vector <- gsub("^c", "", vector)
  # remove URL´s from the string
  vector <- gsub('http\\S+\\s*', '', vector)
  # remove punctuations from the string
  vector <- gsub('[[:punct:]]', '', vector)
  # remove all but normal characters
  vector <- gsub(pattern="[^a-zA-Z]", replacement=' ', vector)
  # set all to lower characters
  vector <- tolower(vector)
  # replace all " won t " with " will not "
  vector <- gsub(" won t ", " will not ", vector)
  # replace all " can t " with " can not "
  vector <- gsub(" can t ", " can not ", vector)
  # replace all "n t" with " not "
  vector <- gsub("n t ", " not ", vector)
  # remove all single words " s " (are pretty common)
  vector <- gsub(" s ", " ", vector)
  # replace all words " u " with " you "
  vector <- gsub(" u ", " you ", vector)
  # remove english stopwords from string
  # vector <- qdap::rm_stopwords(vector, 
  #                              tm::stopwords("english"),
  #                              separate = F)
  # stem document
  # vector <- tm::stemDocument(unlist(vector), language = "english")
  # strip not needed white spaces
  vector <- tm::stripWhitespace(vector)
  
  return(vector)
}

backoff <- function(input, unigramDT, bigramDT, trigramDT, maxResults = 3) {
  input <- preprocessString(input)
  if(input == ''|input == "na na") return('Warning: Just input something')
  input <- unlist(strsplit(input, split = " "))
  
  if (length(input) >= 2) {
    # now continue with the trigram stuff
    seektri <- fDT3[.(input[length(input)-1],
                       input[length(input)])][order(-probability)]
    seektri <- na.omit(seektri[1:maxResults,])
    if (nrow(seektri) >= 1) {
      seektri <- seektri[,.(tritriname, probability)]
      names(seektri) <- c('Next Word', 'Probability')
      return(seektri)
    }
  } 
  
  if (length(input) >= 1) {
    # now continue with the bigram stuff
    seekbi <- fDT2[.(input[length(input)])][order(-probability)]
    seekbi <- na.omit(seekbi[1:maxResults,])
    if (nrow(seekbi) >= 1) {
      seekbi <- seekbi[,.(secname, probability)]
      names(seekbi) <- c('Next Word', 'Probability')
      return(na.omit(seekbi[1:maxResults,]))
    } 
  }
  
  seekuni <- fDT1[order(-probability)][1:maxResults,]
  seekuni <- seekuni[,.(terms, probability)]
  names(seekuni) <- c('Next Word', 'Probability')
  return(seekuni)
}