options(java.parameters = "-Xmx3g")
options(warn = -1) # ignore warning messages in for-loop

rm(list = ls())

suppressWarnings(library(tm))
suppressWarnings(library(tau))
suppressWarnings(library(stringr))
suppressWarnings(library(R.utils))
suppressWarnings(library(SnowballC))
suppressWarnings(library(RWeka))
suppressWarnings(library(plyr))
suppressWarnings(library(markdown))
suppressWarnings(library(ggplot2))
suppressWarnings(library(MASS))
suppressWarnings(library(data.table))
suppressWarnings(library(qdap))

source('kneser_ney_preprocess.R')

# Define file names with relative folder path
wdpath <- 'D:/03_Coursera/01_Data Science Specialization (Johns Hopkins University)/00_R_WD/Data Science Specialization/Capstone'
setwd(wdpath)

f_en_us_blogs_name <- "Data/final/en_US/en_US.blogs.txt"
f_en_us_twitter_name <- "Data/final/en_US/en_US.twitter.txt"
f_en_us_news_name <- "Data/final/en_US/en_US.news.txt"
fDF_nameA <- "Data/FreqDF1/DF1_"
fDF_nameB <- "Data/FreqDF2/DF2_"
fDF_nameC <- "Data/FreqDF3/DF3_"
fDF_nameD <- "Data/FreqDF4/DF4_"

mergeDFs <- function (df1, df2){
  df1 <- df1[order(df1$terms),]
  df2 <- df2[order(df2$terms),]
  
  df1$freq[df1$terms %in% df2$terms] <- df1$freq[df1$terms %in% df2$terms] + df2$freq[df2$terms %in% df1$terms]
  df3 <- rbind(df1, df2[!(df2$terms %in% df1$terms),])
  df3
}

###############################################################
# function to get the discount value D (refer to Körner 2013
# p. 12, (35))
###############################################################

discount <- function(fDT) {
  D <- nrow(fDT[freq == 1])/(nrow(fDT[freq == 1]) + 
                               2*nrow(fDT[freq == 2]))
  return(D)
}

mergeDTs <- function (dt1, dt2){
  dt1 <- dt1[order(terms)]
  dt2 <- dt2[order(terms)]
  
  # update frequency for the already present terms in dt1
  dt1$freq[dt1$terms %in% dt2$terms] <- 
    dt1$freq[dt1$terms %in% dt2$terms] + 
    dt2$freq[dt2$terms %in% dt1$terms]
  # keep the new terms from dt2
  list <- list(dt1, dt2[!(dt2$terms %in% dt1$terms),])
  dt3 <- rbindlist(list)
  return(dt3)
}

# gets a sample of pSmplSize percent from fData
getSmpl <- function(fData, pSmplSize) {
  nData <- length(fData)
  smplSize <- as.integer(pSmplSize*nData)/100
  smplIndx <- sample(1:nData, size = smplSize, replace = F)
  smplData <- fData[smplIndx]
  return(smplData)
}

preprocessString <- function(vector) {
  vector <- gsub("[´’‘]", "'", vector)
  # replace contractions from the string
  vector <- qdap::replace_contraction(vector, replace = '')
  # remove 's ending
  vector <- gsub("'s", '', vector)
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
  # replace all "n t " with " not "
  vector <- gsub("n t ", " not ", vector)
  # remove all single word " s "
  vector <- gsub(" s ", " ", vector)
  # replace all words " u " with " you "
  vector <- gsub(" u ", " you ", vector)
  
  return(vector)
}

nGrams <- function(corpusDF, grams) {
  ngram <- NGramTokenizer(corpusDF, Weka_control(min = grams, max = grams,
                                               delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngram <- data.frame(table(ngram))
  ngram <- ngram[order(ngram$Freq,decreasing = TRUE),]
  colnames(ngram) <- c("String","Count")
  return(ngram)
}

reduceUnigram <- function (unigramDT, coverage) {
  
  unigramDT <- unigramDT[order(-freq)]
  frequency <- 0
  requiredFrequency <- coverage * sum(unigramDT$freq)
  
  for (i in 1:nrow(unigramDT))
  {
    if (frequency >= requiredFrequency)
    {
      return(unigramDT[1:i,])
    }
    
    frequency <- frequency + unigramDT[i,]$freq
  }
  
  return (unigramDT)
}

get_freq_sum <- function(sentence, fDT1) {
  # split up the sentence to its words
  words <- unlist(strsplit(sentence, split = ' '))
  sum <- sum(sapply(words, function(x) fDT1[.(x)]$freq), 
             na.rm = T)
  return(sum)
}

set_trigram_probabilities <- function(fDT1, fDT2, fDT3, D2, D3, splitsize=1000) {
  ndiv = as.integer(nrow(fDT3)/splitsize)
  # initialize fDT3_prob
  fDT3_prob <- data.table(probability = rep(0, nrow(fDT3)))
  for (i in 1:ndiv) {
    start <- (i-1)*splitsize + 1
    end <- i*splitsize
    fDT3_prob[start:end,] <- sapply(fDT3[start:end,]$terms, trigram_probability,
                                    fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, D2 = D2,
                                    D3 = D3)
    print(paste('Iteration:', i))
  }
  start <- ndiv*splitsize + 1
  end <- nrow(fDT3)
  fDT3_prob[start:end,] <- sapply(fDT3[start:end,]$terms, trigram_probability,
                                  fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, D2 = D2,
                                  D3 = D3)
  fDT3 <- cbind(fDT3, fDT3_prob)
  return(fDT3)
}

set_bigram_probabilities <- function(fDT1, fDT2, fDT3, D2, splitsize=1000) {
  ndiv = as.integer(nrow(fDT2)/splitsize)
  # initialize fDT3_prob
  fDT2_prob <- data.table(probability = rep(0, nrow(fDT2)))
  for (i in 1:ndiv) {
    start <- (i-1)*splitsize + 1
    end <- i*splitsize
    fDT2_prob[start:end,] <- sapply(fDT2[start:end,]$terms, bigram_probability,
                                    fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, 
                                    D2 = D2)
    print(paste('Iteration:', i))
  }
  start <- ndiv*splitsize + 1
  end <- nrow(fDT2)
  fDT2_prob[start:end,] <- sapply(fDT2[start:end,]$terms, bigram_probability,
                                  fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, 
                                  D2 = D2)
  fDT2 <- cbind(fDT2, fDT2_prob)
  return(fDT2)
}

set_unigram_probabilities <- function(fDT1, fDT2, splitsize=1000) {
  ndiv = as.integer(nrow(fDT1)/splitsize)
  # initialize fDT3_prob
  fDT1_prob <- data.table(probability = rep(0, nrow(fDT1)))
  for (i in 1:ndiv) {
    start <- (i-1)*splitsize + 1
    end <- i*splitsize
    fDT1_prob[start:end,] <- sapply(fDT1[start:end,]$terms, unigram_probability,
                                    fDT1 = fDT1, fDT2 = fDT2)
    print(paste('Iteration:', i))
  }
  start <- ndiv*splitsize + 1
  end <- nrow(fDT1)
  fDT1_prob[start:end,] <- sapply(fDT1[start:end,]$terms, unigram_probability,
                                  fDT1 = fDT1, fDT2 = fDT2)
  fDT1 <- cbind(fDT1, fDT1_prob)
  return(fDT1)
}

# Read blog text file
f_en_us_blogs <- readLines(f_en_us_blogs_name, 
                           encoding = "UTF-8", skipNul = FALSE)

# Randomly sample the data sources
smplBlgs <- getSmpl(f_en_us_blogs, 12.5)
f_en_us_blogs <- NULL

# Read twitter text file
f_en_us_twitter <- readLines(f_en_us_twitter_name, 
                             encoding = "UTF-8", skipNul=TRUE)

# Randomly sample the data sources
smplTwt <- getSmpl(f_en_us_twitter, 12.5)
f_en_us_twitter <- NULL

# Read news text file
f_en_us_news <- readLines(f_en_us_news_name, 
                          encoding = "UTF-8", skipNul = FALSE)

# Randomly sample the data sources
smplNews <- getSmpl(f_en_us_news, 12.5)
f_en_us_news <- NULL

# Combine the three data sets together
combData <-  c(smplTwt, smplNews, smplBlgs)

# Remove non print data etc
combData <- preprocessString(combData)

rm(smplTwt, smplNews, smplBlgs)

# Divide data into small chunks
nComb <- length(combData)
sz = 1000
nDiv <- as.integer(nComb/sz)

# get the bad words list
undesired <- readLines("Data/badwords.txt")
undesired <- preprocessString(undesired)

for (i in 1:nDiv ) {
  j <- (i-1)*sz+1
  k <- i*sz
  txtSmpl <- combData[j:k]
  # Create a Corpus from the data txtSmpl
  fCorpus <- VCorpus(VectorSource(txtSmpl))
  
  # Clean up the Corpus
  # fCorpus <- tm_map(fCorpus, removeWords, 
  #                   stopwords("english"))
  # fCorpus <- tm_map(fCorpus, stemDocument, 
  #                   language = "english")
  fCorpus <- tm_map(fCorpus, removeWords, undesired)
  fCorpus <- tm_map(fCorpus, stripWhitespace)
  
  # transform the corpus into a data.frame
  fCorpusDF <-data.frame(text=unlist(sapply(fCorpus,
                             `[`, "content")), 
                             stringsAsFactors = FALSE)
  
  # create the n-grams
  unigram <- nGrams(fCorpusDF, 1)
  bigram <- nGrams(fCorpusDF, 2)
  trigram <- nGrams(fCorpusDF, 3)
  fourgram <- nGrams(fCorpusDF, 4)
  
  fDF_name1 <- paste(fDF_nameA,i,sep="")
  fDF_name2 <- paste(fDF_nameB,i,sep="")
  fDF_name3 <- paste(fDF_nameC,i,sep="")
  fDF_name4 <- paste(fDF_nameD,i,sep="")
  
  write.table(unigram, fDF_name1, col.names = TRUE)
  write.table(bigram, fDF_name2, col.names = TRUE)
  write.table(trigram, fDF_name3, col.names = TRUE)
  write.table(fourgram, fDF_name4, col.names = TRUE)
  
  print(i)
}

rm (fCorpus, fCorpusDF, freq1, tdm1, freq2, tdm2, freq3, tdm3, freq4, tdm4)

setwd(wdpath)

fDF1_final <- "Data/FreqDF/fDF1_final"
f1Name = "Data/FreqDF/fDF1_final.RData"
fDF_name1 <- "Data/FreqDF1/DF1_"

fDF2_final <- "Data/FreqDF/fDF2_final"
f2Name = "Data/FreqDF/fDF2_final.RData"
fDF_name2 <- "Data/FreqDF2/DF2_"

fDF3_final <- "Data/FreqDF/fDF3_final"
f3Name = "Data/FreqDF/fDF3_final.RData"
fDF_name3 <- "Data/FreqDF3/DF3_"

fDF4_final <- "Data/FreqDF/fDF4_final"
f4Name = "Data/FreqDF/fDF4_final.RData"
fDF_name4 <- "Data/FreqDF4/DF4_"

wf1_old <- data.table(terms=NA, freq=NA)[numeric(0), ]
wf2_old <- data.table(terms=NA, freq=NA)[numeric(0), ]
wf3_old <- data.table(terms=NA, freq=NA)[numeric(0), ]
wf4_old <- data.table(terms=NA, freq=NA)[numeric(0), ]

############################################################
# Do the same for wf2, 3 and 4 ??!!!
# correct he just did it for the fourgrams ;-) as expected
############################################################

for (i in 1:nDiv ) {
  # get the data for DF1
  fDF_nameA <- paste(fDF_name1, i, sep="")
  wf1 <- fread(fDF_nameA, header = F, 
               select = c(2:3), 
               col.names = c('terms', 'freq'))
  # get the data for DF2
  fDF_nameB <- paste(fDF_name2,i,sep="")
  wf2 <- fread(fDF_nameB, header = F,
               select = c(2:3),
               col.names = c('terms', 'freq'))
  # get the data for DF3
  fDF_nameC <- paste(fDF_name3,i,sep="")
  wf3 <- fread(fDF_nameC, header = F,
               select = c(2:3),
               col.names = c('terms', 'freq'))
  # get the data for DF1
  fDF_nameD <- paste(fDF_name4,i,sep="")
  wf4 <- fread(fDF_nameD, header = F,
               select = c(2:3),
               col.names = c('terms', 'freq'))
  
  # merge the datasets together for DF1
  wf1_new <- mergeDTs(wf1_old, wf1)
  wf1_old <- wf1_new
  # merge the datasets together for DF2
  wf2_new <- mergeDTs(wf2_old, wf2)
  wf2_old <- wf2_new
  # merge the datasets together for DF3
  wf3_new <- mergeDTs(wf3_old, wf3)
  wf3_old <- wf3_new
  # merge the datasets together for DF4
  wf4_new <- mergeDTs(wf4_old, wf4)
  wf4_old <- wf4_new
  ######################################################
  # check if merge data.table is possible too (it is!)
  ######################################################
  
  # print the step out
  print(i)
}

wf1_new <- wf1_new[with(wf1_new, order(-freq)), ]
write.table(wf1_new, fDF1_final, col.names = TRUE)
save(wf1_new, file=f1Name);

wf2_new <- wf2_new[with(wf2_new, order(-freq)), ]
write.table(wf2_new, fDF2_final, col.names = TRUE)
save(wf2_new, file=f2Name);

wf3_new <- wf3_new[with(wf3_new, order(-freq)), ]
write.table(wf3_new, fDF3_final, col.names = TRUE)
save(wf3_new, file=f3Name);

wf4_new <- wf4_new[with(wf4_new, order(-freq)), ]
write.table(wf4_new, fDF4_final, col.names = TRUE)
save(wf4_new, file=f4Name);

View(head(wf1_new))
View(head(wf2_new))
View(head(wf3_new))
View(head(wf4_new))

#=====================================================

# Define file names with relative folder path
setwd(wdpath)

fDF1_name <- "Data/FreqDF/fDF1_final"
fDF2_name <- "Data/FreqDF/fDF2_final"
fDF3_name <- "Data/FreqDF/fDF3_final"
fDF4_name <- "Data/FreqDF/fDF4_final"

fDT1 <- fread(fDF1_name, select = c(2:3), 
              col.names = c('terms', 'freq'))
fDT2 <- fread(fDF2_name, select = c(2:3), 
              col.names = c('terms', 'freq'))
fDT3 <- fread(fDF3_name, select = c(2:3), 
              col.names = c('terms', 'freq'))
fDT4 <- fread(fDF4_name, select = c(2:3), 
              col.names = c('terms', 'freq'))

##########################################################
# functions to add additional columns to fDT2
##########################################################

fDT2$firstname <- sapply(strsplit(fDT2$terms, ' '), 
                         function(a) a[1])
fDT2$secname <- sapply(strsplit(fDT2$terms, ' '), 
                       function(a) a[2])

##########################################################
# functions to add additional columns to fDT3 
##########################################################

fDT3$firsttriname <- sapply(strsplit(fDT3$terms, ' '), 
                            function(a) a[1])
fDT3$sectriname <- sapply(strsplit(fDT3$terms, ' '), 
                          function(a) a[2])
fDT3$tritriname <- sapply(strsplit(fDT3$terms, ' '), 
                          function(a) a[3])

##########################################################
# functions to add additional columns to fDT3 
##########################################################

fDT4$firstquadname <- sapply(strsplit(fDT4$terms, ' '), 
                            function(a) a[1])
fDT4$secquadname <- sapply(strsplit(fDT4$terms, ' '), 
                          function(a) a[2])
fDT4$triquadname <- sapply(strsplit(fDT4$terms, ' '), 
                          function(a) a[3])
fDT4$quadquadname <- sapply(strsplit(fDT4$terms, ' '), 
                           function(a) a[4])

# first get the needed discount values
D2 <- discount(fDT2)
D3 <- discount(fDT3)

###########################################################
# set all key columns of the data.tables
###########################################################

setkey(fDT1, terms)
setkey(fDT2, firstname, secname)
setkey(fDT3, firsttriname, sectriname, tritriname)
setkey(fDT4, firstquadname, secquadname, triquadname, quadquadname)

#############################################################
# prune the datasets to make them better handable
#############################################################

# prune the sets
fDT1 <- reduceUnigram(fDT1, 0.9)
# seems to be pretty efficient !!!
fDT2 <- fDT2[fDT2$freq > 4,]
fDT3 <- fDT3[fDT3$freq > 4,]
fDT4 <- fDT4[fDT4$freq > 2,]

###########################################################
# calculate the whole probabilieties for all values
###########################################################

setkey(fDT1, terms)
setkey(fDT2, firstname, secname)
setkey(fDT3, firsttriname, sectriname, tritriname)
setkey(fDT4, firstquadname, secquadname, triquadname, quadquadname)

fDT3 <- set_trigram_probabilities(fDT1, fDT2, fDT3, 
                                  D2, D3, splitsize=1000)

fDT2 <- set_bigram_probabilities(fDT1, fDT2, fDT3, 
                                 D2, splitsize=1000)

fDT1 <- set_unigram_probabilities(fDT1, fDT2, splitsize=1000)

fDT1 <- na.omit(fDT1)
fDT2 <- na.omit(fDT2)
fDT3 <- na.omit(fDT3)
fDT4 <- na.omit(fDT4)

##############################################################
# save the datasets
##############################################################

# Define file names with relative folder path
setwd(file.path(wdpath, 'Data/FreqDF'))

save(D2, D3, file="constants.RData")
save(fDT1, file="fDT1.RData");
save(fDT2, file="fDT2.RData");
save(fDT3, file="fDT3.RData");
save(fDT4, file="fDT4.RData");

rm(list = ls())

###############################################################
# load the datasets
###############################################################

load("fDT1.RData")
setkey(fDT1, terms)

load("fDT2.RData")
setkey(fDT2, firstname, secname)

load("fDT3.RData")
setkey(fDT3, firsttriname, sectriname, tritriname)

load("fDT4.RData")
setkey(fDT4, firstquadname, secquadname, triquadname, quadquadname)

load('constants.RData')

# Define file names with relative folder path
wdpath <- 'D:/03_Coursera/01_Data Science Specialization (Johns Hopkins University)/00_R_WD/Data Science Specialization/Capstone'
setwd(wdpath)

options(warn = 0) # switch warning messages on again

################################################################
# now we have the final corpus to build our application on
################################################################