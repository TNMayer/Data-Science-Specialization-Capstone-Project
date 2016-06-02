###############################################################
# function to get the unigram probability of a single(!) String
###############################################################

unigram_probability <- function(fDT1, fDT2, inStr, higher = F) {
  # full formula for the probability refer to Körner 2013, 
  # p. 13, (39)
  
  # first get the last words out of the inStr vector
  last_word <- word(inStr, -1, sep = "\\s+")
  
  # setkey(fDT2, secname)
  
  # number of words that precede w_i at least once
  # preceding <- fDT2[grep(paste(" ", last_word[1], "$", sep = ""), 
  #                        fDT2$terms), ]
  preceding <- fDT2[J(unique(firstname), last_word), nomatch=0]
  
  # now check if last word exists as first word in bigrams
  if (higher) {
    # P(w_i) <- N1+(*w_i)/N1+(**)
    probability <- nrow(preceding)/nrow(fDT2)
    return(probability)
  } else {
    count_wi <- fDT1[.(last_word)]$freq
    count_all <- sum(fDT1$freq)
    probability <- count_wi/count_all
  }
}

###############################################################
# function to get the bigram probability of a single(!) String
###############################################################

bigram_probability <- function(fDT1, fDT2, fDT3, D2, inStr, higher = F) {
  # full formula for the probability refer to Körner 2013, 
  # p. 14, (41)
  
  # get the last words of the input string needed for the computation
  # last_2_words <- last_n_words(inStr, 2)
  first <- word(inStr, -2, sep = "\\s+")
  sec <- word(inStr, -1, sep = "\\s+")
  
  # number of words that precede wi at least once
  # preceding <- fDT3[grep(paste(" ", last_2_words[1], "$", sep = ""), 
  #                        fDT3$terms), ]
  preceding <- fDT3[J(unique(firsttriname), first,
                      sec), nomatch=0]
  
  # count of bigrams with last two words
  # count_2 <- fDT2[fDT2$terms == last_2_words, ]$freq
  count_2 <- fDT2[.(first, sec)]$freq
  
  if(is.na(count_2)) {
    return(unigram_probability(fDT1, fDT2, inStr))
  } else {
    # first get the fixed discount-value
    # D <- discount(fDT2)
    # count penultimate
    # c_penultimate <- fDT1[fDT1$terms == penultimate, ]$freq
    c_penultimate <- fDT1[.(first)]$freq
    # N1+(penultimate_word*)
    # N_penultimate <- nrow(fDT2[grep(paste("^", penultimate, 
    #                                       "\\s", sep = ""), 
    #                            fDT2$terms), ])
    N_penultimate <- nrow(fDT2[.(first)])
    # N1+(*penultimate*)
    # slow vector search
    # N_star_pen_star <- nrow(fDT3[grep(paste("\\s", penultimate, 
    #                                         "\\s", sep = ""), 
    #                                   fDT3$terms), ])
    # fast binary search
    N_star_pen_star <- nrow(fDT3[J(unique(firsttriname), 
                                   first),
                                 nomatch = 0])
    # unigram probability
    unigram_prob <- unigram_probability(fDT1, fDT2, inStr, higher = T)
    bigram_prob_1 <- ((nrow(preceding) - D2)/N_star_pen_star) +
      (D2/N_star_pen_star)*N_penultimate*unigram_prob
    bigram_prob_2 <- ((count_2-D2)/c_penultimate) +
      (D2/c_penultimate)*N_penultimate*unigram_prob
    
    if (higher) {
      return(bigram_prob_1)
    } else {
      return(bigram_prob_2)
    }
  }
}

###############################################################
# function to get the trigram probability of a single(!) String
###############################################################

trigram_probability <- function(inStr, fDT1, fDT2, fDT3, D2, D3) {
  # full formula for the probability refer to Körner 2013, 
  # p. 14, (43)
  
  # get the last words of the input string needed for the computation
  # last_3_words <- last_n_words(inStr, 3)
  # two_preceding <- paste(word(last_3_words, -3, sep = "\\s+"),
  #                        word(last_3_words, -2, sep = "\\s+"))
  
  firsttri <- word(inStr, -3, sep = "\\s+")
  sectri <- word(inStr, -2, sep = "\\s+")
  tritri <- word(inStr, -1, sep = "\\s+")
  
  # count of trigrams with last three words
  # count_3 <- fDT3[fDT3$terms == last_3_words, ]$freq
  count_3 <- fDT3[.(firsttri, sectri, tritri)]$freq
  
  if(is.na(count_3)) {
    return(bigram_probability(fDT1, fDT2, fDT3, D2, inStr))
  } else {
    # first get the fixed discount-value
    # D <- discount(fDT3)
    # count penultimate
    # c_two_preceding <- fDT2[fDT2$terms == two_preceding, ]$freq
    c_two_preceding <- fDT2[.(firsttri, sectri)]$freq
    # N1+(two_preceding*)
    # slow vector search
    # N_two_preceding <- nrow(fDT3[grep(paste("^", 
    #                                         two_preceding, 
    #                                         "\\s", sep = ""), 
    #                                 fDT3$terms), ])
    # fast binary search
    N_two_preceding <- nrow(fDT3[.(firsttri, sectri)])
    # bigram probability
    bigram_prob <- bigram_probability(fDT1, fDT2,fDT3, D2, inStr, higher = T)
    trigram_prob <- ((count_3-D3)/c_two_preceding) +
      (D3/c_two_preceding)*N_two_preceding*bigram_prob
    return(trigram_prob)
  }
}