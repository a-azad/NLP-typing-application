source("func.R")
#
# LOAD DATA
if (!exists("n5")) n5 <- read.csv("n5.csv", stringsAsFactors=FALSE)
if (!exists("n4")) n4 <- read.csv("n4.csv", stringsAsFactors=FALSE)
if (!exists("n3")) n3 <- read.csv("n3.csv", stringsAsFactors=FALSE)
if (!exists("n2")) n2 <- read.csv("n2.csv", stringsAsFactors=FALSE)
if (!exists("profanities")) profanities <- readLines("profanities.txt", encoding="UTF-8")
#
# Clean The Input
reclean <- function(x) {
  x <- tolower(x)
  x <- gsub("\\S*[0-9]+\\S*", " ", x)
  x <- gsub("e-mail","email", x)
  x <- gsub("^[(]|[)]$", " ", x)
  x <- gsub("[(].*?[)]", " ", x)
  x <- gsub("[^[:alnum:][:space:]'-]", " ", x)
  x <- gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
  x <- gsub("\\s+"," ",x)
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}
#
# Return The Last n Words
whatislast <- function(x, n) {
  x          <- reclean(x)
  words      <- unlist(strsplit(x, " "))
  wordlength <- length(words)
  if (n > wordlength) {
      n <- wordlength
  } else if (n==1) {
      return(words[wordlength])
  } else {
      return_val <- words[wordlength]
      for (i in 1:(n-1)) {
            return_val <- c(words[wordlength-i], return_val)
            }; return_val
  }
}
#
#--------- Function to calculate SBO
#
# From: https://rpubs.com/erodriguez/nlpquanteda
# Stupid Backoff
# Remember SBO is based on the idea of 
# 0.4^0xNc + 0.4^1xNc-1 + 0.4^2xNc-2 + 0.4^3xNc-3, 
#
SBO.score <- function(alpha=0.4, x5, x4, x3, x2) {
  score <- 0
  if (x5 > 0) {
    score <- x5
  } else if (x4 >= 1) {
    score <- x4 * alpha
  } else if (x3 > 0) {
    score <- x3 * alpha * alpha
  } else if (x2 > 0) {
    score <- x2 * alpha * alpha * alpha
  }
  return(round(score,1))
}
#
# Combine into one 
ScoreNgrams <- function(x, nrows=20) {
  #  
  n5.match <- Check5Gram(x, n5, nrows) 
  n4.match <- Check4Gram(x, n4, nrows)
  n3.match <- Check3Gram(x, n3, nrows) 
  n2.match <- Check2Gram(x, n2, nrows)
  #
  merge5n4 <- merge(n5.match, n4.match, by="nextword", all=TRUE)
  merge4n3 <- merge(merge5n4, n3.match, by="nextword", all=TRUE)
  merge3n2 <- merge(merge4n3, n2.match, by="nextword", all=TRUE)
  #
  df <- subset(merge3n2, !is.na(nextword))  
  #
  if (nrow(df) > 0) {
    df <- df[order(-df$n5.MLE, -df$n4.MLE, -df$n3.MLE, -df$n2.MLE), ]
    df[is.na(df)] <- 0; 
    df$score <- mapply(SBO.score, alpha=0.4, df$n5.MLE, df$n4.MLE,
                         df$n3.MLE, df$n2.MLE)
    df <- df[order(-df$score), ]
  }
  return(df)
}
# Apply SBO
SBO <- function(x, alpha=0.4, getNrows=20, showNresults=1,
                          removeProfanity=TRUE) {
  nextword <- ""
  if (x == "") {
    return("the")
  }
  df <- ScoreNgrams(x, getNrows)
  if (nrow(df) == 0) {
    return("and")
  }
  df <- df[df$nextword != "unk", ]  # remove unk
  if (showNresults > nrow(df)) {
    showNresults <- nrow(df)
  }
  if (showNresults == 1) {
    # check if top overall score is shared by multiple candidates
    topwords <- df[df$score == max(df$score), ]$nextword
    # if multiple candidates, randomly select one
    nextword <- sample(topwords, 1)
  } else {
    nextword <- df$nextword[1:showNresults]
  }
  if (removeProfanity) {
    if (nextword %in% profanities) {
      nextword <- "#@?!"
    }
  }
  return(nextword)
}

