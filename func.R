## Check NGrams
Check5Gram <- function(x, n5, getNrows) {
    words <- whatislast (x, 4)
    match <- subset(n5, word1 == words[1] & word2 == words[2]
                    & word3 == words[3] & word4 == words[4])
    match <- subset(match, select=c(word5, freq))
    match <- match[order(-match$freq), ]
    sumfreq <- sum(match$freq)
    match$freq <- round(match$freq / sumfreq * 100)
    colnames(match) <- c("nextword","n5.MLE")
    if (nrow(match) < getNrows) {
        getNrows <- nrow(match)
    }
    match[1:getNrows,]
}
Check4Gram <- function(x, n4, getNrows) {
    words <- whatislast (x, 3)
    match <- subset(n4, word1 == words[1] & word2 == words[2]
                    & word3 == words[3])
    match <- subset(match, select=c(word4, freq))
    match <- match[order(-match$freq), ]
    sumfreq <- sum(match$freq)
    match$freq <- round(match$freq / sumfreq * 100)
    colnames(match) <- c("nextword","n4.MLE")
    if (nrow(match) < getNrows) {
        getNrows <- nrow(match)
    }
    match[1:getNrows, ]
}
Check3Gram <- function(x, n3, getNrows) {
    words <- whatislast (x, 2)
    match <- subset(n3, word1 == words[1] & word2 == words[2])
    match <- subset(match, select=c(word3, freq))
    match <- match[order(-match$freq), ]
    sumfreq <- sum(match$freq)
    match$freq <- round(match$freq / sumfreq * 100)
    colnames(match) <- c("nextword","n3.MLE")
    if (nrow(match) < getNrows) {
        getNrows <- nrow(match)
    }
    match[1:getNrows, ]
}
Check2Gram <- function(x, n2, getNrows) {  # n4 df should already exist
    words <- whatislast (x, 1)
    match <- subset(n2, word1 == words[1])
    match <- subset(match, select=c(word2, freq))
    match <- match[order(-match$freq), ]
    sumfreq <- sum(match$freq)
    match$freq <- round(match$freq / sumfreq * 100)
    colnames(match) <- c("nextword","n2.MLE")
    if (nrow(match) < getNrows) {
        getNrows <- nrow(match)
    }
    match[1:getNrows, ]
}