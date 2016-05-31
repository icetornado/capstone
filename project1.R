library(tm)
library(SnowballC)
library(RWeka)
library(doParallel)
library(ggplot2)
library(slam)
setwd(file.path("~", "R programming", "capstone"))
registerDoParallel()
options(mc.cores=4)

#read 1st file
conn <- file(file.path("final","en_US", "en_US.twitter.txt"), "r", encoding='UTF-8')
twitter <- readLines(conn)
close(conn)
length(twitter)
## stripping emoji, non-ascii char
twitter <- iconv(twitter, "UTF-8", "ASCII", sub="")

#read 2nd file
conn <- file(file.path("final","en_US", "en_US.blogs.txt"), "r", encoding='UTF-8')
blogs <- readLines(conn)
close(conn)
length(blogs)
blogs <- iconv(blogs, "UTF-8", "ASCII", sub="")

#read 3rd file
conn <- file(file.path("final","en_US", "en_US.news.txt"), "r", encoding='UTF-8')
news <- readLines(conn)
close(conn)
length(news)
news <- iconv(news, "UTF-8", "ASCII", sub="")
## counting words
twittSentenceCn <- 0
blogsSentenceCn <- 0
newsSentenceCn <- 0

# for(i in 1:length(news)) {
#         newsSentenceCn <- newsSentenceCn + length(gregexpr('[[:alnum:] ][.!?]', news[i])[[1]])
# }
# #2241851 sentences
# for(i in 1:length(blogs)) {
#         blogsSentenceCn <- blogsSentenceCn + length(gregexpr('[[:alnum:] ][.!?]', blogs[i])[[1]])
# }
# #2420195 sentences
# for(i in 1:length(twitter)) {
#         twittSentenceCn <- twittSentenceCn + length(gregexpr('[[:alnum:] ][.!?]', twitter[i])[[1]])
# }
#3701902 sentences

## create samples for each file
## according to http://research.microsoft.com/en-us/um/redmond/groups/srg/papers/2001-joshuago-tr72.pdf
## 50,000 sentences is a good size training set
## proportion sampling 2/2/4 news/blogs/twitts
## avg 2 sentences per line in blogs and news, avg 1 sentence per twitt
## blogs and news sentences are more proper english -> t4, get more

set.seed(124567)
sampleTwitts <- sample(twitter, size = 15000)
sampleBlogs <- sample(blogs, size = 30000)
sampleNews <- sample(news, size = 30000)

samples <- c(sampleTwitts, sampleBlogs, sampleNews)
rm(blogs, news, twitter)

vs <- VectorSource(samples)
docs <- VCorpus(vs)

## remove hastag and preserve intra word dashes
removeMostPunctuation <- function (x, keepHashtag = FALSE) {
        rmpunct <- function(x) {
                x <- gsub("#", "\002", x)
                x <- gsub("[[:punct:]]+", "", x)
                gsub("\002", "#", x, fixed = TRUE)
        }
        
        if (keepHashtag) { 
                x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
                x <- rmpunct(x)
                gsub("\001", "-", x, fixed = TRUE)
        } 
        else {
                rmpunct(x)
        }
}

cleaningCorpus <- function(x, pattern) {
        gsub(pattern, " ", x)       
}


###remove hyperlinks, email addresses, or in twitter @, RT and via
patterns = c("(f|ht)tp(s?)://(.*)[.][a-z]+", "\\w*@\\w*\\.\\w*", "(RT|via)((?:\\b\\W*@\\w+)+)", "\\brt\\b", "rt2win", "<3RT");
for(p in patterns) {
        docs <- tm_map(docs, content_transformer(cleaningCorpus), pattern = p) 
}

## remove punc keep hash
#docs <- tm_map(docs, content_transformer(removeMostPunctuation), keepHash = TRUE)
docs <- tm_map(docs, content_transformer(removeMostPunctuation), keepHash = FALSE)
docs <- tm_map(docs, removeNumbers)

### transfrom tolower, a bug of tm 0.6 tolower function returns a vector, not a corpus
docs <- tm_map(docs, content_transformer(tolower)) 

### remove stop words
#docs <- tm_map(docs, removeWords, stopwords("english"))

## strip white spaces
docs <- tm_map(docs, stripWhitespace)

### stemming
docs <- tm_map(docs, stemDocument)

### finalizing
docs <- tm_map(docs, PlainTextDocument)

bigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tetragramToken <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

options(mc.cores=1)
### creating N-grams
wordCount1 <- rowapply_simple_triplet_matrix(TermDocumentMatrix(docs, control=list(wordLengths = c(1, Inf), tokenize = words, bounds = list(global = c(6,Inf)))), sum)
wordCount2 <- rowapply_simple_triplet_matrix(TermDocumentMatrix(docs, control=list(wordLengths = c(1, Inf), tokenize = bigramToken, bounds = list(global = c(2,Inf)))), sum)
wordCount3 <- rowapply_simple_triplet_matrix(TermDocumentMatrix(docs, control=list(wordLengths = c(1, Inf), tokenize = trigramToken, bounds = list(global = c(2,Inf)))), sum)
#wordcount4 <- rowapply_simple_triplet_matrix(TermDocumentMatrix(docs, control=list(wordLengths = c(1, Inf), tokenize = tetragramToken, bounds = list(global = c(1,Inf)))), sum)

wordCount1 <- sort(wordCount1, decreasing = TRUE)
wordCount2 <- sort(wordCount2, decreasing = TRUE)
wordCount3 <- sort(wordCount3, decreasing = TRUE)
#wordcount4 <- sort(wordcount4, decreasing = TRUE)

saveRDS(wordCount1, file = "data3/wordCount1.Rds")
saveRDS(wordCount2, file = "data3/wordCount2.Rds")
saveRDS(wordCount3, file = "data3/wordCount3.Rds")
#save(wordCount1, wordCount2, wordCount3, file = "data2/Ngrams.RData")
#load("data/Ngrams.RData")

## return the last two words of a text
FilterInput <- function(input) {
        
        #contractionsDF <- read.csv(file.path("data", "contractions.csv"), na.strings = "", stringsAsFactors = FALSE)
        #print(contractionsDF)
        inputDoc <- Corpus(VectorSource(input))
        inputDoc <- tm_map(inputDoc, removePunctuation)
        inputDoc <- tm_map(inputDoc, removeNumbers)
        inputDoc <- tm_map(inputDoc, content_transformer(tolower)) 
        ### remove stop words
        #input <- removeWords(input, stopwords("english"))
        inputDoc <- tm_map(inputDoc, stripWhitespace)
        inputDoc <- tm_map(inputDoc, stemDocument)
        inputDoc <- tm_map(inputDoc, PlainTextDocument)
        
        strArr <- unlist(strsplit(inputDoc[[1]]$content, " "))
        
        return(c(strArr[length(strArr) - 1], strArr[length(strArr)]))
}

library(dplyr)
contractionsDF <- read.csv(file.path("data", "contractions.csv"), na.strings = "", stringsAsFactors = FALSE, header = FALSE)
names(contractionsDF) = c("key", "value")
contractionsDF <- mutate(contractionsDF, x = tolower(gsub("'", "", contractionsDF$key)))

## return words either in their original form or in full extansion form 
FilterOutput <- function(w, contractions) {
        matching <- contractions[grepl(paste0("^", w, "$"), contractions$x),]$value
        
        if(length(matching) > 0) {
                return(matching)  
        }
        else {
                return(w)   
        }
}

PredictKN2 <- function(w, wordCount1, wordCount2, limit = 3, cutoff = 20) {
        # d
        d <- length(wordCount2[wordCount2==1]) / (length(wordCount2[wordCount2==1]) + 2 * length(wordCount2[wordCount2==2]))
        
        # cw1, nw1*, n**
        cw1 <- wordCount1[names(wordCount1) == w]
        
        if(length(cw1) == 0) {
                ## the word doesn't exist
        }
        else {
                # cw1w2, n*w2 
                nw1star <- sum(wordCount2[grepl(paste0("^", w), names(wordCount2))])
                nstarstar <- length(wordCount2)
                
                
                listw1star <- wordCount2[grepl(paste0("^", w, " "), names(wordCount2))]
                
                if(length(listw1star) > 0) {
                        if(length(listw1star) > cutoff) {
                                ubound = cutoff
                        }
                        else {
                                ubound = length(listw1star)
                        }
                        
                        pkn <- rep(NA, ubound)
                        for(i in 1:ubound) {
                                w2 <- unlist(strsplit(names(listw1star[i]), " "))[2]
                                
                                #cw1w2, n*w2
                                cw1w2 <- wordCount2[names(wordCount2) == paste0(w, " ", w2)]
                                nstarw2 <-  sum(grepl(paste0(w2,"$"), names(wordCount2)))
                                
                                pkn[i] <- max(cw1w2 - d, 0)/cw1 + d/cw1*nw1star*nstarw2/nstarstar
                                #print(pkn[i])
                        }
                        
                        probList <- data.frame(w = listw1star[1:ubound], probability = pkn, stringsAsFactors = FALSE)
                        probList <- probList[order(probList$probability, decreasing = TRUE), ]
                        
                        return(probList[1:limit,])
                }
                else {
                        print("length = 0")
                }
                 
        }
       
}

PredictKN3 <- function(w1, w2, wordCount1, wordCount2, wordCount3, limit = 3, cutoff = 10) {
        #calculate d3, d2
        d3 <- length(wordCount3[wordCount3==1]) / (length(wordCount3[wordCount3==1]) + 2 * length(wordCount3[wordCount3==2]))
        d2 <- length(wordCount2[wordCount2==1]) / (length(wordCount2[wordCount2==1]) + 2 * length(wordCount2[wordCount2==2]))
        
        # nw1w2*, cw1w2
        nw1w2star <- sum(grepl(paste0("^", w1, " ", w2, " "), names(wordCount3)))
        cw1w2 <- wordCount2[names(wordCount2) == paste0(w1, " ", w2)]
        
        if(length(cw1w2) == 0) {
                #bailing out to PKN2
                print("cw1w2 = 0")
                return(PredictKN2(w2, wordCount1, wordCount2, limit, cutoff))
        }
        else {
                #nw2*, cw2
                nw2star <- sum(grepl(paste0("^", w2, " "), names(wordCount2)))
                cw2 <- wordCount1[names(wordCount1) == w2]
                
                ## n**
                #nstarw2 <- sum(grepl(paste0(w2,"$"), names(wordCount2)))
                nstarstar <- length(wordCount2)
                
                ## get a list of w1w2* from trigram
                listw1w2star <- wordCount3[grepl(paste0("^", w1, " ", w2, " "), names(wordCount3))]
                
                if(length(listw1w2star) > 0) {
                        if(length(listw1w2star) > cutoff) {
                                ubound = cutoff
                        }
                        else {
                                ubound = length(listw1star)
                        }
                        
                        pkn <- rep(NA, ubound)
                        for(i in 1:ubound) {
                                ## get cw1w2w3, n*w3
                                #print("-------------")
                                w3 <- unlist(strsplit(names(listw1w2star[i]), " "))[3]
                                #print(w3)
                                cw1w2w3 <- wordCount3[names(wordCount3) == paste0(w1, " ", w2, " ", w3)]
                                #print(cw1w2w3)
                                
                                nstarw3 <- sum(grepl(paste0(w3,"$"), names(wordCount2)))
                                #print(nstarw3)
                                
                                ## get cw2w3
                                cw2w3 <- sum(grepl(paste0("^", w2, " ", w3, "$"), names(wordCount2)))
                                #print(cw2w3)
                                pw1w2 <- max(cw2w3 - d2,0 )/cw2 + d2/cw2*nw2star*(nstarw3/nstarstar)
                                #print("d2")
                                #print(d2)
                                #print("cw2")
                                #print(cw2)
                                #print("nw2star")
                                #print(nw2star)
                                #print(pw1w2)
                                
                                ## get pkn
                                pkn[i] <- max(cw1w2w3 - d3, 0)/cw1w2 + d3/cw1w2*nw1w2star*pw1w2
                                #print(pkn[i])
                        }
                        #head(pkn, 20)
                        
                        probList <- data.frame(w = listw1w2star[1:ubound], probability = pkn, stringsAsFactors = FALSE)
                        probList <- probList[order(probList$probability, decreasing = TRUE), ]
                        
                        return(probList[1:limit,])
                }
                else {
                        print("length = 0")
                }
                
        }
       
}
