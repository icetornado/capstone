library(tm)
setwd(file.path("~", "R programming", "capstone"))
registerDoParallel()

files <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
cname <- file.path("~", "R programming", "capstone", "final","en_US")

#read 1st file
conn <- file(file.path("final","en_US", "en_US.twitter.txt"), "r")
max1 <- 0
cn1 <- 0
while(length(l <- readLines(conn, 1)) > 0) {
        cn1 <- cn1 + 1
       
        if(nchar(l) > max1) {
                max1 <- nchar(l)
        }
}
close(conn)
print(max1)
print(cn1)

#read 2nd file
conn <- file(file.path("final","en_US", "en_US.news.txt"), "r")
max2 <- 0
cn2 <- 0
while(length(l <- readLines(conn, 1)) > 0) {
        cn2 <- cn2 + 1
        
        if(nchar(l) > max2) {
                max2 <- nchar(l)
        }
}
close(conn)
print(max2)
print(cn2)

#read 3rd file
conn <- file(file.path("final","en_US", "en_US.blogs.txt"), "r")
max3 <- 0
cn3 <- 0
while(length(l <- readLines(conn, 1)) > 0) {
        cn3 <- cn3 + 1
        
        if(nchar(l) > max3) {
                max3 <- nchar(l)
        }
}
close(conn)
print(max3)
print(cn3)

#read twitters file for love hate
conn <- file(file.path("final","en_US", "en_US.twitter.txt"), "r")
cnLove <- 0
cnHate <- 0
while(length(l <- readLines(conn, 1)) > 0) {
       if(length(grep("love", l)) > 0) {
               cnLove <- cnLove + 1
       }
        
        if(length(grep("hate", l)) > 0) {
                cnHate <- cnHate + 1
        }
}
close(conn)
print(cnLove)
print(cnHate)

#read twitters file for biostats
conn <- file(file.path("final","en_US", "en_US.twitter.txt"), "r")
while(length(l <- readLines(conn, 1)) > 0) {
        if(length(grep("biostats", l)) > 0) {
                print(l)
        }
        
        if(l == "A computer once beat me at chess, but it was no match for me at kickboxing") {
                print(l)
        }
}
close(conn)

#docs <- Corpus(DirSource(cname))   

#summary(docs)  