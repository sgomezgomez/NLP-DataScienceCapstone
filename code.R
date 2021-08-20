download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip', 
              method = 'curl', destfile = 'Coursera-SwiftKey.zip')
unzip('Coursera-SwiftKey.zip')


## Loading US twitter data
con <- file("final/en_US/en_US.twitter.txt", "r")
UStwitter = readLines(con) 
close(con) 

## Loading US blogs data
con <- file("final/en_US/en_US.blogs.txt", "r")
USblogs = readLines(con) 
close(con) 

## Loading US news data
con <- file("final/en_US/en_US.news.txt", "r")
USnews = readLines(con) 
close(con) 

maxtwitter = 0
for (t in 1:length(UStwitter)) {
        char = nchar(UStwitter[t])
        if (char > maxtwitter) {maxtwitter = char}
}

maxblogs = 0
for (b in 1:length(USblogs)) {
        char = nchar(USblogs[b])
        if (char > maxblogs) {maxblogs = char}
}

maxnews = 0
for (n in 1:length(USnews)) {
        char = nchar(USnews[n])
        if (char > maxnews) {maxnews = char}
}

length(grep(pattern = 'love', x = UStwitter)) / length(grep(pattern = 'hate', x = UStwitter))

grep(pattern = 'biostats', x = UStwitter)

UStwitter[grep(pattern = 'A computer once beat me at chess, but it was no match for me at kickboxing', x = UStwitter)]