---
title: "Data Science Capstone - Milestone Report"
author: "Sebastian Gomez"
date: "7/21/2021"
output: 
        html_document:
                keep_md: yes
---



## Summary

This Milestone Report includes the code and results of the Data Science Capstone project data downloading, unzipping and loading into R. Additionally, it also has all the necessary data processing commands to build an actual corpus from the data, refine it so that it doesn't include profane terminology, and clean it by removing punctuation, transforming it to plain text and use lowercase characters only. Finally, some exploratory data analysis was perform to understand the data, at least partially, and to establish next steps towards achieving the course's end goal of building a shiny app that illustrates how a model would be able to predict the next word based on the user's input.

## Data loading

Below is the code to download, uncompress and read the english datafiles (twitter, blogs, news) included in assignment.


```r
## Downloading and uncompressing datasets
##download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip', 
##              method = 'curl', destfile = 'Coursera-SwiftKey.zip')
##unzip('Coursera-SwiftKey.zip')
## Reading data files
twitterfile = 'final/en_US/en_US.twitter.txt'
newsfile = 'final/en_US/en_US.news.txt'
blogsfile = 'final/en_US/en_US.blogs.txt'
## Loading US twitter data
con <- file(twitterfile, "r")
UStwitter = readLines(con) 
close(con) 
## Loading US blogs data
con <- file(blogsfile, "r")
USblogs = readLines(con) 
close(con) 
## Loading US news data
con <- file(newsfile, "r")
USnews = readLines(con)
close(con)
```


## Data files summary

Then, a the following summary was developed including:

* File name
* File size
* Number of lines on each file
* Number of total characters on each file
* Total number of words on each file 


```r
## Data files summary
filesummary = data.frame()
filesummary = rbind(filesummary, c('US.twitter', file.size(twitterfile), stri_stats_general(UStwitter)[1], 
                                   stri_stats_general(UStwitter)[1], 
                                   stri_stats_latex(UStwitter)[4]))
names(filesummary) = c('File', 'Size (bytes)', 'Lines', 'Characters', 'Words')
filesummary = rbind(filesummary, c('US.blogs', file.size(blogsfile), stri_stats_general(USblogs)[1], 
                                   stri_stats_general(USblogs)[1], 
                                   stri_stats_latex(USblogs)[4]))
filesummary = rbind(filesummary, c('US.news', file.size(newsfile), stri_stats_general(USnews)[1], 
                                   stri_stats_general(USnews)[1], 
                                   stri_stats_latex(USnews)[4]))
filesummary
```

```
##         File Size (bytes)   Lines Characters    Words
## 1 US.twitter    167105338 2360148    2360148 30578891
## 2   US.blogs    210160014  899288     899288 37865888
## 3    US.news    205811889   77259      77259  2665742
```

## Data processing

### Data sampling

A sample of 5% of the total lines was also obtained from the three files using the code below:


```r
## Sampling datasets
sample_size = 0.05
sample = c(sample(UStwitter, length(UStwitter)*sample_size, replace = FALSE),
                      sample(USnews, length(USnews)*sample_size, replace = FALSE),
                      sample(USblogs, length(USblogs)*sample_size, replace = FALSE))
```

Now, in order to keep memory usage as low as possible, most intermediate files needed to be removed:


```r
## Remove unused variables
rm(con, UStwitter, USnews, USblogs, sample_size)
```

### Corpus creation a processing

The below code was used to:

1. Create the corpus from the three loaded files

2. Remove non-english words

3. Transform all text to lower case

4. Remove punctuation and numbers


```r
## Creating corpus
corpus = Corpus(VectorSource(list(sample)))
## Cleaning up corpus (plain text, lowercase, punctuation, numbers, other languages)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
```

Additionally, a profanity filter was applied to our corpus, based on the file obtained in the URL referenced in the code below:


```r
## Profanity filter
## Download full list of bad words in english
download.file('https://www.freewebheaders.com/download/files/full-list-of-bad-words_csv-file_2021_01_18.zip', 
              method = 'curl', destfile = 'full-list-of-bad-words_csv-file_2021_01_18.zip')
unzip('full-list-of-bad-words_csv-file_2021_01_18.zip')
badwords = read.csv('full-list-of-bad-words_csv-file_2021_01_18.csv', header = FALSE, 
                    sep = ',', col.names = c('word', '', '', ''))
badwords = iconv(badwords$word, "latin1", "ASCII", sub = "")
## Remove bad words
corpus <- tm_map(corpus, removeWords, badwords)
```


## Basic Exploratory Data Analysis

Below we can find a term document matrix calculated from our text processed corpus, with their respective frequencies, the list of the first 50 words that were more commonly found, and a word cloud plot to illustrate the same:


```r
## Exploratory Data Analysis
termdocmat = TermDocumentMatrix(corpus)
terms = sort(rowSums(as.matrix(termdocmat)), decreasing = TRUE)
## Term matrix
terms[1:50]
```

```
##    the   just   like   will    one    can    get   time   love   good    now 
##  14880  12493  11164  10697  10659   9507   9296   8312   7476   7365   7155 
##    day   know    new    see   back people  great  think   make    you  going 
##   7031   6992   6471   5755   5521   5480   5354   5171   5030   4889   4831 
## thanks really  today    and   much   well    got  first   want    way   this 
##   4810   4801   4784   4611   4609   4497   4402   4396   4358   4245   4063 
##   need   work   even  still  right   also   last    but   said    lol   life 
##   4044   4011   3995   3931   3879   3830   3745   3685   3647   3537   3497 
##   come    its little   take  never    say 
##   3377   3345   3261   3229   3221   3172
```

```r
## Word cloud
wordcloud(words = names(terms),
          freq = terms,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.35,
          colors = c("chartreuse", "cornflowerblue", "darkorange"))
```

![](DSC_MilestoneReport_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Findings and Next steps

The purpose of this Milestone Report is to provide evidence of the capstone's data files being processed appropriately, and to demonstrate the overall prediction algorithm can eventually be developed. For this, a basic exploratory analysis was performed. And some very commonly used terms in the English language were identified. However, other important tasks are required in order to build a model that would allow predicting the next word(s) based on the user's input. Some of these tasks are:

* Building n-gram models including 2, 3, or even more groups of words
* Building a prediction model using machine learning or other text mining and NLP algorithms
* Validate memory usage to ensure good performance
* Building a proper shiny app

Please feel free to reach out or leave a comment via email to sgomezgomez@gmail.com
