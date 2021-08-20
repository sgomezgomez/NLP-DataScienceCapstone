##
## Loading library dependencies
##
library(tm)
library(NLP)
library(RWeka)
library(tokenizers)
library(stringi)
library(wordcloud)
library(toOrdinal)
library(caret)
library(parallel)
library(doParallel)
library(tidyr)
library(stringdist)

## Good milestone reports
##https://rpubs.com/LinaG/779935
##https://rpubs.com/AntChanCPT/792465

# Start the clock!
ptm <- proc.time()

##
## Setting up processing functions
##
## Cluster activation
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
##
## Ngram function
ngram = function(corpus, nmin, nmax, sparseval) {
        token = function(corpus) NGramTokenizer(corpus, Weka_control(min = nmin, max = nmax))
        ##unlist(lapply(ngrams(words(corpus), n), paste, collapse = " "), use.names = FALSE)
        tdm = TermDocumentMatrix(corpus, control = list(tokenizer = token))
        tdm = removeSparseTerms(tdm, sparseval)
        return(tdm)
}
##
## Function to extract ngrams and sort on the frequency
freq_ngram = function(tdm, lowfreq) {
        tdm.freq = findFreqTerms(tdm, lowfreq = lowfreq)
        tdm.df = rowSums(as.matrix(tdm[tdm.freq,]))
        tdm.df = data.frame(word = names(tdm.df), frequency = tdm.df)
        tdm.df = tdm.df[order(-tdm.df$frequency),]
        return(tdm.df)
}
##
## Function to separate the most frequentngrams into columns
sep_ngram = function(ngram_freq, n) {
        ngram = data.frame(ngram = rownames(ngram_freq), freq = ngram_freq$frequency)
        ngram$ngram = as.character(ngram$ngram)
        ngram$n = rep(x = n, nrow(ngram_freq))
        names = as.character()
        ##for (i in 1:n) {
        ##        names = c(names, paste0('ngram.',as.character(toOrdinal(i))))
        ##}
        ##ngram = separate(ngram, col = rows, into = names, sep = ' ')
        pat <- '(.*) (.*)'
        ngram = transform(ngram, context = sub(pat, "\\1", ngram), word = sub(pat, "\\2", ngram))
        return(ngram)
}

##
## Profanity filter files
##
## Download full list of bad words in english
download.file('https://www.freewebheaders.com/download/files/full-list-of-bad-words_csv-file_2021_01_18.zip', 
              method = 'curl', destfile = 'full-list-of-bad-words_csv-file_2021_01_18.zip')
unzip('full-list-of-bad-words_csv-file_2021_01_18.zip')
badwords = read.csv('full-list-of-bad-words_csv-file_2021_01_18.csv', header = FALSE, 
                    sep = ',', col.names = c('word', '', '', ''))
badwords = iconv(badwords$word, "latin1", "ASCII", sub = "")

## Setting seed for reproducibility purposes
set.seed(100)

## Downloading and uncompressing datasets
##download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip', 
##              method = 'curl', destfile = 'Coursera-SwiftKey.zip')
##unzip('Coursera-SwiftKey.zip')

## 
## Data Science Capstone data sets
## 
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
##
## Data sets
## Sampling datasets
sample_size = 0.01
sample = c(sample(UStwitter, length(UStwitter)*sample_size, replace = FALSE),
                      sample(USnews, length(USnews)*sample_size, replace = FALSE),
                      sample(USblogs, length(USblogs)*sample_size, replace = FALSE))
## Remove unused variables
rm(con, UStwitter, USnews, USblogs, sample_size)

## Creating corpus
corpus = VCorpus(VectorSource(list(sample)))
## Cleaning up corpus (plain text, lowercase, punctuation, numbers, other languages)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
## Remove bad words
corpus = tm_map(corpus, removeWords, badwords)
corpus = tm_map(corpus, stripWhitespace)
## Remove unused variables
rm(sample)
##
## Tokenization of corpus
sparseval = 0.99
## Execute function for 1, 2, 3, 4, 5 ngrams
unigram_tdm = ngram(corpus, 1, 1, sparseval)
bigram_tdm = ngram(corpus, 2, 2, sparseval)
trigram_tdm = ngram(corpus, 3, 3, sparseval)
fourthgram_tdm = ngram(corpus, 4, 4, sparseval)
fifthgram_tdm = ngram(corpus, 5, 5, sparseval)
## Removing corpus object
rm(corpus)
##
## Extracting ngrams
lowfreq = 1
## Execute function for 1, 2, 3, 4, 5 ngrams
unigram_freq = freq_ngram(unigram_tdm, lowfreq)
bigram_freq = freq_ngram(bigram_tdm, lowfreq)
trigram_freq = freq_ngram(trigram_tdm, lowfreq)
fourthgram_freq = freq_ngram(fourthgram_tdm, lowfreq)
fifthgram_freq = freq_ngram(fifthgram_tdm, lowfreq)
## Removing tdm objects
rm(unigram_tdm, bigram_tdm, trigram_tdm, fourthgram_tdm, fifthgram_tdm)
##
## Separating ngrams into columns
## Execute function for 1, 2, 3, 4, 5 ngrams
unigram = sep_ngram(unigram_freq, 1)
bigram = sep_ngram(bigram_freq, 2)
trigram = sep_ngram(trigram_freq, 3)
fourthgram = sep_ngram(fourthgram_freq, 4)
fifthgram = sep_ngram(fifthgram_freq, 5)
unifiedgram = rbind(unigram, bigram, trigram, fourthgram, fifthgram)
saveRDS(unifiedgram, file = 'unifiedgram.rds')
## Removing freq objects and ngram objects
rm(unigram_freq, bigram_freq, trigram_freq, fourthgram_freq, fifthgram_freq, 
   unigram, bigram, trigram, fourthgram, fifthgram, unifiedgram)

##
## English idioms files
##
## Download English idiom files
download.file('https://raw.githubusercontent.com/prateeksaxena2809/EPIE_Corpus/master/Static_Idioms_Corpus/Static_Idioms_Words.txt', 
              method = 'curl', destfile = 'Static_Idioms_Words.txt')
download.file('https://raw.githubusercontent.com/prateeksaxena2809/EPIE_Corpus/master/Formal_Idioms_Corpus/Formal_Idioms_Words.txt', 
              method = 'curl', destfile = 'Formal_Idioms_Words.txt')
## Static idioms file
con <- file('Static_Idioms_Words.txt', "r")
staticIdioms = readLines(con) 
close(con)
## Formal idioms file
con <- file('Formal_Idioms_Words.txt', "r")
formalIdioms = readLines(con) 
close(con)
staticIdioms = iconv(staticIdioms, "latin1", "ASCII", sub = "")
formalIdioms = iconv(formalIdioms, "latin1", "ASCII", sub = "")
##
corpusIdioms = VCorpus(VectorSource(list(staticIdioms, formalIdioms)))
##corpusIdioms = VCorpus(VectorSource(list(staticIdioms)))
##
## Cleaning up idioms corpus (plain text, lowercase, punctuation, numbers, other languages)
corpusIdioms = tm_map(corpusIdioms, PlainTextDocument)
corpusIdioms = tm_map(corpusIdioms, removeWords, stopwords("english"))
corpusIdioms = tm_map(corpusIdioms, content_transformer(tolower))
corpusIdioms = tm_map(corpusIdioms, removePunctuation)
corpusIdioms = tm_map(corpusIdioms, removeNumbers)
## Remove bad words idioms corpus
corpusIdioms <- tm_map(corpusIdioms, removeWords, badwords)
corpusIdioms = tm_map(corpusIdioms, stripWhitespace)
## Removing text file objects
rm(staticIdioms, formalIdioms)
##
## Execute function for 1, 2, 3, 4, 5 ngrams for idioms corpus
sparseval = 0.99
unigram_tdm_idioms = ngram(corpusIdioms, 1, 1, sparseval)
bigram_tdm_idioms = ngram(corpusIdioms, 2, 2, sparseval)
trigram_tdm_idioms = ngram(corpusIdioms, 3, 3, sparseval)
fourthgram_tdm_idioms = ngram(corpusIdioms, 4, 4, sparseval)
fifthgram_tdm_idioms = ngram(corpusIdioms, 5, 5, sparseval)
## Removing idioms corpus object
rm(corpusIdioms)
##
## Execute function for 1, 2, 3, 4, 5 ngrams for idioms
lowfreq = 1
unigram_freq_idioms = freq_ngram(unigram_tdm_idioms, lowfreq)
bigram_freq_idioms = freq_ngram(bigram_tdm_idioms, lowfreq)
trigram_freq_idioms = freq_ngram(trigram_tdm_idioms, lowfreq)
fourthgram_freq_idioms = freq_ngram(fourthgram_tdm_idioms, lowfreq)
fifthgram_freq_idioms = freq_ngram(fifthgram_tdm_idioms, lowfreq)
## Removing corpus tdm objects
rm(unigram_tdm_idioms, bigram_tdm_idioms, trigram_tdm_idioms, fourthgram_tdm_idioms, fifthgram_tdm_idioms)
##
## Execute function for 1, 2, 3, 4, 5 ngrams for idioms
unigram_idioms = sep_ngram(unigram_freq_idioms, 1)
bigram_idioms = sep_ngram(bigram_freq_idioms, 2)
trigram_idioms = sep_ngram(trigram_freq_idioms, 3)
fourthgram_idioms = sep_ngram(fourthgram_freq_idioms, 4)
fifthgram_idioms = sep_ngram(fifthgram_freq_idioms, 5)
unifiedgram_idioms = rbind(unigram_idioms, bigram_idioms, trigram_idioms, 
                           fourthgram_idioms, fifthgram_idioms)
saveRDS(unifiedgram_idioms, file = 'unifiedgram_idioms.rds')
## Removing freq objects and ngram objects
rm(unigram_freq_idioms, bigram_freq_idioms, trigram_freq_idioms, 
   fourthgram_freq_idioms, fifthgram_freq_idioms, unigram_idioms, bigram_idioms, 
   trigram_idioms, fourthgram_idioms, fifthgram_idioms, unifiedgram_idioms)


## Function to predict
## Prepare data
unifiedgram = readRDS(file = 'unifiedgram.rds',.GlobalEnv)
unifiedgram_idioms = readRDS(file = 'unifiedgram_idioms.rds',.GlobalEnv)
inputToPredict = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
inputToPredict = "You're the reason why I smile everyday. Can you follow me please? It would mean the"
inputToPredict = "Hey sunshine, can you follow me and make me the"
##inputToPredict = "Very early observations on the Bills game: Offense still struggling but the"
##inputToPredict = "Go on a romantic date at the"
##inputToPredict = "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
##inputToPredict = "dust them off and be on my;"
##inputToPredict = "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
##inputToPredict = "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
##inputToPredict = "Be grateful for the good times and keep the faith during the"
##inputToPredict = "If this isn't the cutest thing you've ever seen, then you must be"
inputToPredict = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
inputToPredict = "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
inputToPredict = "I'd give anything to see arctic monkeys this"
inputToPredict = "Talking to your mom has the same effect as a hug and helps reduce your"
inputToPredict = "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
inputToPredict = "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"


## Cleaning up input text
inputToPredict = PlainTextDocument(inputToPredict)
inputToPredict = removePunctuation(inputToPredict)
inputToPredict = tolower(inputToPredict)
inputToPredict = removeNumbers(inputToPredict)
inputToPredict = removeWords(inputToPredict, stopwords("english"))
inputToPredict = removeWords(inputToPredict, badwords)
inputToPredict = stripWhitespace(inputToPredict)
inputToPredict = trimws(inputToPredict, which = c('both'))

num = sapply(strsplit(inputToPredict, " "), length)
predict = data.frame()
predict_idioms = data.frame()
tempinput = inputToPredict; context = ''
for (i in 1:num) {
        ## Substring n last words to facilitate search through the ngrams
        pat <- '(.*) (.*)'
        context = paste(sub(pat, "\\2", tempinput), context) 
        context = trimws(context, which = c('both'))
        tempinput = sub(pat, "\\1", tempinput)
        ## Searching through the data sets ngrams
        temp_predict = unifiedgram[unifiedgram$n == (i + 1), ]
        temp_predict$sim = stringsim(context, temp_predict$context)
        temp_predict = temp_predict[grep(pattern = paste0(context, '$'), x = temp_predict$context), ]
        ## Searching through the idioms ngrams
        temp_predict_idioms = unifiedgram_idioms[unifiedgram_idioms$n == (i + 1), ]
        temp_predict_idioms$sim = stringsim(context, temp_predict_idioms$context)
        temp_predict_idioms = temp_predict_idioms[grep(pattern = paste0(context, '$'), x = temp_predict_idioms$context), ]
        predict = rbind(predict, temp_predict, temp_predict_idioms)
}
predict = predict[predict$sim > 0.8, ]
predict = predict[order(-predict$n, -predict$freq), ]
if (nrow(predict) == 0) {
        
}


## https://rpubs.com/wowkazmir/546921

## Cluster stopping
stopCluster(cluster)
registerDoSEQ()

# Stop the clock
proc.time() - ptm
