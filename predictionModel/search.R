# Katz BackOff model
##################################################
# TODO
# 0. Clean datasets very conciously in order to remove noise...
# 1. separates all unique words in datasets
# 2. store all unique words in a trie(key=word, value=uniqueNumber)
# 3. for datasets : name of ngram = n
   # 3.1. separate ngrams in n colums with frequencies
   # 3.2. replace all words with corresponding trie value, except for the last columns to easily return prediction
    # -> datasets with almost only integer
# 4. search typed words in trie to retrieve words values
# 5. filter datasets by words values i each columns
# 6. run algo in filtered dataset
# 7. return words in last column as prediction
##################################################

library(data.table)
library(dplyr)
library(tidyr)

monogram <- fread("monograms.csv", stringsAsFactors=F)
bigram <- fread("bigrams.csv", stringsAsFactors=F)
trigram <- fread("trigrams.csv", stringsAsFactors=F)
quadgram <- fread("quadgrams.csv", stringsAsFactors=F)
pentagram <- fread("pentagrams.csv", stringsAsFactors=F)

sample1 <- fread("monosample.csv", stringsAsFactors=F)
sample2 <- fread("bisample.csv", stringsAsFactors=F)

# proba calculus for bigrams
# pre-process dataset
# calculus of d(w) : BackOff coef

# function to choose the good ngram depending on typed words
ngramFind <- function(words){
    nbTypedWords <- str_count(words, pattern = "\\w+")
    ngram <- paste("sample",nbTypedWords, sep="")
    ngram
}


countC <- function(words){
    nSample <- get(ngramFind(words))
    number <- filter(nSample, ngram==words)$n
    if(length(number) > 0){
        number
    } else { 0 }
}


countCStar <- function(words){
    nSample <- get(ngramFind(words))
    wC <- countC(words)
    if((wC > 0) && (wC <= 5)){
        # nC nb bigrams with c count
        nC <- dim(filter(nSample, n==wC))[1]
        # nC1 : nb bigrams with (c+1) count
        nC1 <- dim(filter(nSample, n==wC+1))[1]
        wCStar <- (wC+1)*nC1/nC
        wCStar
    } else {
        wC
    }
}


# calculus of Pbo(w) with d > 0
d <- function(words){
    if(countC(words) > 0) {
        d <- countCStar(words)/countC(words)
    } else { d <- 0 } # 0 show that the bigram doesn't exist in the dataset -> usefull to choose the BackOff formula
    d
}


probML <- function(words){
    # separate words to paste only wn-1 for denominator
    library(stringr)
    nbTypedWords <- str_count(words, pattern = "\\w+")

    if(nbTypedWords==1){
        # prob for monograms
        probML <- countC(words)/sum(sample1$n)
    } else {
        word1 <- word(words, end = nbTypedWords-1)
        probML <- countC(words)/countC(word1)
    }
    probML
}

# calculus of Pbo with d=0
beta1 <- function(words){
    # sum of Pbo on all ngrams containing (n-1)words
    nsample <- get(ngramFind(words))
    nbTypedWords <- str_count(words, pattern = "\\w+")
    word1 <- word(words, end = nbTypedWords-1)
    # filter ngrams by (n-1)words
    nsample <- filter(nsample, grepl(paste("^", word1," ", sep=""), ngram))
    nsample
}

sample2
word1 <- word(typedWords, end = nbTypedWords-1)
sample2 %>% filter(grepl(paste("^", word1, sep=""), ngram))
sapply(beta1(typedWords)$ngram, Pbo)
which(sapply(beta1(typedWords)$ngram, function(x) str_count(x, pattern = "\\w+"))!=2)
str_count("of agriculture's", pattern="\\w+")
# find a pattern to keep agriculture's in one word
# OR remove 's in dataHunt and postprocess of typed words
gsub(pattern = "'s", replacement = "",x = "of agriculture's")

Pbo <- function(words){
    if(d(words)!=0){
        Pbo <- d(words)*probML(words)
    } else {
        Pbo <- "try harder man"
    }
    Pbo
}

typedWords <- "of butter"
d(typedWords)
probML(typedWords)
Pbo(typedWords)

countC("the zoning")
ngramFind('the')

unlist(strsplit(word, split = " "))[2]
length(word)
library(stringr)
nbTypedWords <- str_count(typedWords,pattern = "\\w+")
word(typedWords, end = nbTypedWords-1)

# if bigrams exists and count > 5

# if bigrams exist and count < 5

# if bigram doesn't exist
# use alpha and monograms







triSample <- sample_n(trigram, size = 100000)
triSample <- triSample %>% count(ngram, sort=T)
triSample %>% filter(grepl("^is been", ngram))

quadSample <- sample_n(quadgram, size = 100000)
quadSample <- quadSample %>% count(ngram, sort=T)
quadSample %>% filter(grepl("^it is been", ngram))


library(tidyr)
quadsample <- quadsample %>% separate(col = ngram, into = c("word1", "word2", "word3", "word4"), sep = " ")
quadsample %>% unite(col = gram, 1:3, sep=" ") %>% filter(gram=="at the same")

