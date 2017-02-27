# Katz BackOff model
library(data.table)
library(dplyr)

monogram <- fread("monograms.csv", stringsAsFactors=F)
bigram <- fread("bigrams.csv", stringsAsFactors=F)
trigram <- fread("trigrams.csv", stringsAsFactors=F)
quadgram <- fread("quadgrams.csv", stringsAsFactors=F)
pentagram <- fread("pentagrams.csv", stringsAsFactors=F)

set.seed(2704)
triSample <- sample_n(trigram, size = 10000)
triSample %>% count(ngram, sort=T)

quadsample <- sample_n(quadgram, size = 10000)
quadsample %>% count(ngram, sort=T)

