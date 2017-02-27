# Katz BackOff model
library(data.table)
library(dplyr)

monogram <- fread("monograms.csv", stringsAsFactors=F)
bigram <- fread("bigrams.csv", stringsAsFactors=F)
trigram <- fread("trigrams.csv", stringsAsFactors=F)
quadgram <- fread("quadgrams.csv", stringsAsFactors=F)
pentagram <- fread("pentagrams.csv", stringsAsFactors=F)

set.seed(2706)
triSample <- sample_n(trigram, size = 100000)
triSample <- triSample %>% count(ngram, sort=T)
triSample %>% filter(grepl("^is been", ngram))

quadsample <- sample_n(quadgram, size = 100000)
quadsample <- quadsample %>% count(ngram, sort=T)
quadsample %>% filter(grepl("^it is been", ngram))


library(tidyr)
quadsample <- quadsample %>% separate(col = ngram, into = c("word1", "word2", "word3", "word4"), sep = " ")
quadsample %>% unite(col = gram, 1:3, sep=" ") %>% filter(gram=="at the same")

