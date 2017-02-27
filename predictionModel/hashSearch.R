# test of using a hashtable for perfo and size (trie too big)

ht <- new.env()
ht[["a"]] <- data.frame(word1=c("b","c"), freq=c("23","2"))
ht[["b"]] <- c("c","d")
ht[["c"]] <- c("d","e")

names(ht)
ht[["a"]]

object.size(ht)

library(data.table)
library(dplyr)
trigrams <- fread("trigrams.csv", nrows=10000, stringsAsFactors=F)
dim(trigrams)
tail(trigrams)
trigrams <- count(trigrams, ngram, sort=T)
head(trigrams)

# et maintenant ?
# - partial matching for keys ?
# - pre-processing to store the first words as keys and a tibble of possible words with frequencies (and already sorted) as value ?

# creer un dictionnaire avec les 5 meilleures réponses pour le second mot, et ne prendre que ce dictionnaire pour l'algo de prediction.
# stocker en hashtable (car besoind de stocker un tibble)
# Idea : for each first word, store a tibble of the possible word2 and word3 with frequencies. All work done in pre-processing, just read hashtable for prediction algo. Possibility to add values as user type words.

filter(trigrams, grepl("^i ", ngram))
library(tidyr)
separateTri <- separate(data = trigrams, col = ngram, into = c("word1","word2","word3"))
separateTri

separateTri %>% arrange(word1) 
testWord1 <- separateTri %>% filter(word1 == "a")
dim(testWord1)

by_word1 <- group_by(separateTri, word1)
filterI <- slice(by_word1) %>% filter(word1=="i")
filterI

# create key,value hashtable for each first possible words
ht[["i"]] <- filterI
# propose 5 best matches for 2nd word
slice(ht[["i"]], 1:5)
# filter tibble by second word typed
filter2 <- ht[["i"]] %>% filter(word2=="am")
# propose 5 best match for 3th word
slice(filter2, 1:5)
