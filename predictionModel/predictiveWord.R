# TODO NEXT :
# 0. save csv ngrams without non ASCII char (use iconv() function)
# 1. create function to transform ngrams in trie with last word and frequencies as values. Keys should be the typed words.
# 2. change 'nextWords' to take n words and look at the corresponding (n+1)gram
# 3. change 'nextWords' to be updated by the user words
# 4. find a way to store trie in HD without to create them each time the function is called...
# 5. in the function nextWord, read the (n+1)Trie depending on the number of words typed


library(triebeard)
library(dplyr)
library(tidyr)
library(data.table)
biTest <- fread(file = "bigrams.csv", stringsAsFactors=F)
triTest <- fread(file="trigrams.csv", stringsAsFactors=F)
quadTest <- fread(file="quadgrams.csv", stringsAsFactors=F)
pentaTest <- fread(file="pentagrams.csv", stringsAsFactors=F)


ngram2Trie <- function(tidyText){
    # take a ngram tidyText and return a trie
    # keys = ngram, values = last word and frequencies
    library(dplyr)
    library(tidyr)
    library(stringr)
    # count nb words in ngram
    nbCol <- str_count(tidyText$ngram[1], '\\w+')
    name <- NULL
    for(i in 1:nbCol){
        name <- c(name,paste("word",i, sep=""))
    }
    finalWord <- name[nbCol]
    # separate last words and merge with frequencies
    tidyText <- tidyText %>%
        count(ngram) %>%
        mutate(clefs=ngram) %>%
        separate(col=ngram, into=name, sep=" ") %>%
        unite(valeurs, get(finalWord), n) %>%
        select(c(clefs,valeurs))
    # create the trie
    trieVar <- trie(keys=tidyText$clefs, values=tidyText$valeurs)
    trieVar
}

biTrie <- ngram2Trie(biTest)
triTrie <- ngram2Trie(triTest)
quadTrie <- ngram2Trie(quadTest)
pentaTrie <- ngram2Trie(pentaTest)

nextWords <- function(mot) {
    # take a word and return 3 predictives words by using a trie done before and stored in a global variable
    nbMot <- str_count(mot, '\\w+')
    # select the trie depending on the number of words
    # read the nTrie depending on the number of words
    nTrie <- switch(nbMot,biTrie,triTrie,quadTrie,pentaTri)

    nextW <- as.data.frame(prefix_match(trie=nTrie, to_match = mot)[[1]])
    names(nextW) <- "prefix"
    nextW <- nextW %>% separate(col = prefix, into = c('nxt','nb'), sep = "_") 
    nextW$nb <- as.integer(nextW$nb)
    nextW <- nextW %>% arrange(desc(nb))
    nextW[1:3,1]
}

nextWords('spider ')
