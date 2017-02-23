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

ngram4Trie <- function(tidyText.csv, save=FALSE){
    # take a ngram tidyText and return a csv before transform to trie
    # ngrams is stored in HD
    # option to be saved on HD or not
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(data.table)
    # count nb words in ngram
    tidyText <- fread(file=tidyText.csv, stringsAsFactors=F)
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
    if(save==TRUE){
        fwrite(x = tidyText, file = paste(nbCol,"grams4trie.csv", sep=""))
    } else { tidyText }
}

biGrams4trie <- ngram4Trie("bigrams.csv")
triGrams4trie <- ngram4Trie("trigrams.csv")
quadGrams4trie <- ngram4Trie("quadgrams.csv")
pentaGrams4trie <- ngram4Trie("pentagrams.csv")
# save on HD
ngram4Trie("bigrams.csv", save=TRUE)
ngram4Trie("trigrams.csv", save=TRUE)
ngram4Trie("quadgrams.csv", save=TRUE)
ngram4Trie("pentagrams.csv", save=TRUE)

############################################################
ngram2Trie <- function(tidyText){
    # create the trie with the ngrams4trie.csv done with ngram4trie()
    # keys = ngram, values = last word and frequencies
    library(triebeard)
    trieVar <- trie(keys=tidyText$clefs, values=tidyText$valeurs)
    trieVar
}

library(data.table)
biGrams4trie <- fread("2grams4trie.csv")
biTrie <- ngram2Trie(biGrams4trie)

triGrams4trie <- fread("3grams4trie.csv")
triTrie <- ngram2Trie(triGrams4trie)

quadGrams4trie <- fread("4grams4trie.csv")
quadTrie <- ngram2Trie(quadGrams4trie)

pentaGrams4trie <- fread("5grams4trie.csv")
pentaTrie <- ngram2Trie(pentaGrams4trie)


nextWords <- function(mot) {
    # take a word and return 3 predictives words by using a trie done before and stored in a global variable
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(data.table)
    nbMot <- str_count(mot, '\\w+')
    # select the trie depending on the number of words
    # read the nTrie depending on the number of words
    nTrie <- switch(nbMot,biTrie,triTrie,quadTrie,pentaTrie)

    nextW <- as.data.frame(prefix_match(trie=nTrie, to_match = mot)[[1]])
    names(nextW) <- "prefix"
    nextW <- nextW %>% separate(col = prefix, into = c('nxt','nb'), sep = "_") 
    nextW$nb <- as.integer(nextW$nb)
    nextW <- nextW %>% arrange(desc(nb))
    nextW[1:5,1]
}

nextWords('you must be an ')
