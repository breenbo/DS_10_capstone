# Tasks to accomplish :
 
# - Explore new models and data to improve your predictive model.
# - Evaluate your new predictions on both accuracy and efficiency.
 
# Questions to consider :
 
# - What are some alternative data sets you could consider using ?
# - What are ways in which the n-gram model may be inefficient ?
# - What are the most commonly missed n-grams? Can you think of a reason why they would be missed and fix that ?
# - What are some other things that other people have tried to improve their model ?
# - Can you estimate how uncertain you are about the words you are predicting ?

############################################################
# test with hashtable for better memory usage and speed ?
# use different datasets, or more bigger datasets
# post-process typed words to fit the ngrams (remove abbreviations and contractions, change misspelled words ?)
# change function in order to use only one ngrams (post-process trie to have n-1 columns and choose th right words depending on the numbers of words typed by user)
# try to work with trie with ngram as key and frequencies as values ?
############################################################

ngram4Trie <- function(tidyText.csv, save=FALSE){
    # take a ngram tidyText and return a csv before transform to trie
    # ngrams is stored in HD
    # option to be saved on HD or not
    library(dplyr)
    library(tibble)
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

ngram2Trie <- function(tidyText){
    # create the trie with the ngrams4trie.csv done with ngram4trie()
    # keys = ngram, values = last word and frequencies
    library(triebeard)
    trieVar <- trie(keys=tidyText$clefs, values=tidyText$valeurs)
    trieVar
}

library(data.table)
ngram4Trie("bigrams.csv", save=TRUE)
biGrams4trie <- fread("2grams4trie.csv")
biTrie <- ngram2Trie(biGrams4trie)

ngram4Trie("trigrams.csv", save=TRUE)
triGrams4trie <- fread("3grams4trie.csv")
triTrie <- ngram2Trie(triGrams4trie)


nextWords <- function(mot) {
    # take a word and return 3 predictives words by using a trie done before and stored in a global variable
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(data.table)
    nbMot <- str_count(mot, '\\w+')
    # select the trie depending on the number of words
    # read the nTrie depending on the number of words
    nTrie <- switch(nbMot, biTrie, triTrie)

    nextW <- as.data.frame(prefix_match(trie=nTrie, to_match = mot)[[1]])
    names(nextW) <- "prefix"
    nextW <- nextW %>% separate(col = prefix, into = c('nxt','nb'), sep = "_") 
    nextW$nb <- as.integer(nextW$nb)
    nextW <- nextW %>% arrange(desc(nb))
    nextW[1:5,1]
}

nextWords('in each ')
