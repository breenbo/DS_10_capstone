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
    nTrie <- switch(nbMot, biTrie, triTrie, quadTrie, pentaTrie)

    nextW <- as.data.frame(prefix_match(trie=nTrie, to_match = mot)[[1]])
    names(nextW) <- "prefix"
    nextW <- nextW %>% separate(col = prefix, into = c('nxt','nb'), sep = "_") 
    nextW$nb <- as.integer(nextW$nb)
    nextW <- nextW %>% arrange(desc(nb))
    nextW[1:5,1]
}

nextWords('from playing ')
