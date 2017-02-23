library(data.table)
library(stringr)

twit <- readLines("../text/en_US.twitter.txt", skipNul=TRUE)
blog <- readLines("../text/en_US.blogs.txt", skipNul=TRUE)
news <- readLines("../text/en_US.news.txt", skipNul=TRUE)

set.seed(2704)
twitTest <- sample(twit, 1000, replace=F)
blogTest <- sample(blog, 100000, replace=F)
newsTest <- sample(news, 100000, replace=F)

tidyNGram <- function(texte, lang="en", rmStopword=FALSE, stemwords=FALSE, n.gram=1) {
    # take a text file, and return a possibly clean without stopwords and stemmed 
    # tidy table depending if chosen options 
    library(data.table)
    library(tidytext)
    library(dplyr)
    library(qdap)
    # cleaning sentences
    texte <- texte %>% 
        replace_contraction() %>%
        tolower() %>%
        replace_number(remove=TRUE) %>%
        replace_ordinal(remove=TRUE) %>%
        replace_symbol() %>% replace_abbreviation() %>%
        iconv(from = "ASCII", sub="") # remove non english char
    # remove non english words ?
    check_text(twit[1])
    check_spelling(twit[1])
    replace_contraction("btw")
    multigsub(pattern = )
    miss <- as.data.frame(which_misspelled(twit[1]), stringsAsFactors=F)
    miss[,1]
    twit[1] <- tolower(twit[1])
    miss1 <- paste(miss[,1])
    miss1
    str_replace(twit[1],miss1, "")
    gsub(pattern = miss1, replacement = "", x = twit[1])

    # remove stopwords option
    if(rmStopword==TRUE){
        texte <- rm_stopwords(texte, separate=FALSE)
    } 
    # stemming option
    if(stemwords==TRUE){
        texte <- stemmer(texte)
    }
    # transforms list in tidy table
    len <- length(texte)
    textFile <- data.table()
    textFile <- data.table(line=1:len, text=texte)
    tidyText <- textFile %>% 
        unnest_tokens(ngram, text, token="ngrams", n=n.gram)
    tidyText
}

bitwit <- tidyNGram(twitTest,n.gram=2)
bitwit
biblog <- tidyNGram(blogTest,n.gram=2)
binews <- tidyNGram(newsTest,n.gram=2)
bigrams <- rbind(bitwit, biblog, binews)
savebi <- bigrams

data.table::fwrite(bigrams, 'bigrams.csv')

tritwit <- tidyNGram(twitTest,n.gram=3)
triblog <- tidyNGram(blogTest,n.gram=3)
trinews <- tidyNGram(newsTest,n.gram=3)
trigrams <- rbind(tritwit, triblog, trinews)
savetri <- trigrams

fwrite(trigrams, 'trigrams.csv')

quadtwit <- tidyNGram(twitTest,n.gram=4)
quadblog <- tidyNGram(blogTest,n.gram=4)
quadnews <- tidyNGram(newsTest,n.gram=4)
quadgrams <- rbind(quadtwit, quadblog, quadnews)
savequad <- quadgrams

fwrite(quadgrams, 'quadgrams.csv')

pentatwit <- tidyNGram(twitTest,n.gram=5)
pentablog <- tidyNGram(blogTest,n.gram=5)
pentanews <- tidyNGram(newsTest,n.gram=5)
pentagrams <- rbind(pentatwit, pentablog, pentanews)
savepenta <- pentagrams
pentagrams$ngram <- iconv(pentagrams$ngram, "ASCII", sub="")

fwrite(pentagrams, 'pentagrams.csv')
