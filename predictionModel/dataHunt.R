# read datasets
twit <- readLines("../text/en_US.twitter.txt", skipNul=TRUE)
blog <- readLines("../text/en_US.blogs.txt", skipNul=TRUE)
news <- readLines("../text/en_US.news.txt", skipNul=TRUE)

# take samples because datasets are too big
set.seed(2704)
twitTest <- sample(twit, 100000, replace=F)
blogTest <- sample(blog, 100000, replace=F)
newsTest <- sample(news, 100000, replace=F)

# clean datasets in order to make ngrams
cleanText <- function(texte, lang="en", rmStopword=FALSE, stemwords=FALSE) {
    # take a text file, and return a possibly clean without stopwords and stemmed 
    # tibble depending if chosen options 
    library(data.table)
    library(dplyr)
    library(qdap)
    library(hunspell)
    library(stringr)
    # cleaning sentences
    texte <- texte %>% 
        replace_contraction() %>%
        tolower() %>%
        replace_number(remove=TRUE) %>%
        replace_ordinal(remove=TRUE) %>%
        replace_symbol() %>% replace_abbreviation()
    # remove non english char
    texte <- iconv(x = texte, from = "ASCII", sub="") 
    # remove non english words
    # better to use hunspell than qdap because qdap causes errors with some chars
    missReplace <- function(string){
        miss <- unlist(hunspell(string))
        for(m in miss){
            string <- str_replace(string, m, "")
        }
        string
    }
    texte <- lapply(texte, missReplace)

    # remove stopwords option
    if(rmStopword==TRUE){
        texte <- rm_stopwords(texte, separate=FALSE)
    } 
    # stemming option
    if(stemwords==TRUE){
        texte <- stemmer(texte)
    }
    # transforms list in tibble
    len <- length(texte)
    textFile <- tibble()
    textFile <- tibble(line=1:len, text=unlist(texte))
    textFile
}

# create ngrams
tidyNGram <- function(texte, n.gram=1){
    # take a tibble text and return tibble of ngrams
    library(tidytext)
    texte <- unnest_tokens(texte, ngram, text, token="ngrams", n=n.gram)
    texte
}

############################################################
library(data.table)

cleanTwit <- cleanText(twitTest)
cleanBlog <- cleanText(blogTest)
cleanNews <- cleanText(newsTest)
cleanTexts <- rbind(cleanTwit, cleanBlog, cleanNews)
fwrite(cleanTexts, 'cleanTexts.csv')

############################################################
library(data.table)
cleanTexts <- fread("cleanTexts.csv", stringsAsFactors=F)

monograms <- tidyNGram(cleanTexts)
fwrite(monograms, 'monograms.csv')

bigrams <- tidyNGram(cleanTexts, n.gram=2)
data.table::fwrite(bigrams, 'bigrams.csv')

trigrams <- tidyNGram(cleanTexts, n.gram=3)
fwrite(trigrams, 'trigrams.csv')

quadgrams <- tidyNGram(cleanTexts, n.gram=4)
fwrite(quadgrams, 'quadgrams.csv')

pentagrams <- tidyNGram(cleanTexts, n.gram=5)
fwrite(pentagrams, 'pentagrams.csv')
