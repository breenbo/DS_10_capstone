############################################################
tidyNGram <- function(texte, lang="en", rmStopword=FALSE, stemwords=TRUE, n.gram=1) {
    # take a text file, and return a possibly clean without stopwords and stemmed tidy table depending if chosen options 
    library(data.table)
    library(tidytext)
    library(dplyr)
    library(qdap)
    texte <- texte %>% 
        replace_contraction() %>%
        tolower() %>%
        replace_number(remove=TRUE) %>%
        replace_ordinal(remove=TRUE) %>%
        replace_symbol() %>% replace_abbreviation()

    #### ADD STEMMING FUNCTION : as root is written, model could predict the next word...
    if(rmStopword==TRUE){
        texte <- rm_stopwords(texte, separate=FALSE)
    } 
    if(stemwords==TRUE){
        texte <- stemmer(texte)
    }
    len <- length(texte)
    textFile <- data.table()
    textFile <- data.table(line=1:len, text=texte)
    tidyText <- textFile %>% 
        unnest_tokens(ngram, text, token="ngrams", n=n.gram)
    tidyText
}

twit <- readLines("../text/en_US.twitter.txt", skipNul=TRUE)
set.seed(2704)
twitTest <- sample(twit, size=200, replace=F)
twitTest
library(qdap)
twitTest %>% replace_contraction() %>% tolower %>% replace_number(remove=T) %>% replace_symbol() %>% replace_abbreviation() %>% rm_stopwords(separate=FALSE) %>% stemmer()

test <- tidyNGram(twitTest, n=3, rmStopword=FALSE, stemwords=TRUE)
test %>% count(ngram, sort=T)

############################################################
countPlot <- function(tidyText, nb=75){
    # take a tidyText a plot the n most commons words or ngrams with ggplot
    library(dplyr)
    library(ggplot2)
    tidyText %>%
        count(ngram, sort=TRUE) %>%
        mutate(ngram=reorder(ngram,n)) %>%
        mutate(percent=n/sum(n)*100) %>%
        slice(1:nb) %>%
        ggplot(aes(ngram,percent)) +
        geom_bar(stat="identity", fill="lightgreen", color="green") +
        xlab(NULL) +
        theme(axis.text.y = element_text(size=8)) +
        coord_flip()
}

test %>% count(ngram, sort=T) %>% mutate(ngram=reorder(ngram,n)) %>% mutate(percent=n/sum(n)*100) %>% mutate(cumpercent=cumsum(percent))

countPlot(test, nb=75)

############################################################
wordCloudPlot <- function(tidyText, nb=100) {
    # take a tidyText and plot a wordCloud of n most common words or ngrams
    library(wordcloud)
    library(RColorBrewer)
    tidyText <- tidyText %>% count(ngram, sort=TRUE)
    wordcloud(words=tidyText$ngram, freq=tidyText$n, max.words=nb, random.order=F, rot.per=0.4, scale=c(5,0.5), colors=brewer.pal(8,"Dark2"))
}

############################################################
cumulativePlot <- function(tidyText, n1=50, n2=90) {
    # take a tidyText and plot a cumulative plot with nb of words to cover 0.5 and 0.9 of total words
    library(dplyr)
    tidyText <- tidyText %>% count(ngram, sort=TRUE) %>%
        mutate(ngram=reorder(ngram,n)) %>%
        mutate(percent=n/sum(n)*100) %>%
        mutate(cumulative_percent=cumsum(percent))

    x1 <- which(tidyText$cumulative_percent>n1)[1]
    x2 <- which(tidyText$cumulative_percent>n2)[1]

    tidyText %>%
        ggplot(aes(1:nrow(tidyText), cumulative_percent)) +
        geom_point(size=0.2) + xlab("Number of unique ngrams") +
        ylab("Cumulative percentage (%)") +
        geom_hline(yintercept=n1, col="orange") +
        geom_vline(xintercept=x1, col="orange") +
        geom_hline(yintercept=n2, col="green") +
        geom_vline(xintercept=x2, col="green") +
        scale_x_continuous(breaks=c(x1,x2))
}

cumulativePlot(bigram, n1=50, n2=90)

############################################################
relationPlot <- function(tidyText, nb=100) {
    library(igraph)
    library(ggraph)
    library(stringr)
    library(tidyr)
    # take a tidy ngram and plot relationship between the nb most common words
    # separate column and prepare for plotting
    nbCol <- str_count(tidyText$ngram[1], '\\w+')
    name <- NULL
    for(i in 1:nbCol){
        name <- c(name,paste("word",i, sep=""))
    }
    tidyText <- tidyText %>% 
        count(ngram, sort=T) %>%
        separate(col=ngram, into=name, sep=" ") %>%
        slice(1:nb) %>%
        graph_from_data_frame()

    # plot relationship between words
    a <- grid::arrow(type="open", angle=15, length=unit(.15, "inches"))
    ggraph(tidyText, layout="fr") +
        geom_edge_link(aes(edge_alpha=n), show.legend=FALSE, arrow=a) +
        geom_node_point(color="lightgreen", size=2) +
        geom_node_text(aes(label=name), vjust=1, hjust=1, color="red") +
        theme_void()
}

bigram <- tidyNGram(twitTest, n=2)
bigram
countBi <- bigram %>% count(ngram, sort=T)

relationPlot(bigram)

trigram <- tidyNGram(twitTest, n=3)
relationPlot(trigram)

############################################################
