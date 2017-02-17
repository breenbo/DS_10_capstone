# The goal of this exercise is to build and evaluate your first predictive model. You will use the n-gram and backoff models you built in previous tasks to build and evaluate your predictive model. The goal is to make the model efficient and accurate.
# 
# Tasks to accomplish :
# 
# Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.
# Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.
# 
# 
# Questions to consider :
# 
# - How does the model perform for different choices of the parameters and size of the model?
# - How much does the model slow down for the performance you gain?
# - Does perplexity correlate with the other measures of accuracy?
# - Can you reduce the size of the model (number of parameters) without reducing performance?

# Tasks to accomplish
# 
# Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.
# 
# Questions to consider :
# 
# - How can you efficiently store an n-gram model (think Markov Chains)?
# - How can you use the knowledge about word frequencies to make your model smaller and more efficient?
# - How many parameters do you need (i.e. how big is n in your n-gram model)?
# - Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
# - How do you evaluate whether your model is any good?
# - How can you use backoff models to estimate the probability of unobserved n-grams?

############################################################

# Faire les calculs en back-end (bi, tri, quadra et pentagrams)
# Sauver les resultats dans un csv
# Utiliser les résultats en lisant les csv
# Faire traitement du texte entré par utilisateur : abbreviations et contractions pour correspondre aux données des datasets.
# Faire classement des ngrams les plus courant a partir des mots rentrés
# Enregristrer les mots rentrés (et leur classement) dans un dictionnaire utilisateur, qui peut être ajouter à la base de donnée (au dessus ou en back end ?)

############################################################

library(doMC)
registerDoMC(cores=6) # parallel computation
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(qdap) # cleaning text
library(stringr) # wordcount
library(tidyr) # separate words of ngrams into columns
library(tidytext) # for tidy analysis of texts


overview <- function(texte) {
    data.frame(total.Lines=length(texte), total.words=sum(str_count(texte,'\\w+')),
               mean.word.by.line=mean(str_count(texte,'\\w+')),
               sd.of.mean=sd(str_count(texte,'\\w+')),
               max.word.by.line=max(str_count(texte,'\\w+')))
}

# overview for multiple files
multiOverview <- function(liste) {
    # take a list of character containing names of text variables, then return a 
    # dataframe of stats
    dataSumList <- lapply(liste, function(x) overview(get(x)))
    len <- length(liste)
    dataSum <- data.frame()
    for(i in 1:len) {
        dataSum <- rbind(dataSum, dataSumList[[i]])
    }
    row.names(dataSum) <- liste
    dataSum
}

############################################################
# CREATE TIDY NGRAMS 
############################################################
tidyNGram <- function(texte, lang="en", rmStopword=FALSE, stemwords=TRUE, n.gram=1) {
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
        replace_symbol() %>% replace_abbreviation()

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

############################################################
# PLOT MOST COMMON NGRAMS
############################################################
countPlot <- function(tidyText, nb=50){
    # take a tidyText a plot the n most commons words or ngrams with ggplot
    library(dplyr)
    library(ggplot2)
    nbgram <- str_count(tidyText$ngram[1], '\\w+')
    if(nbgram==1){
        graphTitle <- paste(nb, "most commons words.")
    } else {
        graphTitle <- paste(nb, " most commons ",nbgram,"-grams", sep="")
    }
    tidyText %>%
        count(ngram, sort=TRUE) %>%
        mutate(ngram=reorder(ngram,n)) %>%
        mutate(percent=n/sum(n)*100) %>%
        # splitting to plot nb words
        slice(1:nb) %>%
        # plot the plot
        ggplot(aes(ngram,percent)) +
        geom_bar(stat="identity", fill="lightgreen", color="black") +
        xlab(NULL) +
        ggtitle(graphTitle) +
        theme(axis.text.y = element_text(size=8)) +
        coord_flip()
}

############################################################
# PLOT WORDCLOUD
############################################################
wordCloudPlot <- function(tidyText, nb=100) {
    # take a tidyText and plot a wordCloud of n most common words or ngrams
    library(wordcloud)
    library(RColorBrewer)
    tidyText <- tidyText %>% count(ngram, sort=TRUE)
    wordcloud(words=tidyText$ngram, freq=tidyText$n, max.words=nb, random.order=F,
              rot.per=0.35, scale=c(5,0.5), colors=brewer.pal(8,"Dark2"))
}

############################################################
# PLOT NGRAM COVERAGE
############################################################
coverWords <- function(tidyText, n1=50, n2=90) {
    # take a tidyText and return a data.frame with numbers of unique words to describe 
    # n1 and n2 % of the total dataset.
    library(dplyr)
    tidyText <- tidyText %>% count(ngram, sort=TRUE) %>%
        mutate(ngram=reorder(ngram,n)) %>%
        mutate(percent=n/sum(n)*100) %>%
        mutate(cumulative_percent=cumsum(percent))

    # select first row which are greater than n1 and n2
    x1 <- which(tidyText$cumulative_percent>n1)[1]
    x2 <- which(tidyText$cumulative_percent>n2)[1]
    results <- data.frame(x1,x2, dim(tidyText)[1])

    name.x1 <- paste(n1,"percent")
    name.x2 <- paste(n2,"percent")

    # part to chose if words or ngrams (for labs and title)
    nbgram <- str_count(tidyText$ngram[1], '\\w+')
    if(nbgram==1){
        names(results) <- c(name.x1, name.x2, 'total unique words')
        row.names(results) <- "Number of unique words"
    } else {
        name.x3 <- "total unique ngrams"
        names(results) <- c(name.x1, name.x2, name.x3)
        row.names(results) <- paste("Number of unique ",nbgram,"-grams", sep="")
    }
    results
}

cumulativePlot <- function(tidyText, n1=50, n2=90) {
    # take a tidyText and plot a cumulative plot with nb of unique words to cover 
    # n1 and n1 % of total words
    library(dplyr)
    tidyText <- tidyText %>% count(ngram, sort=TRUE) %>%
        mutate(ngram=reorder(ngram,n)) %>%
        mutate(percent=n/sum(n)*100) %>%
        mutate(cumulative_percent=cumsum(percent))

    # select first row which are greater than n1 and n2
    x1 <- which(tidyText$cumulative_percent>n1)[1]
    x2 <- which(tidyText$cumulative_percent>n2)[1]
    x3 <- dim(tidyText)[1]

    # part to chose if words or ngrams (for labs and title)
    nbgram <- str_count(tidyText$ngram[1], '\\w+')
    if(nbgram==1){
        xLab <- "Number of unique words"
        graphTitle <- "Coverage of unique words"
    } else {
        xLab <- paste("Number of unique ",nbgram,"-grams", sep="")
        graphTitle <- paste("Coverage of unique ",nbgram,"-grams", sep="")
    }

    tidyText %>%
        ggplot(aes(1:nrow(tidyText), cumulative_percent)) +
        geom_point(size=0.2) + xlab(NULL) + ggtitle(graphTitle) +
        ylab("Cumulative percentage (%)") +
        geom_hline(yintercept=n1, col="orange") +
        geom_vline(xintercept=x1, col="orange") +
        geom_hline(yintercept=n2, col="green") +
        geom_vline(xintercept=x2, col="green") +
        geom_vline(xintercept=x3, col="black") +
        scale_x_continuous(breaks=c(x1,x2,x3)) +
        theme(axis.text.x=element_text(angle=-30, hjust=0, size=10)) +
        scale_y_continuous(breaks=c(n1,n2,100))
}
############################################################
# PLOT RELATIONSHIP BETWEEN NGRAMS
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
        # separate words to plot relationship
        separate(col=ngram, into=name, sep=" ") %>%
        # split to plot only nb words
        slice(1:nb) %>%
        graph_from_data_frame()

    # plot relationship between words
    a <- grid::arrow(type="open", angle=15, length=unit(.15, "inches"))
    ggraph(tidyText, layout="fr") +
        geom_edge_link(aes(edge_alpha=n), show.legend=FALSE, arrow=a) +
        geom_node_point(color="lightgreen", size=2) +
        geom_node_text(aes(label=name), repel=TRUE, color="red") +
        theme_void()
}

############################################################

twit <- readLines("../text/en_US.twitter.txt", skipNul=TRUE)
blog <- readLines("../text/en_US.blogs.txt", skipNul=TRUE)
news <- readLines("../text/en_US.news.txt", skipNul=TRUE)


library(stringr)
set.seed(2704)
twitTest <- sample(twit, 5000, replace=F)
blogTest <- sample(blog, 5000, replace=F)
newsTest <- sample(news, 5000, replace=F)

# bigrams
twitBigram <- tidyNGram(twitTest, n=2, stemwords=FALSE)
blogBigram <- tidyNGram(blogTest, n=2, stemwords=FALSE)
newsBigram <- tidyNGram(newsTest, n=2, stemwords=FALSE)
uniqueBigram <- rbind(twitBigram, blogBigram, newsBigram)
dim(uniqueBigram)
head(uniqueBigram)
write.csv(uniqueBigram, file="bigrams.csv")
# sans stem
uniqueBigram %>% filter(grepl('^can ', ngram)) %>% count(ngram, sort=T)

# avec stem
test <- read.csv(file = "bigrams.csv")
test %>% filter(grepl('^motor ', ngram)) %>% count(ngram, sort=T)

# trigrams
twitTrigram <- tidyNGram(twitTest, n=3, stemwords=FALSE)
blogTrigram <- tidyNGram(blogTest, n=3, stemwords=FALSE)
newsTrigram <- tidyNGram(newsTest, n=3, stemwords=FALSE)
uniqueTrigram <- rbind(twitTrigram, blogTrigram, newsTrigram)
dim(uniqueTrigram)
head(uniqueTrigram)
write.csv(uniqueTrigram, file="trigrams.csv")
uniqueTrigram %>% filter(grepl('^i am', ngram)) %>% count(ngram, sort=T)
uniqueTrigram %>% filter(grepl('^can be', ngram)) %>% count(ngram, sort=T)

# quadrigrams
twitQuadrigram <- tidyNGram(twitTest, n=4, stemwords=FALSE)
blogQuadrigram <- tidyNGram(blogTest, n=4, stemwords=FALSE)
newsQuadrigram <- tidyNGram(newsTest, n=4, stemwords=FALSE)
uniqueQuadrigram <- rbind(twitQuadrigram, blogQuadrigram, newsQuadrigram)
dim(uniqueQuadrigram)
head(uniqueQuadrigram)
write.csv(uniqueQuadrigram, file="quadrigrams.csv")
uniqueQuadrigram %>% filter(grepl('^i am sure', ngram)) %>% count(ngram, sort=T)

# pentagrams
twitPentagram <- tidyNGram(twitTest, n=5, stemwords=FALSE)
blogPentagram <- tidyNGram(blogTest, n=5, stemwords=FALSE)
newsPentagram <- tidyNGram(newsTest, n=5, stemwords=FALSE)
uniquePentagram <- rbind(twitPentagram, blogPentagram, newsPentagram)
dim(uniquePentagram)
head(uniquePentagram)
write.csv(uniquePentagram, file="pentagrams.csv")
uniquePentagram %>% filter(grepl('^i am sure they', ngram)) %>% count(ngram, sort=T)




# Future

# finding the best (n+1)grams to permit accuracy and quick response.
# trying ngrams with only the words necessary to cover 90 % of the datasets, to lower the number of possible ngrams, and so increase response speed.
