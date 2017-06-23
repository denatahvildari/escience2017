setwd("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/documents")
require(mallet)
documents <- mallet.read.dir("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/documents")
mallet.instances <- mallet.import (documents$id, documents$text, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/en.txt", 
                                   token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#create topic trainer object
n.topics <- 100
topic.model <- MalletLDA(n.topics)

#load documents
topic.model$loadDocuments(mallet.instances)
mallet.word.freqs(topic.model)
## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()

word.freqs <- mallet.word.freqs(topic.model)
word.freqs <- as.matrix(word.freqs)
write.csv(word.freqs, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/termfreq.csv")

## Optimize hyperparameters every 20 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## We can specify the number of iterations. 
topic.model$train(400)

## go through a few iterations where we pick the best topic for each token, 
##rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts.
# Here I want probabilities, so i normalize, and put "smoothed=true so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
doctopics <-as.matrix(doc.topics)

write.csv(doc.topics, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/documenttopics.csv")

topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.words<- as.matrix(topic.words)
dim(topic.words)

write.csv(topic.words, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/topicsprobabilities.csv")
topicwords <- as.matrix(topic.words)
words.topic <- t(topic.words)
wordtopic<-as.matrix(words.topic)
dim(wordtopic)
## transpose & normalize the doc.topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
m1 <-write.csv(topic.docs, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/topictermsprobability.csv")

out <- as.data.frame(out)
View(out)

mallet.top.words(topic.model, topic.words[1,])

## we create a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=10)$words, collapse=" ")
# get keywords for each topic
topics.labels
m2 <- write.csv(topics.labels, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/topics.csv")

# create data.frame 
topic_docs <- data.frame(topic.docs)
topicdoc<-as.matrix(topic_docs)
dim(topicdoc)
names(topic_docs) <- documents$id

## cluster based on shared words
plot(hclust(dist(topic.words)), labels=topics.labels)

#' Calculate similarity matrix
#' Shows which documents are similar to each other by their proportions of topics. 

library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
 topic_df_dist [sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0

# Use kmeans to identify groups of 
km <- kmeans(topic_df_dist, n.topics)
# get names for each cluster
allnames <- vector("list", length = n.topics)
for(i in 1:n.topics){
  allnames[[i]] <- names(km$cluster[km$cluster == i])
}
# Here's the list 
allnames
#' Visualize 
# install.packages("igraph")
library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "red", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)
