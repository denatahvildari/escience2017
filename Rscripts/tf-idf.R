library(tm)
library(proxy)
library(dplyr)
library(RWeka)
library(NLP)
# -----------------------------------------------------------------------------------
#                                    TF-IDF
# -----------------------------------------------------------------------------------

#Set the working directory
setwd("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/documents")

#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern ="*.txt")

#read files into a character vector
files <- lapply(filenames,readLines)

#create corpus from vector
docs <- Corpus (VectorSource(files))

# stop words list 
# stopwords("SMART")

myStopwords <- c(stopwords("SMART"), "the" ,"for", "then", "than","they", "about", "a", "b", "an", "to", "for", "becasue")#, "The","For", "Then", "Than","They", "about", "a", "an", "to", "for,") #to add words
docs <- tm_map(docs, removeWords, myStopwords)
writeLines(as.character(docs[[1]]))
#Remove the numbers from documents 
docs <- tm_map(docs, removeNumbers)
#Remove the punctuations from documents 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
writeLines(as.character(docs[[1]]))
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
tdm <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))

tdm_matrix <- as.matrix(tdm)
dim(tdm_matrix)
tdm_matrix <- rowSums(tdm_matrix)
tf_idf<- as.matrix(tdm_matrix)

#dim(term_count)
#tf <- as.matrix( term_count / nrow(term_count))
#dim(tf)
#inspect(removeSparseTerms(tdm[, 1], 0.7))

allterms <- write.csv(tf_idf, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/bigrams.csv")
#idf <- log( ncol(tf) / (1 + rowSums(tf != 0)))
#idf <- diag(idf)
#tf_idf <- crossprod(tf, idf)
#colnames(tf_idf) <- rownames(tf)
#tf_idf <- as.matrix(tf_idf)
#tf_idf <- t(tf_idf)
#tf_idf <- rowSums(tf_idf)
#final <- as.matrix(tf_idf)
#allterms <- write.csv(final, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/finalbigramtfidf.csv")


#Tokenize
#tokenize_ngrams <- function(x, n=2) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

#Stem the token to their roots 
#library(SnowballC)
#docs <- tm_map(docs, stemDocument)

# tokeniz
MC_tokenizer(docs)
docs_tokens <- scan_tokenizer(docs)

#docs-tokens<- strsplit_space_tokenizer <- function(x)
 # unlist(strsplit(as.character(x), "[[:space:]]+"))
#strsplit_space_tokenizer(docs)

#Create a TDM applying TF-IDF weighting 
terms <- TermDocumentMatrix (docs,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE))) 

#Analyse how frequently terms appear by summing the content of all terms (i.e., rows),

freq <-  rowSums(as.matrix(terms))
TFIDFM <- as.matrix(freq)
final_tf_idf <- as.data.frame(as.table(TFIDFM))  
#write the output in a csv file
allterms <- write.csv(TFIDFM, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/allterms.csv")
#################################################
# -----------------------------------------------------------------------------------
#                                    Analysis
# -----------------------------------------------------------------------------------

#we creat three subset of roc terms 
# 1- rocterms-unigram-nosynonyms
# 2- rocterms-synonyms
# 3- rocterms-bigram 

#read the all the into rstudio
wholeset = readLines ("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/wholeset.csv")
#rocterms unigram (single terms)
rocunigram = readLines ("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/rocterms-unigram-nosyn.csv")
common_rocunigram_whole <- as.matrix(intersect(rocunigram, wholeset))
difference_rocunigram_whole <- as.matrix(setdiff(rocunigram, wholeset))

#terms in roc that are ngram 
rocbigram = readLines ("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/rocterms-ngram-nosyn.csv")
common_rocbigram_whole <- as.matrix(intersect(rocbigram, wholeset))
difference_rocbigram_whole <- as.matrix(setdiff(rocbigram, wholeset))

#terms in roc that are labeled as synonyms
rocsynonyms = readLines ("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/rocterms-synonyms.csv")
common_synonyms_whole <- as.matrix(intersect(rocsynonyms, wholeset))
difference_synonyms_whole <- as.matrix(setdiff(rocsynonyms, wholeset))
 
#terms that are only in docs and not in roc.
rocall = readLines ("/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/rocallterms.csv")
doc_diff_roc <- as.matrix(setdiff(wholeset, rocall))
allterms <- write.csv(doc_diff_roc, "/Users/denatahvildari/Desktop/TextAnalysiswithR/Second experiment/outputs/diff.csv")

#concatenate the commons, 
#concatenate the difference, 
#add the tf-idf to the commons and differences, 

library(reader)
list2 <- read_csv("~/Desktop/TextAnalysiswithR/Second experiment/outputs/list2.csv")
class(list2)
list2 <- as.data.frame(list2)

                            
                  
 