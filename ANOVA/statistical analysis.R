#Category common terms
library(readr)
commonunigram <- read_delim("~/Desktop/TextAnalysiswithR/Second experiment/outputs/commonunigram.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

View(commonunigram)


summary(commonunigram)
plot(commonunigram$tfidf~ commonunigram$ranking~ commonunigram$lda)
hist(common$`tfidf`)
boxplot(common$`tfidf`)
scatter

boxplot(commonunigram$`ranking`, commonunigram$`tfidf`, commonunigram$lda)
cor(commonunigram$`ranking`, commonunigram$`tfidf`)
cor(commonunigram$`ranking`, commonunigram$`lda`)
cor(commonunigram$`tfidf`, commonunigram$`lda`)

#diff.corr <- function( r1, n1, r2, n2 ){
  
#  Z1 <- 0.5 * log( (1+r1)/(1-r1) )
  #Z2 <- 0.5 * log( (1+r2)/(1-r2) )
  
  #diff   <- Z1 - Z2
  #SEdiff <- sqrt( 1/(n1 - 3) + 1/(n2 - 3) )
 # diff.Z  <- diff/SEdiff
  
#  p <- 2*pnorm( abs(diff.Z), lower=F)
 # cat( "Two-tailed p-value", p , "\n" )
#}

#diff.corr( r1=0., n1=100, r2=0.40, n2=80 )

#diff.corr( r1=0.13, n1=157, r2=-0.02, n2= 115)   
#diff.corr( r1=0.1, n1=100, r2=-0.13, n2= 100)  
cor.test(commonunigram$tfidf, commonunigram$ranking, method = "kendall", alternative = "greater",
       exact = FALSE)
cor.test(commonunigram$ranking, commonunigram$lda, method = "kendall", alternative = "greater",
         exact = FALSE)

cor.test(commonunigram$tfidf, commonunigram$lda, method = "kendall", alternative = "greater",
         exact = FALSE)


plot(density(commonunigram$`tfidf`, na.rm=TRUE))
plot(density(commonunigram$`lda`, na.rm=TRUE))
plot(density(commonunigram$`ranking`, na.rm=TRUE))

plot(ranking~lda, commonunigram)

plot(ranking~tfidf, commonunigram)
abline(lm(commonunigram$`ranking`~ commonunigram$`tfidf`))

plot(lda~tfidf, commonunigram)
abline(lm(commonunigram$`lda`~ commonunigram$`tfidf`))

plot(lda~ranking, commonunigram)
abline(lm(commonunigram$`lda`~ commonunigram$`ranking`))
##########################################################
#tfidf, roc, lda

library(readr)
difference <- read_delim("~/Desktop/TextAnalysiswithR/Second experiment/outputs/difference.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
View(difference)

plot(difference$ranking[1:43],difference$lda[44:86])
boxplot(difference$ranking[1:43],difference$lda[44:100],difference$tfidf[100:150])
summary(difference)

plot(ranking[1:43]~lda[44:86], difference)
abline(lm(difference$ranking[1:43]~difference$lda[44:86]))

plot(ranking[1:43]~tfidf[100:142], difference)
abline(lm(difference$ranking[1:43]~difference$tfidf[100:142]))

plot(lda[44:94]~tfidf[100:150], difference)
abline(lm(difference$lda[44:94] ~ difference$tfidf[100:150]))
###########################################
#analysis of covariance