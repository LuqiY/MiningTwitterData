# the twitteR package makes it very easy to grab data on Twitte.
library(twitteR)
library(plyr)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(fpc)
library(cluster)

# get 1,500 most recent tweets mentioning 'Lexus':
Lexus = searchTwitter('@Lexus', n=1500)
head(Lexus)
Lexus.text = laply(Lexus, function(t) t$getText())
Lexus.text <- iconv(Lexus.text, "ASCII", "UTF-8", sub="")
head(Lexus.text)

# Similary, get 1,500 most recent tweets mentioning some other car brands:
Benz = searchTwitter('@Mercedes-Benz', n=1500)
Benz.text = laply(Benz, function(t) t$getText())
Benz.text <- iconv(Benz.text, "ASCII", "UTF-8", sub="")

BMW = searchTwitter('@BMW', n=1500)
BMW.text = laply(BMW, function(t) t$getText())
BMW.text <- iconv(BMW.text, "ASCII", "UTF-8", sub="")

Toyota = searchTwitter('@Toyota', n=1500)
Toyota.text = laply(Toyota, function(t) t$getText())
Toyota.text <- iconv(Toyota.text, "ASCII", "UTF-8", sub="")

Hyundai = searchTwitter('@Hyundai', n=1500)
Hyundai.text = laply(Hyundai, function(t) t$getText())
Hyundai.text <- iconv(Hyundai.text, "ASCII", "UTF-8", sub="")

Ford = searchTwitter('@Ford', n=1500)
Ford.text = laply(Ford, function(t) t$getText())
Ford.text <- iconv(Ford.text, "ASCII", "UTF-8", sub="")

Nissan = searchTwitter('@Nissan', n=1500)
Nissan.text = laply(Nissan, function(t) t$getText())
Nissan.text <- iconv(Nissan.text, "ASCII", "UTF-8", sub="")

Audi = searchTwitter('@Audi', n=1500)
Audi.text = laply(Audi, function(t) t$getText())
Audi.text <- iconv(Audi.text, "ASCII", "UTF-8", sub="")

# Download the positive and negative words from [https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html] for sentiment analysis.
pos = scan('/Users/youluqi/webdata/positive-words.txt',what = 'character', comment.char = ';')
neg = scan('/Users/youluqi/webdata/negative-words.txt',what = 'character', comment.char = ';')

# Score the tweets of each brand, add new columns to identify the car brand for combining all the scores later.
source('sentiment.r')

Lexus.score = score.sentiment(Lexus.text,pos,neg, .progress = 'text')
Lexus.score$brand = 'Lexus'
head(Lexus.score)
hist(Lexus.score$score)

Benz.score = score.sentiment(Benz.text,pos,neg, .progress = 'text')
Benz.score$brand = 'Benz'
BMW.score = score.sentiment(BMW.text,pos,neg, .progress = 'text')
BMW.score$brand = 'BMW'
Toyota.score = score.sentiment(Toyota.text,pos,neg, .progress = 'text')
Toyota.score$brand = 'Toyota'
Hyundai.score = score.sentiment(Hyundai.text,pos,neg, .progress = 'text')
Hyundai.score$brand = 'Hyundai'
Ford.score = score.sentiment(Ford.text,pos,neg, .progress = 'text')
Ford.score$brand = 'Ford'
Nissan.score = score.sentiment(Nissan.text,pos,neg, .progress = 'text')
Nissan.score$brand = 'Nissan'
Audi.score = score.sentiment(Audi.text,pos,neg, .progress = 'text')
Audi.score$brand = 'Audi'

# Combine the results in a data frame.
scores = rbind(Lexus.score, Benz.score, BMW.score, Toyota.score, Hyundai.score, Ford.score, Nissan.score, Audi.score)

# Visulize the distributions of score from different brands.
ggplot(data = scores) +
        geom_bar(aes(x = score, fill = brand),binwidth = 1) +
        facet_grid(brand~.) +
        theme_bw() + scale_color_brewer(palette="Pastel2")

# Ignore the tweets that has the score from -1 to 1. Only focus on very positive and very negative tweets.
scores$vpos = as.numeric(scores$score > 1)
scores$vneg = as.numeric(scores$score < -1)
head(scores)

# For each car brand, we use (the number of very positive tweets)/(the number of very positive tweets + the number of very negative tweets) as the final score of each brand.
twitter_score = ddply(scores, 'brand', summarise, pos_count = sum(vpos), neg_count = sum(vneg))
twitter_score$all_count = twitter_score$pos_count + twitter_score$neg_count
twitter_score$final_score = round(100*twitter_score$pos_count/twitter_score$all_count)
twitter_score = arrange(twitter_score, desc(final_score))

# Add ACSI score (http://247wallst.com/special-report/2015/08/26/the-best-and-worst-car-brands/) to the score table.
twitter_score$ACSI_score = c(83,81,84,79,77,78,82,82)
twitter_score

ggplot(data = twitter_score) +
        geom_point( aes(x = final_score, y = ACSI_score, label = brand, color=brand),size = 4) +
        geom_smooth(aes(x= final_score, y = final_score, group=1),se=F,method='lm')+
        theme_bw()+
        geom_text(aes(x= final_score, y= ACSI_score, label=brand),position=position_dodge(width=0.9), vjust=-0.6, size = 4)

# In this plot, the points on diagonal line shoud have the same ACSI and twitte predicted score. The points which fall in the upper left side of 
# the line must have a ACSI score higher than twitter predicted score. 
# For Lexus, the ACSI score and predicted score are the same.
# For Audi, Ford, Nissan, the predicted scores are slightly lower than ACSI scores.
# For Hyundai and Benz, the predicted scores are much higher that ACSI score.
# For Toyota and BMW, the ACSI scores are much higher that predicted scores from twitter.

# Since the Benz gets the highest sentiment score, I am going to analyze the Benz tweets deeper: 

## Text Cleaning
# Build a corpus. Then conduct text cleaning.
Benz.text1 = laply(Benz, function(t) t$getText())

Benz.Corpus = Corpus(VectorSource(Benz.text1))
Benz.Corpus <- tm_map(Benz.Corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),mc.cores=1)
Benz.Corpus = tm_map(Benz.Corpus, content_transformer(tolower))
Benz.Corpus = tm_map(Benz.Corpus, PlainTextDocument)
Benz.Corpus = tm_map(Benz.Corpus, removePunctuation, lazy = TRUE)
Benz.Corpus = tm_map(Benz.Corpus, removeNumbers, lazy = TRUE)
reURL = function(x) gsub("http[[:alnum:]]*","",x)
Benz.Corpus = tm_map(Benz.Corpus, reURL, lazy = TRUE)
mystopwords = c(stopwords("english"),"mercedes-benz","mercedes","benz","mercedesbenz","almost","always","ags","wasnt")
Benz.Corpus = tm_map(Benz.Corpus,removeWords,mystopwords,lazy=TRUE)
Benz.Corpus.Copy = Benz.Corpus
Benz.Corpus = tm_map(Benz.Corpus,stripWhitespace,lazy = TRUE)
Benz.Corpus = tm_map(Benz.Corpus, PlainTextDocument)
#Benz.Corpus = tm_map(Benz.Corpus, stemDocument,language = "english",lazy = TRUE)
#Benz.Corpus = tm_map(Benz.Corpus, stemCompletion, dictionary = Benz.Corpus.Copy,lazy = TRUE)

# Build a term-document matrix
tdm = TermDocumentMatrix(Benz.Corpus)
tdm

## Explore frequent terms and their associations
# Find words that occur at least 30 times
freqwords = findFreqTerms(tdm, lowfreq = 30)
freqwords

#The code below identifies which words are associated with "eclass"
eclassassocs = findAssocs(tdm,terms = 'eclass',corlimit=0.2)
eclassassocs

# Generate the Word Cloud
m = as.matrix(tdm)
word.freq = sort(rowSums(m),decreasing = T)
wordcloud(words=names(word.freq),freq=word.freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8, "Dark2"))

# Remove sparse terms
tdm2 = removeSparseTerms(tdm, sparse = 0.95)
m2 = as.matrix(tdm2)
m3 = t(m2)
set.seed(123)

## K-mean clustering
# number of clusters
k = 6
kmeansOut = kmeans(m3,k)
# cluster centers
round(kmeansOut$centers,digits = 3)

#print clusters
for(i in 1:k){
        cat(paste("cluster", i, ": ", sep = ""))
        s = sort(kmeansOut$centers[i, ], decreasing = T)
        cat(names(s)[1:5],"\n")
}


# Partitioning (clustering) of the data into k clusters “around medoids”, it is more robust than kmeans.
pamOut = pamk(m3)
k2 = pamOut$nc
k2
pamOut = pamOut$pamobject

# print cluster medoids
for(i in 1:k2){
        cat("cluster",i,": ",colnames(pamOut$medoids)[which(pamOut$medoids[i,]==1)],"\n")
}
clusplot(pamOut, col.p = pamOut$clustering)

