api.key = "96bnf78DaZvm18O11Qb7i6Ufb"
api.secret = "If5kAOb7CjR6tON17TgMPs0GjqLlCMSLpI9EmG4k8reH78jG0P"
access.token = "99977639-FtF7LB0ki2lI4c5kgGnoNwYaStnM77LmYVUaORW2H"
token.secret = "8W52EFxOLmxcQmTMlPHg9ww9oVN2cfGjUq9LOkm8cw3Bx"

#install.packages("RWeka") # To use classification
library(RWeka) #
#install.packages("twitteR")
library(twitteR)
#install.packages("tm")
library(tm)
#install.packages("stringr")
library(stringr)
#install.packages("caret")
library(caret)
#install.packages("rminer")
library(rminer) # Classification Evaluation
#install.packages("kernlab")
library(kernlab)
#install.packages("rpart")
library(rpart) # Decision tree

setup_twitter_oauth(api.key, api.secret, access.token, token.secret)

#Extract tweets. Example


#trump.tweets = searchTwitter("#PresidentTrump", n=5000, lang = 'en')


#converts to data frame
df <- do.call("rbind", lapply(trump.tweets, as.data.frame))
write.csv (df, file = ('rawtweets.csv'))

trump.tweets <- read.csv("rawtweets.csv", header = T)

df <- do.call("rbind", lapply(trump.tweets, as.data.frame))
          
head(df)
#remove the charecters
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL
df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
df$text = gsub("@\\w+", "", df$text)
df$text = gsub("[[:punct:]]", "", df$text)
df$text = gsub("[[:digit:]]", "", df$text)
df$text = gsub("http\\w+", "", df$text)
df$text = gsub("https\\w+", "", df$text)
df$text = gsub("[ \t]{2,}", "", df$text)
df$text = gsub("[ \n]{2,}", "", df$text)
df$text = gsub("^\\s+|\\s+$", "", df$text)
df$text = gsub("amp", "", df$text)
df$text = gsub("[ \n]{2,}", "", df$text)

sample <- df$text

head(sample)

pos.words = scan('positive-words.txt', what='character', comment.char=';') 
neg.words = scan('negative-words.txt', what='character', comment.char=';')



#To Add positive and negative words to the databases

pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader','brilliant')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not', 'fuck')


install.packages("plyr")
install.packages("stringr")

#Function to get the positive and negative match in 

library(plyr)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

install.packages("reshape")

library(reshape)

#In order to clean the tweets
#Returns merged data frame
result = score.sentiment(sample, pos.words, neg.words)

test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative

#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

table_final

write.csv(table_final, file = ('C:/Users/hp/Desktop/Project 525/score data.csv'), eol = "\n")
library(dplyr)
stat <- table_final
stat <- mutate(stat, label=ifelse(stat$Score > 0, 'positive', ifelse(stat$Score < 0, 'negative', 'neutral')))

str(stat)

head(stat)

stat$label <- factor(stat$label)

names(stat)

summary(stat)

label_df = data.frame(stat$Text, stat$label)

label_df

write.csv(label_df, file= ('C:/Users/hp/Desktop/Project 525/Labeled data.csv'), eol = '\n')

write.arff(label_df, file= ('C:/Users/hp/Desktop/Project 525/Labeled data.arff'))
summary(label_df)

summary(stat$Score)

barplot(table(stat$Score),xlab="Tweet Sentiment",col="blue")

worst_tweet = table_final$Text[table_final$Score==-3]
worst_tweet
names(worst_tweet)

best_tweet = table_final$Text[table_final$Score==6]
best_tweet

install.packages("tm")

library(tm)

trumpCorpus = Corpus(VectorSource(stat$Text))
length(trumpCorpus)

tc_tm <- tm_map(trumpCorpus, tolower)
length(tc_tm)


# Remove Numbers
tc_tm <- tm_map(tc_tm, removeNumbers)

# Remove Stop words
trumpCorpus = tm_map(tc_tm,removePunctuation)

# Remove Puctuations 
trumpCorpus = tm_map(trumpCorpus,removeWords,stopwords("english"))


trumpCorpus = tm_map(trumpCorpus,stripWhitespace)

trumpCorpus <- tm_map(trumpCorpus, removeWords, c("presidenttrump", "trump","donald", "rally", "rting", "first","media","cnn","fox","report","harrisburg","today", "watch","day", "line","padeep","trumpral","pakeep","trumpral","presid","people","peopl","speech","aswith","president","the","gzpojqvtb"))

install.packages("SnowballC")

library(SnowballC)
trumpCorpus = tm_map(trumpCorpus,stemDocument, language = "english")
inspect(trumpCorpus[[1]])

trumpTDM = TermDocumentMatrix(trumpCorpus)

trumpTDM

trump.mat = as.matrix(trumpTDM)
length(trump.mat)
trump.mat[226:238,1:30]

word.freq = sort(rowSums(trump.mat),decreasing=T)

word.freq[1:20]

To figure out the most common words.
#summary(trump.mat)
barplot(word.freq[1:20],xlab="Frequent Trump Tweet Words",cex.names=.7)

Prepare the wordcloud
#install.packages("wordcloud")
library(wordcloud)
wordcloud(trumpCorpus, random.order=F,max.words=50, col=rainbow(50), scale=c(3.5,1))

findAssocs(trumpTDM,"media",.0)
trump.mat
