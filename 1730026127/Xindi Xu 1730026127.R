#
#  DATA SPRINT 
#  Text Mining course
#  UIC United International College
#  spring 2020 
#  Author: Xindi XU
#  Student ID: 1730026127
#

#########################################
#
#  LOAD of required libraries
#  and working directory path
#
#########################################

# install.packages("jiebaR")
# install.packages("wordcloud2")
# install.packages("randomForest")
# install.packages("RTextTools")
# install.packages("ggplot2")
# install.packages("Rwordseg")
# install.packages("doBy")
library(jiebaR)
library(wordcloud)
library(wordcloud2)
library(randomForest) 
library(tm)
library(caret)
library(RTextTools)
library(ggplot2)
library(Rwordseg)
library(doBy)

Sys.setlocale(locale="chinese")
path1 = "D:/TM_1730026127/CovidCoronavirus/"
path2 = "D:/TM_1730026127/CovidWuhanBingdu/"
path2_1 = "D:/TM_1730026127/resources/"
path3_1 = "D:/TM_1730026127/WeiboNamedEntity/"
path3_2 = "D:/TM_1730026127/TwitterNamedEntity/"
path3_3 = "D:/TM_1730026127/resources/"
#########################################
#
#  Covid Wuhan Bingdu dataset
#
#########################################

# Step1: The number of weibo changes over time
df1 = readLines(paste0(path1,"part2_wuhanvirus.txt"),encoding = "UTF-8") # read the file
head(df1) # View the first few data

# data processing
df1_clean = gsub(':', ' ',df1)
df1_clean = gsub("\n", " ", df1_clean);
df1_clean = gsub("\r", " ", df1_clean);
df1_clean = gsub(":", " ", df1_clean);
df1_clean = gsub("weibo text", ":", df1_clean);
df1_clean = gsub("weibo temps", ":", df1_clean);
df1_clean = gsub("weibo username", ":", df1_clean);
df1_clean = gsub("weibo uid", ":", df1_clean);
df1_clean = gsub("weibo mid", ":", df1_clean); 

df1 <- data.frame(do.call('rbind', strsplit(as.character(df1_clean),':',fixed=TRUE)))
df1 = cbind( as.character(df1[, 2]), as.character(df1[, 3]) )
colnames(df1) = c("text","date") # give names to columns
head(df1)

newdata <- df1[order(df1[,'date']),] ;head(newdata)   #sort on the date field by increasing order
# just take the exact data format I want (year-month-day)
newdata = substring(newdata[,'date'],2,11)
head(newdata)
freq = table(newdata)
plot(freq, ylab = "#messsages", main="number of weibo over time",  xaxt="n", xlab='time')
axis(side = 1, at = 1:length(names(freq)),labels = names(freq), las=2,cex.axis = 0.5)

# Step2: Make wordcloud according to word frequency
df2 = readLines(paste0(path1,"part2_wuhanvirus.txt"),encoding = "UTF-8")
head(df2) # View the first few data
# data processing
df2_clean = gsub(':', ' ',df2)
df2_clean = gsub("\n", " ", df2_clean);
df2_clean = gsub("\r", " ", df2_clean);
df2_clean = gsub(":", " ", df2_clean);
df2_clean = gsub("weibo text", ":", df2_clean);
df2_clean = gsub("weibo temps", ":", df2_clean);
df2_clean = gsub("weibo username", ":", df2_clean);
df2_clean = gsub("weibo uid", ":", df2_clean);
df2_clean = gsub("weibo mid", ":", df2_clean); 
# Use qseg type word segmentation, and save the result to the object seg
seg <- qseg[df2_clean]
# Remove words with character length less than 2
seg <- seg[nchar(seg)>1]
# Calculate word frequency
seg <- table(seg)
# Remove numbers
seg <- seg[!grepl('[0-9]+',names(seg))]
# Remove letters
seg <- seg[!grepl('a-zA-Z',names(seg))]
# View the number of words remaining after processing
length(seg)
# Sort in descending order to extract the 100 words with the most words
seg <- sort(seg, decreasing = TRUE)[1:100]
seg
View(seg)
word_freq <- getWordFreq(string=unlist(seg))
data=data.frame(seg)
data
# draw the word cloud
wordcloud2(data)
#########################################
#
#  Covid xin guan bing du dataset
#
#########################################

# Step1: The number of weibo changes over time
df2 = readLines(paste0(path2,"part1_wuhanvirus.txt"),encoding = "UTF-8") # read the file
head(df2) # View the first few data

df_animals = readLines( paste0(path2_1, "animals_utf8.txt") , encoding ="UTF-8")	# we read the file of animal names
# data processing
df2_clean2 = gsub(':', ' ',df2)
df2_clean2 = gsub("\n", " ", df2_clean2);
df2_clean2 = gsub("\r", " ", df2_clean2);
df2_clean2 = gsub(":", " ", df2_clean2);
df2_clean2 = gsub("weibo text", ":", df2_clean2);
df2_clean2 = gsub("weibo temps", ":", df2_clean2);
df2_clean2 = gsub("weibo username", ":", df2_clean2);
df2_clean2 = gsub("weibo uid", ":", df2_clean2);
df2_clean2 = gsub("weibo mid", ":", df2_clean2); 

df2_final <- data.frame(do.call('rbind', strsplit(as.character(df2_clean2),':',fixed=TRUE)))
df2_final = cbind( as.character(df2_final[, 2]), as.character(df2_final[, 3]) )
colnames(df2_final) = c("text","date") # give names to columns
head(df2_final)


newdata2 <- df2_final[order(df2_final[,'date']),] ;head(newdata2)   #sort on the date field by increasing order
# just take the exact data format I want (year-month-day)
newdata2_time = substring(newdata2[,'date'],2,11)
newdata2[,'date'] = newdata2_time
head(newdata2)

QueryAnimals = paste(df_animals,collapse="|")				# we make a regular expression query

listval = grep(QueryAnimals, newdata2[,'text'], value=T)	#we apply the query to the list of weibos
listval[1]							#we display the first
length(listval)							#number of weibos in the result list

SearchAnimal <- function(x) { a= grep( x, newdata2[,'text'], value=T);c(length(a))}; 
resudoc = lapply(df_animals, SearchAnimal)
CountMessAnimal = unlist(resudoc, use.names=FALSE)

plot(sort(CountMessAnimal),ylab = "#messsages", main="number of weibo over time",  xaxt="n", xlab='time')  #we see that we have an outlier

df_animals[ CountMessAnimal > 1000 ]   #this is the outlier

CleanAnimal <- function(x) { if( nchar(x) < 1)  {return (0)} else {return (1)} };    #we filter the table of animal by the length of string
resudoc = lapply(df_animals, CleanAnimal)
CountMessAnimal = as.logical( unlist(resudoc, use.names=FALSE) )
df_animals_new = df_animals[ CountMessAnimal   ]			#we make a new table

df_animals[ CountMessAnimal > 100 ]		#animals being very frequent 
resudoc = lapply(df_animals_new, SearchAnimal)				#we make a new vector of document for each animal
CountMessAnimal =  unlist(resudoc, use.names=FALSE)			#we transform the list into vector
df_animals_new = df_animals[ CountMessAnimal < 100 & CountMessAnimal > 0 ]	#we select the animal being cited in at least one message but no more than 100 
CountMessAnimal_new =  CountMessAnimal[ CountMessAnimal < 100 & CountMessAnimal > 0 ] # we select the count for these animal 
length(df_animals_new )								#the count of animals

frequent_animal = df_animals_new[ sort(CountMessAnimal_new, decreasing=T, index.return=TRUE)$ix[1:30] ]   # 30 most relatively frequent cited animals
count = sort(CountMessAnimal_new, decreasing=T,index.return=TRUE)$x[1:30]
dff = cbind( as.character(frequent_animal), as.numeric(count) )
colnames(dff) = c("animals","frequency") # give names to columns
dff
# we can see that the most frequent one is "野生动物" with 93 frequency
# Also “蝙蝠” exist with 44 times 
wordcloud(dff)
#########################################
#
#  sentiment analysis - English dataset
#
#########################################

df3 = readLines( paste0(path3_2, "twitter_pola_en.csv") )	#read the file
df3 <- do.call( rbind, strsplit( as.character(df3),";" ))  # using ; as delimiter we split each line and transform into a data frame 
df3_del = df3[c(-1)]			# we delete the first line 
df3_clean = gsub(';',     ' ', df3_del )	# we replace all ; by a blank because we want to use it as delimiter
df3_clean = gsub(',0$',     ';0', df3_clean )	# we replace all ; by a blank because we want to use it as delimiter
df3_clean = gsub(',1$',     ';1', df3_clean )	# we replace all ; by a blank because we want to use it as delimiter
df3_clean = gsub(',-1$',     ';-1', df3_clean )	# we replace all ; by a blank because we want to use it as delimiter
df3_format = sub("([0-9]*)( ,)([0-9]{2}).{1}([0-9]{2}).{8}(.*)", "\\1;2020-\\3-\\4;\\5", df3_clean,perl=TRUE) # we use a regex to extract id;dat;content
df3_final <- do.call( rbind, strsplit( as.character(df3_format),";" ))  # using ; as delimiter we split each line and transform into a data frame 
colnames(df3_final) = c("CONTENT") #we give names to columns
head( df3_final)# I find there are some NA values

df3_final<-na.omit(df3_final) # delete the row that has NA value
pos = scan(paste0(path3_3,"positive-words.txt"),what = 'character',comment.char = ';',encoding = "UTF-8")
neg = scan(paste0(path3_3,"negative-words.txt"),what = 'character',comment.char = ';',encoding = "UTF-8")
class(pos)
length(pos)

class(neg)
length(neg)

pos.words = c(pos,'upgrade')
neg.words = c(neg,'wait')

score.sentiment = function(sentences,pos.words,neg.words)
{
  require(plyr)
  require(stringr)
  
  sentences<-sapply(sentences,function(row) iconv(row,"latin1","ASCII",sub=""))
  
  scores = lapply(sentences,function(sentence,pos.words,neg.words){
    sentence = gsub('[[:punct:]]',"",sentence)
    sentence = gsub('\\d+',"",sentence)
    
    sentence = tolower(sentence)
    
    word.list = str_split(sentence,'\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words,pos.words)
    neg.matches = match(words,neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  },pos.words,neg.words)
  
  scores.df = data.frame(score=as.numeric(scores),
  text = as.character(sentences))
  return(scores.df)
}
result = score.sentiment(df3_final,pos.words,neg.words)
result
q = qplot(result$score)
q = q+theme_bw()
plot(q)

#########################################
#
#  sentiment analysis - Chinese dataset
#
#########################################

Sys.setlocale(locale="Chinese")
data = readLines(paste0(path3_1,"weibo_pola_zh.csv"),encoding = "UTF-8")

df4_clean = gsub(':', ' ',data)
df4_clean = gsub("\n", " ", df4_clean);
df4_clean = gsub("\r", " ", df4_clean);
df4_clean = gsub(":", " ", df4_clean);
df4_clean = gsub("weibo text", ":", df4_clean);
df4_clean = gsub("weibo temps", ":", df4_clean);
df4_clean = gsub("weibo username", ":", df4_clean);
df4_clean = gsub("weibo uid", ":", df4_clean);
df4_clean = gsub("weibo mid", ":", df4_clean);

df<-data.frame(do.call('rbind',strsplit(as.character(df4_clean),':',fixed=TRUE)))
df.pola.zh = cbind(as.character(df[,2]),as.character(df[,3]));head(df)
head(df.pola.zh)

colnames(df.pola.zh) = c("text","date") # give names to the columns

pola.zh = df.pola.zh[,'text']

length(pola.zh)

head(pola.zh,5)
class(pola.zh)

list.pos = scan(paste0(path3_3,"positive-words-zh.txt"),what = 'character',comment.char = ';')
list.neg = scan(paste0(path3_3,"negative-words-zh.txt"),what = 'character',comment.char = ';',encoding = "UTF-8")

list.pos = readLines(paste0(path3_3,"positive-words-zh.txt")) ;
list.neg = readLines(paste0(path3_3,"negative-words-zh.txt")) ;

pos.words = c(list.pos,'高兴')
neg.words = c(list.neg,'死')

engine1 = worker()

pos.words = segment(pos.words,engine1)
neg.words = segment(neg.words,engine1)

score.sentiment.zh = function(sentences,pos.words,neg.words)
{
  require(plyr)
  require(stringr)
  
  scores = lapply(sentences,function(sentence,pos.words,neg.words){
    sentence = gsub('[[:punct:]]',"",sentence)
    sentence = gsub('\\d+',"",sentence)
    
    words = segment(sentence,engine1)

    pos.matches = match(words,pos.words)
    neg.matches = match(words,neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  },pos.words,neg.words)
  
  scores.df = data.frame(score=as.numeric(scores),
                         text = as.character(sentences))
  return(scores.df)
}
  result2 = score.sentiment.zh(pola.zh,pos.words,neg.words) 
  result2
  q2 = qplot(result2$score)
  q2 = q2+theme_bw()
  plot(q2)              
#########################################
#
#  conclusion
#
#########################################
# Dataset1:
# From the first picture we can infer the approximate time of the outbreak, which is almost at the begin in 2020.
# From the second picture we can see that during this time, the most mentioned topic is “肺炎”, which is the topic we want to prove.
# Dataset2:
# From the animal name dataframe, we can find that the most mentioned word is "野生动物”
# We can see from the word cloud that “蝙蝠” is also a high-frequency animal name
# Dataset3：
# From the data set evaluation in English, we can see that people's evaluation of the brand is mainly neutral, followed by the second most people with a positive attitude towards the product.
# From the evaluation of the Chinese data set, we can see that more and more concentrated people hold a positive attitude towards the brand, while a smaller number hold a negative attitude.
# I think this conclusion is reasonable. A large part of the reason is because the brand originated in Japan. Is a well-known makeup brand in Japan.
# Compared with Western countries that use Twitter more frequently, Asians have a closer skin texture and are more suitable for Asian-designed cosmetics belonging to Asian brands.
  