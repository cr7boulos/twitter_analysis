# Note: for this script to run properly,
# you must have developer keys/secrets from Twitter
# go to this website to generate them: https://apps.twitter.com/
# (you will need a Twitter account to do so)

# always make sure to install/require all the following packages.
# begin package requirements

install.packages('twitteR')

install.packages('RCurl')

install.packages('tm')

install.packages('wordcloud')



# This tutorial introduced me to the syuzhet package 
# http://juliasilge.com/blog/Joy-to-the-World/

install.packages('syuzhet') # used for sentiment analysis

# end package requirements

require(tm) #require functions just like the library function

require(twitteR) 

require(RCurl)

require(wordcloud)

require(syuzhet)

remove_links <- function(char_vect){
  
  for(c in 1:nchar(char_vect) ){
    if(substr(char_vect, c, c + 3) == 'http' ){
      ve = c
      while(substr(char_vect, ve, ve) != ' ' && ve <= nchar(char_vect)){
        substr(char_vect, ve, ve) = ' '
        ve = ve + 1
      }
    }
  }
  char_vect
}

# fill in your authentication keys from Twitter in
# these variables below

cKey = 'hidden'
cSecret = 'hidden'

access_token = 'hidden'

access_secret = 'hidden'

# uncomment this line to authenticate with twitter
# setup_twitter_oauth(cKey, cSecret, access_token = access_token, access_secret = access_secret)

numberOfTweets = 500; # change this number to whatever you want

# see this guide on building 
# a twitter search query: https://dev.twitter.com/rest/public/search

timeMag = searchTwitter('from:time', n = numberOfTweets, lang='en' )

wsj = searchTwitter('from:wsj', n = numberOfTweets, lang='en' )

nyTimes = searchTwitter('from:nytimes', n = numberOfTweets, lang='en' )

washingtonPost = searchTwitter('from:washingtonpost', n = numberOfTweets, lang='en' )

nyPost = searchTwitter('from:nypost', n = numberOfTweets, lang='en' )

foxNews = searchTwitter('from:foxNews', n = numberOfTweets, lang='en' )

# end lists of tweets

t = character(numberOfTweets)
v = character(numberOfTweets)
u = character(numberOfTweets)
w = character(numberOfTweets)
p = character(numberOfTweets)
f = character(numberOfTweets)

for(i in 1:numberOfTweets){
  w[i] = wsj[[i]]$text
  v[i] = nyTimes[[i]]$text
  u[i] = washingtonPost[[i]]$text
  t[i] = timeMag[[i]]$text
  p[i] = nyPost[[i]]$text
  f[i] = foxNews[[i]]$text
}




for(i in 1:numberOfTweets){
  w[i] = remove_links(w[i])
  v[i] = remove_links(v[i])
  u[i] = remove_links(u[i])
  t[i] = remove_links(t[i])
  p[i] = remove_links(p[i])
  f[i] = remove_links(f[i])
}




# thanks to this Stack Overflow question for removing non-ascii text from tweets
# OP: http://stackoverflow.com/questions/11970891/r-remove-special-characters-from-data-frame
# Asked by: http://stackoverflow.com/users/580110/henk

# Stack Overflow answer: http://stackoverflow.com/a/11971126
# Answers provided by: http://stackoverflow.com/users/602276/andrie
# and: http://stackoverflow.com/users/271616/joshua-ulrich  

# credit: http://stackoverflow.com/users/271616/joshua-ulrich
for(i in 1:numberOfTweets) {
  w[i] = gsub("[^[:alnum:]////' ]", "", w[i])
  v[i] = gsub("[^[:alnum:]////' ]", "", v[i])
  u[i] = gsub("[^[:alnum:]////' ]", "", u[i])
  t[i] = gsub("[^[:alnum:]////' ]", "", t[i])
  p[i] = gsub("[^[:alnum:]////' ]", "", p[i])
  f[i] = gsub("[^[:alnum:]////' ]", "", f[i])
}





srcT = VectorSource(t)
srcU = VectorSource(u)
srcV = VectorSource(v)
srcW = VectorSource(w)
srcP = VectorSource(p)
srcF = VectorSource(f)

corpT = VCorpus(srcT)
corpU = VCorpus(srcU)
corpV = VCorpus(srcV)
corpW = VCorpus(srcW)
corpP = VCorpus(srcP)
corpF = VCorpus(srcF)

corpT <- tm_map(corpT, removeWords, stopwords("en"))
corpU <- tm_map(corpU, removeWords, stopwords("en"))
corpV <- tm_map(corpV, removeWords, stopwords("en"))
corpW <- tm_map(corpW, removeWords, stopwords("en"))
corpP <- tm_map(corpP, removeWords, stopwords("en"))
corpF <- tm_map(corpF, removeWords, stopwords("en"))


corpT <- tm_map(corpT, removePunctuation)
corpT <- tm_map(corpT, stripWhitespace)

corpU <- tm_map(corpU, removePunctuation)
corpU <- tm_map(corpU, stripWhitespace)

corpV <- tm_map(corpV, removePunctuation)
corpV <- tm_map(corpV, stripWhitespace)

corpW <- tm_map(corpW, removePunctuation)
corpW <- tm_map(corpW, stripWhitespace)

corpP <- tm_map(corpP, removePunctuation)
corpP <- tm_map(corpP, stripWhitespace)

corpF <- tm_map(corpF, removePunctuation)
corpF <- tm_map(corpF, stripWhitespace)





tM = as.matrix(TermDocumentMatrix(corpT))
uu = as.matrix(TermDocumentMatrix(corpU))
vv = as.matrix(TermDocumentMatrix(corpV))
ww = as.matrix(TermDocumentMatrix(corpW))
pM = as.matrix(TermDocumentMatrix(corpP))
ff = as.matrix(TermDocumentMatrix(corpF))

term_frequencyT = rowSums(tM)
term_frequencyT = sort(term_frequencyT, decreasing = T)
term_frequencyU = rowSums(uu)
term_frequencyU = sort(term_frequencyU, decreasing = T)
term_frequencyV = rowSums(vv)
term_frequencyV = sort(term_frequencyV, decreasing = T)
term_frequencyW = rowSums(ww)
term_frequencyW = sort(term_frequencyW, decreasing = T)
term_frequencyP = rowSums(pM)
term_frequencyP = sort(term_frequencyP, decreasing = T)
term_frequencyF = rowSums(ff)
term_frequencyF = sort(term_frequencyF, decreasing = T)





# Code for creating the First Week of Barplot Term Distribution
windows()
barplot(term_frequencyW[1:10], main = 'Term distribution of Wall Street Journal tweets', col = (2:11))
windows()
barplot(term_frequencyP[1:10], main = 'Term distribution of New York Post tweets', col = (2:11))
windows()
barplot(term_frequencyF[1:10], main = 'Term distribution of Fox News tweets', col = (2:11))
windows()
barplot(term_frequencyU[1:10], main = 'Term distribution of Washington Post tweets', col = (2:11))
windows()
barplot(term_frequencyT[1:10], main = 'Term distribution of Time Magazine tweets', col = (2:11))
windows()
barplot(term_frequencyV[1:10], main = 'Term distribution of New York Times tweets', col = (2:11))


# Hypothesis testing of proportion here.


# New York Times vs Wall Street Journal
counts = sum(term_frequencyV[1:10][c(which(attr(term_frequencyV[1:10], 'names') == 'donald'), 
                      which(attr(term_frequencyV[1:10], 'names') == 'trumps'),
                      which(attr(term_frequencyV[1:10], 'names') == 'trump'))])
trials = sum(term_frequencyV[1:10])

counts = c(counts,
           sum(term_frequencyW[1:10][c(which(attr(term_frequencyW[1:10], 'names') == 'donald'), 
                                       which(attr(term_frequencyW[1:10], 'names') == 'trumps'),
                                       which(attr(term_frequencyW[1:10], 'names') == 'trump'))]))
trials = c(trials, sum(term_frequencyW[1:10]))

prop.test(counts, trials)

# check for equal proportion of mentions regarding Hillary Clinton
# New York Times vs Wall Street Journal
counts2 = sum(term_frequencyV[1:10][c(which(attr(term_frequencyV[1:10], 'names') == 'hillary'), 
                                     which(attr(term_frequencyV[1:10], 'names') == 'clinton'))])
trial2 = sum(term_frequencyV[1:10])

counts2 = c(counts2,
           sum(term_frequencyW[1:10][c(which(attr(term_frequencyW[1:10], 'names') == 'hillary'), 
                                       which(attr(term_frequencyW[1:10], 'names') == 'clinton'))]))
trial2 = c(trial2, sum(term_frequencyW[1:10]))

prop.test(counts2, trial2)

# now I need to subset the data and grab all tweets that mention Donald Trump.
# do this for each of the six TDMs. Legend: tM = TimeMag
# uu = Washington Post; vv = New York Times; ww = Wall Street Journal
# pM = New York Post; ff = Fox News; 

# find the row that corresponds to terms mentioning 'donald', 'trump', 'trumps'



# grab the indices of the tweets that mention Donald Trump

# d1, d2, ..., d6 below are vectors containing all the tweets that mention 
# Donald Trump from each of the six selected news organizations. 

# Time Magazine
timeMentions = numeric(0)
temp = c('donald', 'trump', 'trumps')
for( i in 1:3){
  timeMentions = c(timeMentions, unique(which(tM[which(rownames(tM) == temp[i]) , ] > 0)))
}

timeMentions = unique(timeMentions)
d1 = t[timeMentions]


# Wall Street Journal
wsjMentions = numeric(0)

for( i in 1:3){
  wsjMentions = c(wsjMentions, unique(which(ww[which(rownames(ww) == temp[i]) , ] > 0)))
}

wsjMentions = unique(wsjMentions)
d2 = w[wsjMentions]

# New York Times
nytMentions = numeric(0)

for( i in 1:3){
  nytMentions = c(nytMentions, unique(which(vv[which(rownames(vv) == temp[i]) , ] > 0)))
}
nytMentions = unique(nytMentions)
d3 = u[nytMentions]

# New York Post
nypMentions = numeric(0)

for( i in 1:3){
  nypMentions = c(nypMentions, unique(which(pM[which(rownames(pM) == temp[i]) , ] > 0)))
}
nypMentions = unique(nypMentions)
d4 = p[nypMentions]

# Fox News 
foxMentions = numeric(0)

for( i in 1:3){
  foxMentions = c(foxMentions, unique(which(ff[which(rownames(ff) == temp[i]) , ] > 0)))
}

foxMentions = unique(foxMentions)
d5 = f[foxMentions]

# Washington Post

WashPostMentions = numeric(0)

for( i in 1:3){
  WashPostMentions = c(WashPostMentions, unique(which(uu[which(rownames(uu) == temp[i]) , ] > 0)))
}

WashPostMentions = unique(WashPostMentions)
d6 = u[WashPostMentions]



# TODO: create a histogram of the number of favorites for tweets 
# mentioning Donald Trump from a given news site.





srcT2 = VectorSource(d1)
srcU2 = VectorSource(d6)
srcV2 = VectorSource(d3)
srcW2 = VectorSource(d2)
srcF2 = VectorSource(d5)
srcP2 = VectorSource(d4)

corpT2 = VCorpus(srcT2)
corpU2 = VCorpus(srcU2)
corpV2 = VCorpus(srcV2)
corpW2 = VCorpus(srcW2)
corpF2 = VCorpus(srcF2)
corpP2 = VCorpus(srcP2)

# I need to add these words to the list of stopwords: 'donald', 'trump' , 'trumps'
corpT2 <- tm_map(corpT2, removeWords, c(stopwords("en"), 'donald', 'trump', 'trumps'))
corpU2 <- tm_map(corpU2, removeWords, c(stopwords("en"), 'donald', 'trump', 'trumps'))
corpV2 <- tm_map(corpV2, removeWords, c(stopwords("en"), temp))
corpW2 <- tm_map(corpW2, removeWords, c(stopwords("en"), temp))
corpP2 <- tm_map(corpP2, removeWords, c(stopwords("en"), temp))
corpF2 <- tm_map(corpF2, removeWords, c(stopwords("en"), temp))

corpT2 <- tm_map(corpT2, removePunctuation)
corpT2 <- tm_map(corpT2, stripWhitespace)
corpU2 <- tm_map(corpU2, removePunctuation)
corpU2 <- tm_map(corpU2, stripWhitespace)
corpV2 <- tm_map(corpV2, removePunctuation)
corpV2 <- tm_map(corpV2, stripWhitespace)
corpW2 <- tm_map(corpW2, removePunctuation)
corpW2 <- tm_map(corpW2, stripWhitespace)



corpP2 <- tm_map(corpP2, removePunctuation)
corpP2 <- tm_map(corpP2, stripWhitespace)

corpF2 <- tm_map(corpF2, removePunctuation)
corpF2 <- tm_map(corpF2, stripWhitespace)


tM2 = as.matrix(TermDocumentMatrix(corpT2))
uu2 = as.matrix(TermDocumentMatrix(corpU2))
vv2 = as.matrix(TermDocumentMatrix(corpV2))
ww2 = as.matrix(TermDocumentMatrix(corpW2))
pM2 = as.matrix(TermDocumentMatrix(corpP2))
ff2 = as.matrix(TermDocumentMatrix(corpF2))

term_frequencyT2 = rowSums(tM2)
term_frequencyT2 = sort(term_frequencyT2, decreasing = T)
term_frequencyU2 = rowSums(uu2)
term_frequencyU2 = sort(term_frequencyU2, decreasing = T)
term_frequencyV2 = rowSums(vv2)
term_frequencyV2 = sort(term_frequencyV2, decreasing = T)
term_frequencyW2 = rowSums(ww2)
term_frequencyW2 = sort(term_frequencyW2, decreasing = T)
term_frequencyP2 = rowSums(pM2)
term_frequencyP2 = sort(term_frequencyP2, decreasing = T)
term_frequencyF2 = rowSums(ff2)
term_frequencyF2 = sort(term_frequencyF2, decreasing = T)

# Code for creating the First Week of Barplot Term Distribution
windows()
barplot(term_frequencyW2[1:10], main = 'Term distribution of Wall Street Journal tweets\nthat mention Donald Trump'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyP2[1:10], main = 'Term distribution of New York Post tweets\nthat mention Donald Trump'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyF2[1:10], main = 'Term distribution of Fox News tweets\nthat mention Donald Trump'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyU2[1:10], main = 'Term distribution of Washington Post tweets\nthat mention Donald Trump'
        , col = (2:11), ylim = c(0,100))
windows()
barplot(term_frequencyT2[1:10], main = 'Term distribution of Time Magazine tweets\nthat mention Donald Trump'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyV2[1:10], main = 'Term distribution of New York Times tweets\nthat mention Donald Trump'
        , col = (2:11), ylim = c(0,50))

# the folling lines of code generate the word clouds
windows()
word_freqT2 = data.frame(term = names(term_frequencyT2), num = term_frequencyT2)
wordcloud(word_freqT2$term, word_freqT2$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqU2 = data.frame(term = names(term_frequencyU2), num = term_frequencyU2)
wordcloud(word_freqU2$term, word_freqU2$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqV2 = data.frame(term = names(term_frequencyV2), num = term_frequencyV2)
wordcloud(word_freqV2$term, word_freqV2$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqW2 = data.frame(term = names(term_frequencyW2), num = term_frequencyW2)
wordcloud(word_freqW2$term, word_freqW2$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqP2 = data.frame(term = names(term_frequencyP2), num = term_frequencyP2)
wordcloud(word_freqP2$term, word_freqP2$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqF2 = data.frame(term = names(term_frequencyF2), num = term_frequencyF2)
wordcloud(word_freqF2$term, word_freqF2$num, max.words = 100, min.freq = 3, colors = 'red')

#end word cloud code

# note: the sentiment analysis algorithm incorrectly gives
# the term 'trump' the descriptor 'suprise' thus skewing the results.
# I am suppressing the suprise column in my plots 
windows()
barplot(colSums(get_nrc_sentiment(d1))[-7], main = 'Sentiment Analysis of Time Magazine\ntweets that mention Donald Trump'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d2))[-7], main = 'Sentiment Analysis of Wall Street Journal\ntweets that mention Donald Trump'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d3))[-7], main = 'Sentiment Analysis of New York Times\ntweets that mention Donald Trump'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d4))[-7], main = 'Sentiment Analysis of New York Post\ntweets that mention Donald Trump'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d5))[-7], main = 'Sentiment Analysis of Fox News\ntweets that mention Donald Trump'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d6))[-7], main = 'Sentiment Analysis of Washington Post\ntweets that mention Donald Trump'
        , col = c(5:13), ylim = c(0,50))

# this next section of code analyzes tweets mentioning Hilary Clintion

# find the row that corresponds to terms mentioning 'hilary', 'clinton'
tmagHC = c(which(rownames(tM) == 'hilary'), which(rownames(tM) == 'clinton'))
wallStrJourHC = c(which(rownames(ww) == 'hilary'), which(rownames(ww) == 'clinton'), which(rownames(ww) == 'clintons'))
NYTHC = c(which(rownames(vv) == 'hilary'), which(rownames(vv) == 'clinton'), which(rownames(vv) == 'clintons'))
NYpostHC = c(which(rownames(pM) == 'hilary'), which(rownames(pM) == 'clinton'), which(rownames(pM) == 'clintons'))
fxNewsHC = c(which(rownames(ff) == 'hilary'), which(rownames(ff) == 'clinton'), which(rownames(ff) == 'clintons'))
washPostHC = c(which(rownames(uu) == 'hilary'), which(rownames(uu) == 'clinton'), which(rownames(uu) == 'clintons'))

# grab the indices of the tweets that mention Hilary Clinton 


# d1HC, d2HC, ..., d6HC below are vectors containing all the tweets that mention 
# Hillary Clinton from each of the six selected news organizations.


# Time Magazine
timeMentionsHC = numeric(0)
tempHC = c('hilary', 'clinton', 'clintons')
for( i in 1:3){
  timeMentionsHC = c(timeMentionsHC, unique(which(tM[which(rownames(tM) == tempHC[i]) , ] > 0)))
}

timeMentionsHC = unique(timeMentionsHC)
d1HC = t[timeMentionsHC]
# Wall Street Journal


wsjMentionsHC = numeric(0)

for( i in 1:3){
  wsjMentionsHC = c(wsjMentionsHC, unique(which(ww[which(rownames(ww) == tempHC[i]) , ] > 0)))
}

wsjMentionsHC = unique(wsjMentionsHC)
d2HC = w[wsjMentionsHC]

# New York Times
nytMentionsHC = numeric(0)

for( i in 1:3){
  nytMentionsHC = c(nytMentionsHC, unique(which(vv[which(rownames(vv) == tempHC[i]) , ] > 0)))
}
nytMentionsHC = unique(nytMentionsHC)
d3HC = u[nytMentionsHC]

# New York Post
nypMentionsHC = numeric(0)

for( i in 1:3){
  nypMentionsHC = c(nypMentionsHC, unique(which(pM[which(rownames(pM) == tempHC[i]) , ] > 0)))
}
nypMentionsHC = unique(nypMentionsHC)
d4HC = p[nypMentionsHC]

# Fox News 
foxMentionsHC = numeric(0)

for( i in 1:3){
  foxMentionsHC = c(foxMentionsHC, unique(which(ff[which(rownames(ff) == tempHC[i]) , ] > 0)))
}

foxMentionsHC = unique(foxMentionsHC)
d5HC = f[foxMentionsHC]

# Washington Post

WashPostMentionsHC = numeric(0)

for( i in 1:3){
  WashPostMentionsHC = c(WashPostMentionsHC, unique(which(uu[which(rownames(uu) == tempHC[i]) , ] > 0)))
}

WashPostMentionsHC = unique(WashPostMentionsHC)
d6HC = u[WashPostMentionsHC]



corpT2HC = VCorpus(VectorSource(d1HC))
corpU2HC = VCorpus(VectorSource(d6HC))
corpV2HC = VCorpus(VectorSource(d3HC))
corpW2HC = VCorpus(VectorSource(d2HC))
corpF2HC = VCorpus(VectorSource(d5HC))
corpP2HC = VCorpus(VectorSource(d4HC))

# I need to add these words to the list of stopwords: 'hilary', 'clinton' , 'clintons'
corpT2HC <- tm_map(corpT2HC, removeWords, stopwords("en"))
corpU2HC <- tm_map(corpU2HC, removeWords, stopwords("en"))
corpV2HC <- tm_map(corpV2HC, removeWords, stopwords("en"))
corpW2HC <- tm_map(corpW2HC, removeWords, stopwords("en"))
corpP2HC <- tm_map(corpP2HC, removeWords, stopwords("en"))
corpF2HC <- tm_map(corpF2HC, removeWords, stopwords("en"))

corpT2HC <- tm_map(corpT2HC, removePunctuation)
corpT2HC <- tm_map(corpT2HC, stripWhitespace)
corpU2HC <- tm_map(corpU2HC, removePunctuation)
corpU2HC <- tm_map(corpU2HC, stripWhitespace)
corpV2HC <- tm_map(corpV2HC, removePunctuation)
corpV2HC <- tm_map(corpV2HC, stripWhitespace)
corpW2HC <- tm_map(corpW2HC, removePunctuation)
corpW2HC <- tm_map(corpW2HC, stripWhitespace)



corpP2HC <- tm_map(corpP2HC, removePunctuation)
corpP2HC <- tm_map(corpP2HC, stripWhitespace)

corpF2HC <- tm_map(corpF2HC, removePunctuation)
corpF2HC <- tm_map(corpF2HC, stripWhitespace)


tM2HC = as.matrix(TermDocumentMatrix(corpT2HC))
uu2HC = as.matrix(TermDocumentMatrix(corpU2HC))
vv2HC = as.matrix(TermDocumentMatrix(corpV2HC))
ww2HC = as.matrix(TermDocumentMatrix(corpW2HC))
pM2HC = as.matrix(TermDocumentMatrix(corpP2HC))
ff2HC = as.matrix(TermDocumentMatrix(corpF2HC))

term_frequencyT2HC = rowSums(tM2HC)
term_frequencyT2HC = sort(term_frequencyT2, decreasing = T)
term_frequencyU2HC = rowSums(uu2HC)
term_frequencyU2HC = sort(term_frequencyU2, decreasing = T)
term_frequencyV2HC = rowSums(vv2HC)
term_frequencyV2HC = sort(term_frequencyV2, decreasing = T)
term_frequencyW2HC = rowSums(ww2HC)
term_frequencyW2HC = sort(term_frequencyW2, decreasing = T)
term_frequencyP2HC = rowSums(pM2HC)
term_frequencyP2HC = sort(term_frequencyP2, decreasing = T)
term_frequencyF2HC = rowSums(ff2HC)
term_frequencyF2HC = sort(term_frequencyF2, decreasing = T)

windows()
barplot(term_frequencyW2HC[1:10], main = 'Term distribution of Wall Street Journal tweets\nthat mention Hilary Clinton'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyP2HC[1:10], main = 'Term distribution of New York Post tweets\nthat mention Hilary Clinton'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyF2HC[1:10], main = 'Term distribution of Fox News tweets\n that mention Hilary Clinton'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyU2HC[1:10], main = 'Term distribution of Washington Post tweets\nthat mention Hilary Clinton'
        , col = (2:11), ylim = c(0,100))
windows()
barplot(term_frequencyT2HC[1:10], main = 'Term distribution of Time Magazine tweets\nthat mention Hilary Clintion'
        , col = (2:11), ylim = c(0,50))
windows()
barplot(term_frequencyV2HC[1:10], main = 'Term distribution of New York Times tweets\nthat mention Hilary Clinton'
        , col = (2:11), ylim = c(0,50))

# the folling lines of code generate the word clouds
windows()
word_freqT2HC = data.frame(term = names(term_frequencyT2HC), num = term_frequencyT2HC)
wordcloud(word_freqT2HC$term, word_freqT2HC$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqU2HC = data.frame(term = names(term_frequencyU2HC), num = term_frequencyU2HC)
wordcloud(word_freqU2HC$term, word_freqU2HC$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqV2HC = data.frame(term = names(term_frequencyV2HC), num = term_frequencyV2HC)
wordcloud(word_freqV2HC$term, word_freqV2HC$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqW2HC = data.frame(term = names(term_frequencyW2HC), num = term_frequencyW2HC)
wordcloud(word_freqW2HC$term, word_freqW2HC$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqP2HC = data.frame(term = names(term_frequencyP2HC), num = term_frequencyP2HC)
wordcloud(word_freqP2HC$term, word_freqP2HC$num, max.words = 100, min.freq = 3, colors = 'red')

windows()
word_freqF2HC = data.frame(term = names(term_frequencyF2HC), num = term_frequencyF2HC)
wordcloud(word_freqF2HC$term, word_freqF2HC$num, max.words = 100, min.freq = 3, colors = 'red')

#end word cloud code

# run some sentiment analysis on tweets mentioning Hillary Clinton

# note: the sentiment analysis algorithm incorrectly gives
# the term 'trump' the descriptor 'suprise' thus skewing the results.
# I am suppressing the suprise column in my plots 
windows()
barplot(colSums(get_nrc_sentiment(d1HC))[-7], main = 'Sentiment Analysis of Time Magazine\ntweets that mention Hillary Clinton'
        , col = c(5:13), ylim = c(0,30))
windows()
barplot(colSums(get_nrc_sentiment(d2HC))[-7], main = 'Sentiment Analysis of Wall Street Journal\ntweets that mention Hillary Clinton'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d3HC))[-7], main = 'Sentiment Analysis of New York Times\ntweets that mention Hillary Clinton'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d4HC))[-7], main = 'Sentiment Analysis of New York Post\ntweets that mention Hillary Clinton'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d5HC))[-7], main = 'Sentiment Analysis of Fox News\ntweets that mention Hillary Clinton'
        , col = c(5:13), ylim = c(0,50))
windows()
barplot(colSums(get_nrc_sentiment(d6HC))[-7], main = 'Sentiment Analysis of Washington Post\ntweets that mention Hillary Clinton'
        , col = c(5:13), ylim = c(0,50))
