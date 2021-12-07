################## Assignment 3 - DE 2 - Cloud Computing #######################

library(rvest)
library(data.table)
library(aws.translate)
#install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"), INSTALL_opts = "--no-multiarch")
library("aws.comprehend")
library(ggplot2)
library(kableExtra)
library(aws.polly)
library(tuneR)
library(tm)
library(wordcloud)
library(reshape)
library(RColorBrewer)

################# Scraping #####################

## Argentinian Newspapers

url2 <- 'https://www.batimes.com.ar/news/sports/messis-enduring-brilliance-rewarded-with-another-ballon-dor.phtml'

u <- read_html(url2)

arg_text <- u %>% html_nodes('#news-body p') %>% html_text()

text2 <- NULL
for ( i in arg_text ){
  text2 <- paste( text2, i )
}


url5 <- 'https://www.batimes.com.ar/news/sports/messi-wins-ballon-dor-for-seventh-time-as-putellas-crowned-womens-winner.phtml'
x <- read_html(url5)

arg_text2 <- x %>% html_nodes('#news-body p') %>% html_text()

text5 <- NULL
for ( i in arg_text2 ){
  text5 <- paste( text5, i )
}

## Polish newspapers

url3 <- 'https://sport.fakt.pl/zlota-pilka-argentynski-portal-krytykuje-france-football-przepraszam-robert/fdks8h7'

v <- read_html(url3)

pol_text <- v %>% html_nodes('.article-p') %>% html_text()

text3 <- NULL
for ( i in pol_text) {
  text3 <- paste(text3, i)
}

url4 <- 'https://sport.fakt.pl/pilka-nozna/zlota-pilka-niemiecka-prasa-oburzona-wynikami-france-football/f72vf1p'

w <- read_html(url4)

pol_text2 <- w %>% html_nodes('.article-p') %>% html_text()

text4 <- NULL
for ( i in pol_text2) {
  text4 <- paste(text4, i)
}



################# AWS Services ##########################

## Connecting to AWS ##


keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
} 

keyTable <- read.csv(keyfile, header = T) 
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

##################### AWS Translate ##############################

detect_language(text2)
detect_language(text5)

## Translating the polish newspaper articles

detect_language(text3)
text3 <- translate(text3, from = "pl" , to = "en")

detect_language(text4)
text4 <- translate(text4, from = "pl" , to = "en")



# Rowbinding the articles from argentina and  poland

argentina <- rbind(text2, text5)


poland <- rbind(text3, text4)

detect_sentiment(text2)
detect_sentiment(text5)

##################### Wordclouds & Frequency Plots ##############################

#### Argentina

trial6 <- VCorpus(VectorSource(argentina))
trial6 <- tm_map(trial6, content_transformer(tolower))
trial6 <- tm_map(trial6, removePunctuation)
trial6 <- tm_map(trial6, removeWords, stopwords())
inspect(trial6)


tdm6 <- DocumentTermMatrix(trial6)
inspect(tdm6)

word_frequency6 <- sort(colSums(as.matrix(tdm6)),
                       decreasing=TRUE)
df_frequency6 <- data.frame(word = names(word_frequency6),
                          freq=word_frequency6)

head(df_frequency6)

terms2 <- df_frequency6 %>% head(sort(freq,decreasing=TRUE), n = 10)

ggplot(terms2, aes( x = reorder(word, freq ) , y = freq)) +
  geom_bar(stat = "identity", aes( fill = freq)) +
  coord_flip() +
  labs(title = "Term frequency in the Argentine articles", x = "Words", y = "Frequency") +
  theme_bw()+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)


wordcloud(words = df_frequency6$word, freq =  df_frequency6$freq, min.freq = 1,
          max.words=42, random.order=FALSE, rot.per=0.25,
          colors=brewer.pal(8, "Dark2"))

#### Poland

trial7 <- VCorpus(VectorSource(poland))
trial7 <- tm_map(trial7, content_transformer(tolower))
trial7 <- tm_map(trial7, removePunctuation)
trial7 <- tm_map(trial7, removeWords, stopwords())
inspect(trial7)


tdm7 <- DocumentTermMatrix(trial7)
inspect(tdm7)

word_frequency7 <- sort(colSums(as.matrix(tdm7)),
                        decreasing=TRUE)
df_frequency7 <- data.frame(word = names(word_frequency7),
                            freq=word_frequency7)

terms1 <- df_frequency7 %>% head(sort(freq,decreasing=TRUE), n = 10)



ggplot(terms1, aes( x = reorder(word, freq ) , y = freq, fill = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Term frequency in the Polish articles", x = "Words", y = "Frequency") +
  theme_bw()

wordcloud(words = df_frequency7$word, freq =  df_frequency7$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.25,
          colors=brewer.pal(8, "Dark2"))

######################### Amazon Comprehend ##################################

## Entities

ent_poland <- data.frame()

for (i in 1:2) {
  ent_poland <- detect_entities(poland[i]) 
}

ent_poland


person <- subset(ent_poland, ent_poland$Type == 'PERSON')
  
ggplot(person, aes(x = Text, y = Score)) +
  geom_point(colour = "red", size = 3) +
  theme_bw() +
  labs( y = 'Confidence')

ent_argentina <- data.frame()

for (i in 1:2) {
  ent_argentina <- detect_entities(argentina[i]) 
}

ent_argentina


## Sentiment Analysis

sent_arg <- data.frame()

for (i in 1:2) {
  sent_arg[i,1:6] <- detect_sentiment(argentina[i]) 
}

sent_arg[1:2, 3:6] <- round(sent_arg[1:2, 3:6] * 100, 2)

df2 <- data.frame(Index = 0, Sentiment = 'Neutral', Mixed = round(mean(sent_arg$Mixed),2),
                  Negative = round(mean(sent_arg$Negative),2),
                  Neutral = round(mean(sent_arg$Neutral),2),
                  Positive = round(mean(sent_arg$Positive),2) )


df2 [1,2:6] %>% 
  kbl(caption = "Sentiment Distribution in the Argentine Articles") %>%
  kable_classic_2(full_width = F, html_font = "Cambria")


sent_poland <- data.frame()

for (i in 1:2) {
  sent_poland[i,1:6] <- detect_sentiment(poland[i]) 
}


sent_poland[1:2, 3:6] <- round(sent_poland[1:2, 3:6] * 100, 2)


df1 <- data.frame(Index = 0, Sentiment = 'Neutral', Mixed = round(mean(sent_poland$Mixed),2),
                                                  Negative = round(mean(sent_poland$Negative),2),
                                                  Neutral = round(mean(sent_poland$Neutral),2),
                                                  Positive = round(mean(sent_poland$Positive),2) )


df1[1,2:6] %>%
  kbl(caption = "Sentiment Distribution in the Polish Articles") %>%
  kable_classic_2(full_width = F, html_font = "Cambria")

#################################################################################

# - Sentiment plot

new <- rbind(df2, df1)
new$country <- "Argentina"
new$country[2] <- "Poland"
long <- melt(new, id.vars = c("country", "Sentiment", "Index"))

ggplot(long, aes( x = country, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  theme_bw() + 
  labs( title = "Comparing Sentiment Distribution", x= "Country", y = "Percentage")



## Polly for reading out sentiment (optional)

if (long[5,5] > long[6,5]) {
vec1 <- synthesize("There is greater neutrality in the Argentine articles. There is also more negativity
                   expressed in the polish articles", voice = "Joey")
play(vec1) }