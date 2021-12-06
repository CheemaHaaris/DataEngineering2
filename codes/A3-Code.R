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

keyTable <- read.csv(keyfile, header = T) # *accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
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







##################### Wordcloud ##############################


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


wordcloud(df_frequency6$word,
          df_frequency6$freq)


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

head(df_frequency7)


wordcloud(df_frequency7$word,
          df_frequency7$freq)


######################### Amazon Comprehend ##################################

## Entities

ent_poland <- data.frame()

for (i in 1:2) {
  ent_poland <- detect_entities(poland[i]) 
}

ent_poland


ent_argentina <- data.frame()

for (i in 1:2) {
  ent_argentina <- detect_entities(argentina[i]) 
}

ent_argentina


## Sentiment Analysis

sent_arg <- data.frame()

for (i in 1:2) {
  sent_arg <- detect_sentiment(argentina[i]) 
}

sent_arg %>% 
  kbl(caption = "Sentiment Distribution in the Argentine Articles") %>%
  kable_classic_2(full_width = F, html_font = "Cambria")


sent_poland <- data.frame()

for (i in 1:2) {
  sent_poland <- detect_sentiment(poland[i]) 
}

sent_poland %>%
  kbl(caption = "Sentiment Distribution in the Polish Articles") %>%
  kable_classic_2(full_width = F, html_font = "Cambria")

#################################################################################

# - Sentiment plot

new <- rbind(sent_arg, sent_poland)
new$country <- "Argentina"
new$country[2] <- "Poland"
long <- melt(new, id.vars = c("country", "Sentiment", "Index"))

ggplot(long, aes( x = country, y = value * 100, fill = variable)) +
  geom_bar(stat = "identity") +
  theme_bw() + 
  labs( title = "Comparing Sentiment Distribution", x= "Country", y = "Percentage")



## Polly for reading out sentiment (optional)

if (long[5,5] > long[6,5]) {
vec1 <- synthesize("There is greater neutrality in the Argentine articles. There is also more negativity
                   expressed in the polish articles", voice = "Joey")
play(vec1) }






