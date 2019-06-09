library("twitteR")
library("tm")


#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'Byar5NpnRUjZtvrXGJjOLt0iy'
consumer_secret <- 'vpTMV2AYRq4KFE3wxHs83bpfrf5D37y5vzMDEfrWHY6tzYDoeF'
access_token <- '152952148-3LPmRtv9QCdxpfZxCQWKMoApPy9suRAJLj5wBQC9'
access_secret <- 'sv7v59TdPKRHrd7wRVv3cXjhkpypAN1jC0MI1aKoFNd7S'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
JustinBieber <- twitteR::searchTwitter('#JUSTINBIEBER + #MUSIC', n = 1000, since = '2006-01-01', retryOnRateLimit = 1e3)
J = twitteR::twListToDF(JustinBieber)

Marshmello <- twitteR::searchTwitter('#Marshmello + #MUSIC', n = 1000, since = '2006-01-01', retryOnRateLimit = 1e3)
m = twitteR::twListToDF(Marshmello)

AlanWalker <- twitteR::searchTwitter('#AlanWalker + #MUSIC', n = 1000, since = '2006-01-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(AlanWalker)

library(dplyr)
library(stringr)
library(tidytext)

data(stop_words)
custom_stop_words <- bind_rows(data_frame(word = c("https","t.co","rt"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

frequencies_tokens_nostop_J <- J %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) 

frequencies_tokens_nostop_J%>%count(word, sort=TRUE)

frequencies_tokens_nostop_m <- m %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) 
frequencies_tokens_nostop_m%>%count(word, sort=TRUE)

frequencies_tokens_nostop_a <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) 
frequencies_tokens_nostop_a%>%count(word, sort=TRUE)



library(tidyr)
frequency <- bind_rows(mutate(frequencies_tokens_nostop_J, country="JustinBieber"),
                       mutate(frequencies_tokens_nostop_m, country="Marshmello"), 
                       mutate(frequencies_tokens_nostop_a, country="AlanWalker")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(country, word) %>%
  group_by(country) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(country, proportion) %>% 
  gather(country, proportion, `JustinBieber`:`Marshmello`)

#plot the correlograms:
library(scales)
library(ggplot2)

ggplot(frequency, aes(x=proportion, y=`AlanWalker`, 
                      color = abs(`AlanWalker`-proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~country, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "AlanWalker", x=NULL)


cor.test(data=frequency[frequency$country == "JustinBieber",],
         ~proportion + `AlanWalker`)

cor.test(data=frequency[frequency$country == "Marshmello",],
         ~proportion + `AlanWalker`)





#################################
####bigram application:#########
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)


aw_bigrams <-a %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%count(bigram, sort = TRUE)


library(tidyr)
bigrams_separated <- aw_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigram_counts <- bigrams_filtered %>%count(word1, word2, sort = TRUE)
bigram_counts


