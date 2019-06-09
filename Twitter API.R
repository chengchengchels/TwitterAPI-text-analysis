###############################################################
######Querying Twitter for shares, like Trump tweets###########
###############################################################
#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")

library("twitteR")
library("tm")


consumer_key <- 'Byar5NpnRUjZtvrXGJjOLt0iy'
consumer_secret <- 'vpTMV2AYRq4KFE3wxHs83bpfrf5D37y5vzMDEfrWHY6tzYDoeF'
access_token <- '152952148-3LPmRtv9QCdxpfZxCQWKMoApPy9suRAJLj5wBQC9'
access_secret <- 'sv7v59TdPKRHrd7wRVv3cXjhkpypAN1jC0MI1aKoFNd7S'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Blockchain', n = 1000, since = '2017-01-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)

EU <- twitteR::searchTwitter('#EU + #Blockchain', n = 1000, since = '2017-01-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)

Asia <- twitteR::searchTwitter('#Asia + #Blockchain', n = 1000, since = '2017-01-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(Asia)



library(dplyr)
library(stringr)
library(tidytext)

data(stop_words)

frequencies_tokens_nostop_d <- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

frequencies_tokens_nostop_d%>%count(word, sort=TRUE)


frequencies_tokens_nostop_e <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 
frequencies_tokens_nostop_e%>%count(word, sort=TRUE)

frequencies_tokens_nostop_a <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 
frequencies_tokens_nostop_a%>%count(word, sort=TRUE)



library(tidyr)
frequency <- bind_rows(mutate(frequencies_tokens_nostop_d, country="USA"),
                       mutate(frequencies_tokens_nostop_e, country="EU"), 
                       mutate(frequencies_tokens_nostop_a, country="Asia")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(country, word) %>%
  group_by(country) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(country, proportion) %>% 
  gather(country, proportion, `USA`:`EU`)



library(scales)
library(ggplot2)
ggplot(frequency, aes(x=proportion, y=`Asia`, 
                      color = abs(`Asia`-proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~country, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Asia", x=NULL)


cor.test(data=frequency[frequency$country == "EU",],
         ~proportion + `Asia`)

cor.test(data=frequency[frequency$country == "USA",],
         ~proportion + `Asia`)

