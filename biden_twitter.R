#library(twitteR)
library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(rtweet)
library(googledrive)
library(readr)
library(lubridate)
library(patchwork)


# #https://randomjohn.github.io/r-twitter/
# setwd("C:/Users/Marc Eskew/Documents/")

consumer_key <- "PfRLa8j1adkL2kU9vdAkYvsIh"
consumer_secret <- "MMfoGeOJftZvpshy9yfgJSLj9lpgX9hvO4SmMwz9b9BlhhR8mV"
access_token <- "826305709209890816-Cu8PVJ9aGgIv8ZIq5CZQ6P7kxzCG8hh"
access_secret <- "tv5LjBd9DEuGDuoF2UWsG8hZi4iQThcGDo93jBtUfPwhM" 



token <- create_token(app = "marc_app_testing",
                      consumer_key,
                      consumer_secret,
                      access_token,
                      access_secret)


biden <- get_timeline(user = c("@JoeBiden"),n = 3200)
kamala <- get_timeline(user = c("@KamalaHarris"),n = 3200)
trump <- get_timeline(user = "@realDonaldTrump", n=3200, include_rts = FALSE)

`%out%` <- function(a,b) ! a %in% b

biden_unnest <- biden %>%
  mutate(text = plain_tweets(text)) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!word %in% c(stop_words$word, "jura","t.co")) %>%
  group_by(word) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

wordsearch <- "violence"

biden_harris <- bind_rows(biden,kamala)

textfilt <- biden_harris %>%
  mutate(text = str_to_lower(text)) %>%
  filter(str_detect(text, wordsearch)) %>%
  filter(str_detect(text,"gun violence") == F) %>%
  mutate(week = floor_date(created_at, "day")) %>%
  group_by(week) 

textfilt <- trump %>%
  mutate(text = str_to_lower(text)) %>%
  filter(str_detect(text, wordsearch)) %>%
  #filter(str_detect(text,"gun violence") == F) %>%
  mutate(week = floor_date(created_at, "week")) %>%
  group_by(week) 

textsum <- textfilt %>%
  summarise(num = n()) %>%
  filter(week >= as.POSIXct("2020-06-01"))

p2 <- ggplot(textsum , aes(x = week, y = num)) + 
  geom_bar(stat = "identity",fill = "red") +
  scale_x_datetime(breaks = scales::breaks_width("1 month"), labels = scales::date_format(format = "%b-%Y")) +
  labs(title = paste0("Joe Biden & Kamala Harris Tweets Mentioning '",wordsearch,"'"),
       subtitle = "Excluding the phrase 'gun violence'",
       y = "Number of Mentions in Week",
       x = "Week")  +
  theme_minimal() +
  theme(
         legend.position = "none",
         legend.spacing.x = unit(.95, 'cm'),
         legend.background = element_rect(fill = "#e8e1e5"),
         plot.background = element_rect(fill = "#fff7fb"),
         text  = element_text(family = "Bahnschrift", size = 14),
         plot.title = element_text(size = 20,hjust = .5),
         plot.subtitle = element_text(size = 16,hjust = .5),
         plot.title.position = "plot",
         plot.caption = element_text(color = "grey50",size = 8)
         
       ) 


p2
p1+ p2
