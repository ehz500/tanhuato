bots = scan("~/Documents/tanhuato/data/tweets/clean_bot_tweets.txt", what="", sep = "\n")
humans = scan("~/Documents/tanhuato/data/tweets/clean_human_tweets.txt", what="", sep = "\n")


tweets = data.frame(text = c(bots, humans))
tweets$author = "human"
tweets$author[c(1:length(bots))] = "bot"



library(tidytext)
library(stringr)
library(dplyr)



spanish.stop.words = scan("~/Documents/tanhuato/data/spanish_stop_words.txt", what="", sep = "\n")


reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% c(stop_words$word, spanish.stop.words),
         str_detect(word, "[a-z]"))


frequency <- tidy_tweets %>% 
  group_by(author) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(author) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

library(tidyr)

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, author) %>%
  filter(sum(n) >= 5) %>%
  spread(author, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(human / bot)) %>%
  arrange(desc(logratio))

library(plotly)


word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(30, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (Humans/Bots)") +
  scale_fill_discrete(name = "", labels = c("Humans", "Bots")) -> p

## show words that have roughly same likelihood from originating from both
word_ratios %>% 
  arrange(abs(logratio)) 


##
# investigate:
# #tlatlaya
# #felizsbado

