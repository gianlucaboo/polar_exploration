library(rvest); library(xml2); library(stringr); 
library(tidyverse); library(tidyselect); library(tidytext)


amundsenJournal_html <- read_html("https://www.gutenberg.org/files/4229/4229-h/4229-h.htm")

amundsenJournal_df <- amundsenJournal_html %>% 
  html_nodes("h1, h2, h3, h4, p") %>% html_text(trim = TRUE) %>% 
  as.data.frame() %>% rename(text=1) %>% 
  mutate(text=str_replace_all(text, "\r\n", " "),
         text=str_replace_all(text, "\\\"", ""),
         text=text %>% tolower(),
         text=ifelse(text=="chapter v", "chapter v:", text))

amundsenJournal_df_t <- amundsenJournal_df %>% 
  #extract chapters end filter chapters five to fourteen
  mutate(chapter=str_extract_all(text, paste("chapter ", seq(1, 16, 1) %>% as.roman() %>% tolower(), ":", sep="", collapse="|"), simplify = TRUE) %>% 
           as.character() %>% na_if("") %>% str_remove_all(":"), .before=text) %>% 
  fill(chapter, .direction="down") %>% 
  mutate(paragraph=row_number(), .after=chapter) %>% 
  filter(str_detect(chapter, paste("chapter", seq(5, 14, 1) %>% as.roman() %>% tolower(), collapse="|")) & str_length(text)>50) %>% 
  #extract years
  mutate(year=str_extract_all(text, seq(1872, 1912, 1) %>% paste(collapse="|"), simplify = F), 
         year=ifelse(year=="character(0)", NA_character_, year),
  #extract month and year
  month=str_extract_all(text, month.name %>% tolower() %>% paste(collapse="|"), simplify = F),
  month=ifelse(month=="character(0)", NA_character_, month),
  month_day=str_extract_all(text, paste0(month.name %>% tolower(), "\\s\\d{1,2}", collapse="|"), simplify = F),
  month_day=ifelse(month_day=="character(0)", NA_character_, month_day),
  day=str_extract_all(text, "\\d+th\\s*\\w*", simplify = F),
  day=ifelse(day=="character(0)", NA_character_, day),
  temp=str_extract_all(text, "\\S{1}\\d*\\S{1}\\d*º\\s*f|\\S{1}\\d*\\S{1}\\d*º\\s*c", simplify = F) %>% lapply(., function(x){str_remove_all(x, "\\s*")}),
  temp=ifelse(temp=="character(0)", NA_character_, temp),
  lat=str_extract_all(text, "\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*s+|\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*n+", simplify = F),
  lat=ifelse(lat=="character(0)", NA_character_, lat),
  long=str_extract_all(text, "\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*e+|\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*w+", simplify = F),
  long=ifelse(long=="character(0)", NA_character_, long),
  pressure_ridge=str_match(text, "pressure ridge"),
  depot=str_match(text, "depot")) %>% 
  mutate(month_inferred=lapply(month_day, function(x) {str_extract(x, "\\D*") %>% last()}) %>% unlist(), .after=text) %>% fill(month_inferred, .direction="down") %>% 
  mutate(day_inferred=ifelse(!is.na(month_day), lapply(month_day, function(x) {str_extract(x, "\\d{1,2}") %>% last()}) %>% unlist(), 
                                                       lapply(day, function(x) {str_extract(x, "\\d{1,2}") %>% last()}) %>% unlist()), .after=month_inferred) %>% 
  mutate(sentiment=NA)

amundsenSentiments <- amundsenJournal_df_t %>% 
  unnest_tokens(output=word, input=text, token="words", to_lower=F, drop=T) %>% 
  select(chapter, paragraph, word) %>% anti_join(stop_words)

amundsenSentiments %>% count(word, sort = TRUE) %>%  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL, x="word count")+
  theme_minimal()

bing_sentimet <- get_sentiments("bing") 

amundsenSentiments_bing <- amundsenSentiments %>% inner_join(bing_sentimet) %>% 
  group_by(paragraph, sentiment) %>% summarize(words_count=n()) %>% mutate(words_count=ifelse(sentiment=="negative", -abs(words_count), words_count))

amundsenSentiments_bing %>% ggplot(aes(paragraph, words_count, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(y = "world count") +
  theme_minimal()

amundsenSentiments %>% with(wordcloud(word, n, max.words = 100))
