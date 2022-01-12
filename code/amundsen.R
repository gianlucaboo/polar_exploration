library(rvest); library(xml2); library(stringr); 
library(tidyverse); library(tidyselect)


lat_1_1 <- paste0(seq(1,90, 1), "º")
lat_1_2 <- paste0("lat.", seq(1,90, 1), "º")
lat_2_1 <- paste0(" ", seq(1,90, 1), "'")
lat_2_2 <- paste0(" ", seq(1,90, 1), "' S.")
lat_1 <- paste(rep(lat_1_1, each = length(lat_2_2)), lat_2_2, sep = "")
lat_2 <- paste(rep(lat_1_2, each = length(lat_2_1)), lat_2_1, sep = "")
lat <- c(lat_1, lat_2)


amundsenJournal_html <- read_html("https://www.gutenberg.org/files/4229/4229-h/4229-h.htm")

amundsenJournal_df <- amundsenJournal_html %>% 
  html_nodes("h1, h2, h3, h4, p") %>% html_text(trim = TRUE) %>% 
  as.data.frame() %>% rename(paragraph=1) %>% 
  mutate(paragraph=str_replace_all(paragraph, "\r\n", " "),
         paragraph=str_replace_all(paragraph, "\\\"", ""),
         paragraph=paragraph %>% tolower(),
         paragraph=ifelse(paragraph=="chapter v", "chapter v:", paragraph))

amundsenJournal_df_t <- amundsenJournal_df %>% 
  mutate(chapter=str_extract_all(paragraph, paste("chapter ", seq(1, 16, 1) %>% as.roman() %>% tolower(), ":", sep="", collapse="|"), simplify = TRUE) %>% 
           as.character() %>% na_if("") %>% str_remove_all(":"), .before=paragraph) %>% 
  fill(chapter, .direction="down") %>% 
  filter(str_detect(chapter, paste("chapter", seq(5, 14, 1) %>% as.roman() %>% tolower(), collapse="|")) & str_length(paragraph)>50) %>% 
  mutate(year=str_extract_all(paragraph, seq(1872, 1912, 1) %>% 
                                paste(collapse="|"), simplify = FALSE))
,
         month=str_extract_all(paragraph, month.name %>% tolower() %>% 
                                 paste(collapse="|"), simplify = FALSE),
         month_day=str_extract_all(paragraph, paste(rep(month.name %>% tolower(), each = length(seq(1, 31, 1))), seq(1, 31, 1), sep = "") %>% 
                                     paste(collapse="|"), simplify = FALSE),
         day_year=str_extract_all(paragraph, paste(rep(seq(1, 31, 1), length(seq(1, 31, 1))), seq(1872, 1912, 1), sep=",") %>% 
                                          paste(collapse="|"), simplify = FALSE))
,
         temp=str_extract_all(paragraph, paste0(seq(-100, 100, 0.1), "ºF") %>% 
                                paste(collapse="|"), simplify = FALSE),
         ) %>% fill(chapter, .direction="down")

lat=str_extract_all(paragraph, lat %>% paste(collapse="|"), simplify = FALSE)


,
day=regmatches(paragraph, gregexpr(month[1], paragraph)),
numeric=regmatches(paragraph, gregexpr('[0-9]+',paragraph)))

month_length=str_length(month),
day=substring(sentence, first=regexpr(pattern=month, text=sentence)+month_length))

str_length(month))

long=ifelse(str_detect(.$sentence, "long | Long | longitude | Longitude"), 1, 0),
lat=ifelse(str_detect(.$sentence, "lat | Lat | latitude | Latitude"), 1, 0),
degrees=ifelse(str_detect(.$sentence, "º"), 1, 0),
degrees_f=ifelse(str_detect(.$sentence, "º F"), 1, 0),
south=ifelse(str_detect(.$sentence, "south | South"), 1, 0),
west=ifelse(str_detect(.$sentence, "west | West"), 1, 0),
lat_degree=substring(sentence, 
                     first=regexpr(" lat.", .$sentence, fixed = TRUE)-10, 
                     last=regexpr(" lat.", .$sentence, fixed = TRUE)+10))





temp_f=substring(sentence, 
                 first=regexpr("º F", .$sentence)-4, 
                 last=regexpr("º F", .$sentence)+2) %>% 
  ifelse(grepl("º F", .), ., NA) %>% 
  str_replace_all("O", "0") %>% 
  gsub("[^0-9.-]", "", .) %>% 
  as.numeric())


%>% 
  mutate(temps=gsub("[^0-9.-]", "", temp))


b <- a[100:110]
b <- str_split(a, ".")
