library(rvest); library(stringr); 
library(tidyverse); library(tidyselect)

year <- seq(1890, 1920, 1) %>% as.character()
month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
month_day <-  paste(rep(month, each = length(seq(1, 31, 1))), seq(1, 31, 1), sep = " ")
temp <- paste0(seq(-100, 100, 0.1), "º F")
lat_1_1 <- paste0(seq(1,180, 1), "º")
lat_1_2 <- paste0("lat.", seq(1,180, 1), "º")
lat_2_1 <- paste0(" ", seq(1,180, 1), "'")
lat_2_2 <- paste0(" ", seq(1,180, 1), "' S.")
lat_1 <- paste(rep(lat_1_1, each = length(lat_2_2)), lat_2_2, sep = "")
lat_2 <- paste(rep(lat_1_2, each = length(lat_2_1)), lat_2_1, sep = "")
lat <- c(lat_1, lat_2)


amundsenJournal_html <- read_html("https://www.gutenberg.org/files/4229/4229-h/4229-h.htm")

amundsenJournal_df <- amundsenJournal_html %>% 
  html_nodes("p") %>% html_text(trim = TRUE) %>% 
  as.data.frame() %>% rename(paragraph=1) %>% 
  mutate(paragraph=str_replace_all(paragraph, "\r\n", " "),
         paragraph=str_replace_all(paragraph, "\\\"", ""))

amundsenJournal_df <- amundsenJournal_df %>% 
  mutate(year=str_extract_all(paragraph, year %>% paste(collapse="|"), simplify = FALSE),
         month=str_extract_all(paragraph, month %>% paste(collapse="|"), simplify = FALSE),
         month_day=str_extract_all(paragraph, month_day %>% paste(collapse="|"), simplify = FALSE),
         temp=str_extract_all(paragraph, temp %>% paste(collapse="|"), simplify = FALSE),
         lat=str_extract_all(paragraph, lat %>% paste(collapse="|"), simplify = FALSE)
         )



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
