library(rvest); library(xml2); library(stringr); 
library(tidyverse); library(tidyselect)


amundsenJournal_html <- read_html("https://www.gutenberg.org/files/4229/4229-h/4229-h.htm")

amundsenJournal_df <- amundsenJournal_html %>% 
  html_nodes("h1, h2, h3, h4, p") %>% html_text(trim = TRUE) %>% 
  as.data.frame() %>% rename(paragraph=1) %>% 
  mutate(paragraph=str_replace_all(paragraph, "\r\n", " "),
         paragraph=str_replace_all(paragraph, "\\\"", ""),
         paragraph=paragraph %>% tolower(),
         paragraph=ifelse(paragraph=="chapter v", "chapter v:", paragraph))

amundsenJournal_df_t <- amundsenJournal_df %>% 
  #extract chapters end filter chapters five to fourteen
  mutate(chapter=str_extract_all(paragraph, paste("chapter ", seq(1, 16, 1) %>% as.roman() %>% tolower(), ":", sep="", collapse="|"), simplify = TRUE) %>% 
           as.character() %>% na_if("") %>% str_remove_all(":"), .before=paragraph) %>% 
  fill(chapter, .direction="down") %>% 
  filter(str_detect(chapter, paste("chapter", seq(5, 14, 1) %>% as.roman() %>% tolower(), collapse="|")) & str_length(paragraph)>50) %>% 
  #extract years
  mutate(year=str_extract_all(paragraph, seq(1872, 1912, 1) %>% paste(collapse="|"), simplify = F), 
         year=ifelse(year=="character(0)", NA_character_, year),
  #extract month and year
  month=str_extract_all(paragraph, month.name %>% tolower() %>% paste(collapse="|"), simplify = F),
  month=ifelse(month=="character(0)", NA_character_, month),
  month_day=str_extract_all(paragraph, paste0(month.name %>% tolower(), "\\s\\d{1,2}", collapse="|"), simplify = F),
  month_day=ifelse(month_day=="character(0)", NA_character_, month_day),
  day=str_extract_all(paragraph, "\\d{1,2}th", simplify = F),
  day=ifelse(day=="character(0)", NA_character_, day),
  temp=str_extract_all(paragraph, "\\S{1}\\d*\\S{1}\\d*º\\s*f|\\S{1}\\d*\\S{1}\\d*º\\s*c", simplify = F) %>% lapply(., function(x){str_remove_all(x, "\\s*")}),
  temp=ifelse(temp=="character(0)", NA_character_, temp),
  lat=str_extract_all(paragraph, "\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*s+|\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*n+", simplify = F),
  long=str_extract_all(paragraph, "\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*e+\d*\W*\d+º\s*\d*\.*\d*\'*\s*e+|\\w*\\.*\\w*º\\s*\\d*\\.*\\d*\\'*\\s*w+", simplify = F)),
  pressure_ridge=str_extract_all(paragraph, "[pressure ridge]"))



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
