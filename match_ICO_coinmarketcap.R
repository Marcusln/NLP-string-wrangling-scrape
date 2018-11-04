##
# Find which of the ICO is listed on coinmarketcap within 60 days
##

library(tidyverse)
library(magrittr)

# remove space from beginning or end of firm name
coinmarketcap$firm.name %<>%  str_trim(side = "both")
ICO$firm.name %<>%  str_trim(side = "both")

# to lower case
coinmarketcap$firm.name %<>% tolower()
ICO$firm.name %<>%  tolower()

# date listed må være etter date start
coinmarketcap %<>% filter(date.listed > min(ICO$date.start))

# Function to grepl the firm name from url link
webfirmnamefunc <- function(web.url.historical.prices){
  firmname <- sub('https://coinmarketcap.com/currencies/(.*)/historical-data/(.*)','\\1',web.url.historical.prices)
  firmname <- tolower(firmname) # all letters to lower case
  firmname <- str_replace_all(firmname, "[^[:alnum:]\\s|\\?|\\!|;|:|\\.|\\,|\\$]", "")
  firmname <- gsub("[[:space:]]", "", firmname)
  firmname <- firmname[firmname != ""]
  return(firmname)
}

# For loop through the web url and grab the names  with lowercase and all punctuation removed
for (i in 1:length(coinmarketcap$web.url.historical.prices)){
  coinmarketcap$web.firm.name[i] <- webfirmnamefunc(coinmarketcap$web.url.historical.prices[i])  
}


# VIEW ALL
coinmarketcap.join %>% 
  select(match, date.start, firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(is.na(match)) %>% 
  View()

# join by token
coinmarketcap.join <- ICO %>% 
  left_join(coinmarketcap %>% 
              select(token, firm.name, date.listed, web.firm.name) %>% 
              rename(firm.name.by.token = firm.name,
                     date.listed.by.token = date.listed,
                     web.firm.name.by.token = web.firm.name),
            by = "token") %>% 
  mutate(token.by.token = ifelse(!is.na(firm.name.by.token), token, NA))

# join by firm.name
coinmarketcap.join <- coinmarketcap.join %>% 
  left_join(coinmarketcap %>% 
              select(token, firm.name, date.listed, web.firm.name) %>% 
              rename(token.by.name = token,
                     date.listed.by.name = date.listed,
                     web.firm.name.by.name = web.firm.name),
            by = "firm.name") %>% 
  mutate(firm.name.by.name = ifelse(!is.na(token.by.name), firm.name, NA))

# if match in both join -> success, else NA
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(!is.na(firm.name.by.name) & !is.na(token.by.token), TRUE, NA))

# match == TRUE when matched by name
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(!is.na(firm.name.by.name), TRUE, match))

# check if filter makes sense
# if first 4 char & last 4 char in firm.name.by.token found in firm.name -> success
coinmarketcap.join %>% 
  select(match, firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(!is.na(firm.name.by.token) &
           is.na(firm.name.by.name) &
           str_detect(str_sub(coinmarketcap.join$firm.name, 1, 4),
                      coll(str_sub(coinmarketcap.join$firm.name.by.token, 1, 4), ignore_case = TRUE)) &
           str_detect(str_sub(coinmarketcap.join$firm.name, -4, -1),
                      coll(str_sub(coinmarketcap.join$firm.name.by.token, -4, -1), ignore_case = TRUE))) %>% 
  View()

# so match -> TRUE
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(!is.na(firm.name.by.token) &
                          is.na(firm.name.by.name) &
                          is.na(match) &
                          str_detect(str_sub(firm.name, 1, 4),
                                     regex(str_sub(firm.name.by.token, 1, 4), ignore_case = TRUE)) &
                          str_detect(str_sub(firm.name, -4, -1),
                                     regex(str_sub(firm.name.by.token, -4, -1), ignore_case = TRUE)), TRUE, match))

# a: hvis firm.name.by token finnes i firm.name, eller
# b: hvis firm.name finnes i firm.name.by.token
a <- str_detect(coinmarketcap.join$firm.name.by.token, coll(coinmarketcap.join$firm.name, ignore_case = TRUE))
b <- str_detect(coinmarketcap.join$firm.name, coll(coinmarketcap.join$firm.name.by.token, ignore_case = TRUE))

# sjekke om det gir mening
coinmarketcap.join %>% 
  filter(a | b) %>% 
  filter(is.na(match)) %>% 
  select(match, firm.name, firm.name.by.token) %>% 
  View()

# jepp: match -> TRUE
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(a | b, TRUE, match))

# hvis navn er likt - > match
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(firm.name == firm.name.by.name, TRUE, match))

split.match <- function(vector, regx = TRUE) {
  pat <- str_split(vector, "[:space:]|\\.")
  pat[is.na(pat)] <- "NA"
  
  if (regx == TRUE) {
    pat <- lapply(pat, function(x) sub("(.*)^", "^", x))
    pat <- lapply(pat, function(x) sub("$", "$", x))
  }
  return(pat)
}

coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(
    is.na(match) & !is.na(firm.name.by.token) &
      is.na(match) & str_detect(firm.name, fixed(firm.name.by.token)),
    TRUE, match))

coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(temp = str_c(split.match(coinmarketcap.join$firm.name.by.token), collapse = "|")) %>% 
  select(temp) %>% View()
  mutate(match = ifelse(
    is.na(match) & !is.na(firm.name.by.token) &
      is.na(match) & str_detect(firm.name, fixed(firm.name.by.token)),
    TRUE, match))

coinmarketcap.join %>% 
  mutate(firm.name.temp = split.match(firm.name, regx = FALSE)) %>% 
  filter(is.na(match) & !is.na(firm.name.by.name) |
      is.na(match) & !is.na(firm.name.by.token)) %>% 
  filter(is.na(match) & str_detect(firm.name.temp, fixed(firm.name.by.token))) %>% 
  select(match, firm.name, token, date.start,
         firm.name.by.name, token.by.name, date.listed.by.name,
         firm.name.by.token, token.by.token, date.listed.by.token) %>%
  View()

#### Vibe = vibehub er feil

# hvil
coinmarketcap.join %>% 
  filter(is.na(match) & !is.na(firm.name.by.token)) %>% 
  select(firm.name, firm.name.by.token) %>% 
  View()

match.f <- str_detect(coinmarketcap.join$firm.name, str_split(coinmarketcap.join$firm.name.by.token, "\\."))

# VIEW ALL
coinmarketcap.join %>% 
  select(match, date.start, firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(is.na(match)) %>% 
  View()

# VIEW JOIN BY TOKEN
coinmarketcap.join %>% 
  filter(is.na(match)) %>%
  select(match, date.start, firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token) %>% 
  View()

# match på token, men ikke firm.name
coinmarketcap.join %>% 
  select(match, firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(!is.na(firm.name.by.token) & is.na(firm.name.by.name)) %>% View()

# feil i dato
# ser ut til at date.listed.by.name er mest riktig
coinmarketcap.join %>% 
  select(firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name,
         match) %>% 
  filter(match == TRUE & date.listed.by.name != date.listed.by.token) %>% 
  View()

coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(date.listed = date.listed.by.name,
         diff.start.listed = date.listed - date.start,
         success = ifelse(match == TRUE & diff.start.listed <= 60, TRUE, FALSE))

coinmarketcap.join %>% 
  select(firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name,
         diff.start.listed, match, success) %>% View()

coinmarketcap.join %>% 
  filter(date.listed > as.Date("2017-07-07", format = "%Y-%m-%d")) %>% View()
mutate(length.ico )

coinmarketcap %>% 
  mutate(duplicated.token = ifelse(duplicated(coinmarketcap$token), TRUE, FALSE)) %>% 
  filter(duplicated.token == TRUE) %>% 
  arrange(token) %>% 
  View()

coinmarketcap %>% 
  group_by(token) %>% 
  filter(n() > 1) %>% 
  which()

ICO %>% 
  filter(date.start > date.end) %>% 
  View()

