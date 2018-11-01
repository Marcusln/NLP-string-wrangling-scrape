library(RCurl)

find.date.listed <- function(URL) {
  if (!RCurl::url.exists(URL)) return(NA)
  
  coin.prices <- readLines(URL, warn = FALSE)
  
  # keep only lines/rows which contains "<td"
  coin.prices <- subset(coin.prices, grepl("<td", coin.prices))
  
  # remove all HTML tags
  coin.prices <- gsub("<.*?>", "", coin.prices)
  
  # every 7th row is data for a new date -> jump through lines and store in columns
  coin.prices <- coin.prices[seq(1, length(coin.prices), by = 7)]
  
  # remove whitespace before and after string
  coin.prices <- trimws(coin.prices, which = "both")
  
  # format as date
  coin.prices <- as.Date(coin.prices, format = "%b %d, %Y")
  
  # return earliest date, i.e. date of listing
  return(min(coin.prices))
}

coinmarketcap$date.listed <- NA

for (i in 1:nrow(coinmarketcap)) {
  coinmarketcap$date.listed[i] <- find.date.listed(coinmarketcap$web.url.historical.prices[i])
  print(paste0('Progress: ', i, "/", nrow(coinmarketcap)))
  Sys.sleep(2)
}

coinmarketcap$date.listed <- as.Date(coinmarketcap$date.listed, origin = "1970-01-01")

##
# Find which of the ICO is listed on coinmarketcap within 60 days
##

# match is different by firm.name and token
table(ICO$firm.name %in% coinmarketcap$firm.name, ICO$token %in% coinmarketcap$token)

# join by token
coinmarketcap.join <- ICO %>% 
  left_join(coinmarketcap %>% 
              select(token, firm.name, date.listed) %>% 
              rename(firm.name.by.token = firm.name,
                     date.listed.by.token = date.listed),
            by = "token") %>% 
  mutate(token.by.token = ifelse(!is.na(firm.name.by.token), token, NA))

# join by firm.name
coinmarketcap.join <- coinmarketcap.join %>% 
  left_join(coinmarketcap %>% 
              select(token, firm.name, date.listed) %>% 
              rename(token.by.name = token,
                     date.listed.by.name = date.listed),
            by = "firm.name") %>% 
  mutate(firm.name.by.name = ifelse(!is.na(token.by.name), firm.name, NA))

# her er det match i begge -> successful (men noen feil i datoer)
coinmarketcap.join %>% 
  select(firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(!is.na(firm.name.by.name) & !is.na(token.by.token)) %>% View()

# if match in both join -> success, else NA
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(!is.na(firm.name.by.name) & !is.na(token.by.token), TRUE, NA))

# if match in both join -> success, else NA
# coinmarketcap.join <- coinmarketcap.join %>% 
#   mutate(match = ifelse(token == token.by.token & firm.name == firm.name.by.name, TRUE, NA))

# match på name, men ikke token
coinmarketcap.join %>% 
  select(firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(!is.na(firm.name.by.name) & is.na(firm.name.by.token)) %>% View()

# dobbeltsjekk
identical(
  coinmarketcap.join %>% filter(!is.na(firm.name.by.name) & is.na(firm.name.by.token)),
  coinmarketcap.join %>% filter(!is.na(firm.name.by.name) & is.na(token.by.token))
)

# match på token, men ikke firm.name
coinmarketcap.join %>% 
  select(match, firm.name, token,
         firm.name.by.token, token.by.token, date.listed.by.token,
         firm.name.by.name, token.by.name, date.listed.by.name) %>% 
  filter(!is.na(firm.name.by.token) & is.na(firm.name.by.name)) %>% View()

identical(
  coinmarketcap.join %>% filter(!is.na(token.by.token) & is.na(firm.name.by.name)),
  coinmarketcap.join %>% filter(!is.na(token.by.token) & is.na(token.by.name))
)

# if firm.name found in firm.name.by.token -> success
# if firm.name.by.token found in firm.name -> success
# if first 4 char & last 4 char in firm.name.by.token found in firm.name -> success
coinmarketcap.join <- coinmarketcap.join %>% 
  mutate(match = ifelse(str_detect(coinmarketcap.join$firm.name.by.token, coll(coinmarketcap.join$firm.name, ignore_case = TRUE)), TRUE, match),
         match = ifelse(str_detect(coinmarketcap.join$firm.name, coll(coinmarketcap.join$firm.name.by.token, ignore_case = TRUE)), TRUE, match),
         match = ifelse(!is.na(firm.name.by.token) &
                            is.na(firm.name.by.name) &
                            str_detect(str_sub(coinmarketcap.join$firm.name, 1, 4),
                                       coll(str_sub(coinmarketcap.join$firm.name.by.token, 1, 4), ignore_case = TRUE)) &
                            str_detect(str_sub(coinmarketcap.join$firm.name, -4, -1),
                                       coll(str_sub(coinmarketcap.join$firm.name.by.token, -4, -1), ignore_case = TRUE)), TRUE, match))

# disse mangler
coinmarketcap.join %>%
  select(success, match, token, firm.name,
         token.by.token, firm.name.by.token,
         token.by.name, firm.name.by.name) %>%
  filter(!is.na(match)) %>% View()
  filter(!is.na(token.by.name) | !is.na(token.by.token) | !is.na(firm.name.by.name) | !is.na(firm.name.by.token)) %>% 
  View()

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
