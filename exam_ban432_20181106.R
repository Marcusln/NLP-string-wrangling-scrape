# ------------------------------------------------
#
# BAN432: Final Exam
# Candidates: 73, 3, 59
#
# ------------------------------------------------

rm(list = ls())

library(pdftools)
library(purrr)
library(tm)
library(tibble)
library(dplyr)
library(tidytext)
library(slam)
library(stringr)
library(SnowballC)
library(syuzhet)
library(SentimentAnalysis)
library(tidyr)
library(tidyverse)
library(stringr)
library(tm)
library(lsa)
library(qdapRegex)
library(wordcloud)
library(ggplot2)
library(stargazer)
library(magrittr)

# Load the exam dataset
load("BAN432_exam.Rda")

load("syllable.Rda")
load("verbs.Rda")


# If the R file dosent work for some reason, we have included Rda files, that you can run to get our data
# load("coinmarketcap_dates.Rda")
# load("forum_quotes_removed.Rda")


# ------------------------------------------------
# Find date listed
# ------------------------------------------------
#
# Our dataset doesnt contain the dates for when the coin was listed
# on coinmarketcap.com. We get the dates by scraping the URL
# provided. 
###

find.date.listed <- function(URL) {
  if (!RCurl::url.exists(URL)) return(NA)
  
  # read URL
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

# create variable
coinmarketcap$date.listed <- NA

# insert date.listed in our dataframe
for (i in 1:nrow(coinmarketcap)) {
  coinmarketcap$date.listed[i] <- find.date.listed(coinmarketcap$web.url.historical.prices[i])
  print(paste0('Progress: ', i, "/", nrow(coinmarketcap)))
  Sys.sleep(2)
}

# format date
coinmarketcap$date.listed <- as.Date(coinmarketcap$date.listed, origin = "1970-01-01")



# ------------------------------------------------
# Date mixup
# ------------------------------------------------
# Some ICOs have date.start > date.end and needs to be fixed. We 
# saw that many of them matches dates from icobench.com, but
# reversed. We double check this by scraping icobench and
# comparing with the dates in our dataset. Ideally, we would
# have cross checked with multiple sources.
###

for (i in 1:nrow(ICO)) {
  coin <- ICO$firm.name[i]
  coin <- tolower(coin)
  coin <- gsub("\\.|\\s", "-", coin)
  url <- paste0("https://icobench.com/ico/", coin)
  
  s <- readLines(url, warn = FALSE, skipNul = TRUE)
  s <- gsub('\\t', "", s)
  
  if (any(str_detect(s, "^ICO[[:space:]]start$"))) {
    # locate ~where info is stored
    start <- str_which(s, "^ICO[[:space:]]start$")
    # define start and end of relevant data
    start <- start + 3
    end <- start + 7
    s <- s[start:end]
    # remove HTML tags, dates are isolated in lines
    s <- gsub("<.*?>", "", s)
    
    ICO$date.start2[i] <- dmy(s[1])
    ICO$date.end2[i] <- dmy(s[8])
    
    print(paste0('Progress ', i, '/', nrow(ICO)))
    Sys.sleep(1)
  } else {
    # if not a match then store as unreasonable date to inspect later
    ICO$date.start2[i] <- as.Date("1900-01-01", format = "%Y-%m-%d")
    ICO$date.end2[i] <- as.Date("1900-01-01", format = "%Y-%m-%d")
    
    print(paste0('Error at: ', i, '/', nrow(ICO)))
    Sys.sleep(1)
  }
}

# format dates 
ICO$date.start2 <- as.Date(ICO$date.start2, origin = "1970-01-01")
ICO$date.end2 <- as.Date(ICO$date.end2, origin = "1970-01-01")

# if date.start > date.end, or date.start < date.end, and
# there is a match on icobench.com on the reversed date,
# use dates from icobench.com.
# There are only a few ICOs that dont match, we
# assume that the pattern is valid also for these,
# and switch the dates so that all date.start < date.end

ICO <- ICO %>% 
  mutate(
    date.start = if_else(
      date.start > date.end &
        date.start == date.end2 &
        date.end == date.start2,
      date.end, date.start
    ),
    date.end = if_else(
      date.end < date.start &
        date.start == date.end2 &
        date.end == date.start2,
      date.start, date.end
    ),
    date.start = if_else(date.start > date.end, date.end, date.start),
    date.end = if_else(date.end < date.start, date.start, date.end)
  ) %>% 
  select(-c(date.start2, date.end2))

# ------------------------------------------------
# Crypto categories
# ------------------------------------------------
# Get cryptocurrency coins from icodrops.com.
# They are used later in the prediction section.
###

scrape <- readLines("https://icodrops.com/category/ended-ico/")

i <- str_which(scrape, '<span class="ico-category-name">.')
cat <- scrape[i]
cat <- gsub("<.*?>|\\t|</span>", "", cat)

i <- str_which(scrape, "ico-main-info")
coin <- scrape[i + 1]
coin <- gsub("\t", "", coin)
coin <- gsub("<.*?>", "", coin)
coin <- coin[coin != ""]

coin.categories <- tibble(firm.name = coin, category = cat)

# ------------------------------------------------
# Quotes
# ------------------------------------------------
# For sentiment analysis, the quotes represent noise in the data. The
# quote is not something the user has said, and a sentiment score with
# the quote included would be distorted. Hence we remove the quote from
# the post. But first we extract the info about the quote, i.e. the date
# and user etc. In the next section, we use the dates as a filter when
# searching for matching parts of the post.
###

# split string into sentences
forum$text.split <- strsplit(forum$text, '(?<![^!?.])\\s+', perl = TRUE)

# variable for how many quotes there are in a post
forum$n.quotes <- str_count(forum$text, "Quote")

has.quote <- which(forum$n.quotes > 0)
quote.regex <- "Quote[:space:]from:.+[:space:][:digit:]{2}:[:digit:]{2}:[:digit:]{2}[:space:](AM|PM)"
quote.date.regex <- "[:alpha:]+[:space:][:digit:]{1,2},[:space:]20(17|18)"

# create variable to store dates of the quotes
forum$quote.dates <- NA

# store erros from loop
str.detect.errors <- c()
gsub.errors <- c()

# loop through rows that contain quotes
for (i in has.quote) {
  print(paste0('Progress: ', i, '/', nrow(forum)))
  
  # if str_detect() doesnt work, store row in vector and skip iteration
  if (str_detect(forum$text.split[[i]][1], quote.regex) == FALSE) {
    str.detect.errors <- append(str.detect.errors, i)
    print(paste('str_detect error at row: ', i))
    next
  }
  
  # check for error in gsub()
  possibleError <- tryCatch(
    gsub(str_extract(forum$text.split[[i]][1], quote.regex), "", forum$text.split[[i]][1], perl = TRUE),
    error = function(e) e
  )
  
  # if error -> store error in vector and skip iteration
  if(inherits(possibleError, "error")) {
    gsub.errors <- append(gsub.errors, i)
    print(paste('gsub error at row: ', i))
    next
  }
  
  # extract quote info
  quote <- str_extract(forum$text.split[[i]][1], quote.regex)
  
  # remove it from the post
  forum$text.split[[i]][1] <- gsub(quote, "", forum$text.split[[i]][1], perl = TRUE)
  
  # split quotes info into individual quote info, store as list in column
  forum$quote.dates[i] <- strsplit(quote, "(?<=[A|P]M)", perl = TRUE)
  
  # for each quote in post, store date(s) as list in column
  for (n in 1:forum$n.quotes[i]) {
    forum$quote.dates[i][[1]][n] <- as.Date(str_extract(forum$quote.dates[i][[1]][n], quote.date.regex), format = "%B %d, %Y")
  }
}

# count errors
length(str.detect.errors); length(gsub.errors)

# check for missed quote
table(str_detect(forum$text.split, "Quote"))

# ------------------------------------------------
# Remove the quote itself
# ------------------------------------------------
# In this section, quotes are removed. Ideally, one could fetch the
# excact beginning and end of the quotes in the HTML markup after
# scraping the website. However, this was not possible in our
# dataset.
# 
# We remove quotes by searching for matching strings in other posts
# that does not contain a quote. There might be some errors caused
# by using this method. For example, the quote might come from a post
# which also contain quotes. If two sentences are identical, we risk
# losing valuable data. However, we have applied a filter to avoid this
# as much as possible. Short sentences are more likely to be identical, 
# but also less likely to have great importance, analogous to the tf-idf
# statistic. The algorithm for removing a quote is:
#
# loop through forum posts, and each string in a post:
# if a string in the post matches a string in another post
# which was posted on a date which matches any of the dates
# listed in forum%quote.dates and the post does not contain a quote
# -> remove the string
###

# format dates as numeric
forum$quote.dates <- lapply(forum$quote.dates, function(x) sapply(x, as.numeric))

# create vector to store indices of posts with removed quotes
removed.quotes <- c()

# NB! This could take 3-6 hours
# -> one could also load forum_quotes_removed.Rda
start <- Sys.time()

for (i in has.quote) {
  print(paste0('Progress: ', i, '/', length(has.quote), ' (~', round(i / nrow(forum), digits = 3) * 100, '%)'))
  
  post <- forum$text.split[i]
  post.len <- length(forum$text.split[i][[1]])
  
  for (s in 1:post.len) {
    posts <- forum$text.split
    
    # to save on computation cost, search for a match
    # only in posts without any quotes
    # and only posts on given dates
    filter <- which(forum$n.quotes == 0 & as.numeric(forum$date) %in% forum$quote.dates[[i]])
    filtered.posts <- posts[filter]
    
    # if a string in the post matches
    # any other strings in the filtered posts
    if (
      any(
        grepl(
          forum$text.split[i][[1]][s],
          filtered.posts,
          fixed = TRUE, useBytes = TRUE
        )
      )
    ) { # remove the post
      print(paste('Removing string', s, "of", post.len))
      forum$text.split[i][[1]][s] <- ""
      # and store index
      removed.quotes <- append(removed.quotes, i)
    }
  }
}

print(paste("Time elapsed:", Sys.time() - start)) # 6 hrs

# combine strings in the list element into one string and return a vector
forum$text.split <- sapply(forum$text.split, function(x) str_c(x, collapse = " "))

# remove URLs
forum$text.split <- rm_url(forum$text.split)

# remove empty strings in list
forum$text.split <- lapply(forum$text.split, function(x) x[sapply(x, nchar)>0])

# replace punctuation
forum$text.split <- gsub("\\?|\\!|\\.|,", " ", forum$text.split)

# format to lower for later matching
forum$token <- tolower(forum$token)

# Filtering out all forum posts that we're not going to use 
# for this analysis, because the posts are written after the ico has started

forum <- forum %>%
  left_join(ICO, by="token") %>%
  select(date, date.start, user.name, user.status,
         text,token,ICO.issuer,text.split) %>%
  mutate(time_diff = date - date.start) %>%
  filter(time_diff < 0)

# ------------------------------------------------
# Document Term Matrix: Forum
# ------------------------------------------------
# Create document term matrix. A few different parameters
# were tested, and chosen based on prediction results.
# Only meaningful words are kept, and punctuation is
# removed (this solves our problem of some
# sentences being.like.this).
#
# Stemming interprets terms with the same meaning as one
# term. For example, fishing and fished will be treated as
# the same word. Finally, the weighting function used is
# the term frequency inverse document frequency. Tf-idf
# reflect how important a term is in the document, i.e.
# it is proportional. If a term is used very frequently, it
# becomes inflated, and the function makes sure that it is
# weighted less. For example, the term "scandal" is more influential
# if it is not used that often.
###

dtm.forum <-
  Corpus((VectorSource(forum$text.split))) %>%
  DocumentTermMatrix(control = list(tolower = TRUE,
                                    removeNumbers = TRUE,
                                    removePunctuation = TRUE,
                                    stopwords = TRUE,
                                    wordLengths = c(5, 20),
                                    stripWhitespace = TRUE,
                                    stemming = TRUE,
                                    weighting = function(x) weightTfIdf(x)))

##
# Two Document Term Matrix were compared.
# One with only the quote info removed, and the
# other with the quote itself removed.
#
# Terms in forum only quote info removed: 47249
# Terms in forum with quotes removed: 47106
# (this was calculated before we removed
# irrelevant posts, so count is now different)
# -> lost ~143 words -> acceptable, but not
# a strong indicator of how much data is lossed
##

# ------------------------------------------------
# Whitepapers
# ------------------------------------------------
# Some whitepapers are missing. We scrape icodrops.com
# to try to get as many as possible. Ideally, we could
# have used several sources and most likely found all 
# whitepapers needed for our dataset.
###

dir <- paste0(getwd(), "/whitepaper/")
pdfs <- paste0(dir, list.files(dir, pattern = "*.pdf"))

# Change all names to lowercase, in case some dont match on token. 
names <- list.files(dir, pattern = "*.pdf")
names <- tolower(names)

# Match ICO df with an ID and boolean variable for whether the token has a whitepaper 
match_whitepaper <- 
  names %>% 
  str_remove(pattern = ".pdf") %>%
  as_data_frame() %>%
  mutate(whitepaper = 1)%>%
  rename(token = value)

for (i in 1:nrow(match_whitepaper[which(match_whitepaper$whitepaper == 0)])) {
  coin <- match_whitepaper$firm.name[i]
  coin <- tolower(coin)
  coin <- gsub("\\.|\\s", "-", coin)
  url <- paste0("https://icodrops.com/", coin)
  
  if (!RCurl::url.exists(url)) next
  
  s <- readLines(url, warn = FALSE, skipNul = TRUE)
  s <- gsub('\\t', "", s)
  i <- str_which(s, "WHITEPAPER")
  s[i] <- gsub('.WEBSITE', "", s[i])
  s[i] <- gsub('<a href=\"', "", s[i])
  url <- gsub('(.*)" target=\"_blank\" rel=\"nofollow\"><div class=\"button\" </div></a>(.*)" target=\"_blank\" rel=\"nofollow\"><div class=\"button\" >WHITEPAPER</div></a><div class=\"sosial-links-title\">social links</div>', "\\2", s[i])
  
  # check for error in gsub()
  possibleError <- tryCatch(
    download.file(url, destfile = paste0('whitepaper/', coin, ".pdf")),
    error = function(e) e
  )
  
  # if error -> store error in vector and skip iteration
  if(inherits(possibleError, "error")) {
    print(paste('Invalid URL: ', url, 'at', i))
    next
  }
}

# Read pdf-files that can be handled with pdf_text
pdf <- map(pdfs, pdf_text)

ICO <- ICO %>% left_join(match_whitepaper, by=c("token"))

# Setting NA's in column whitepaper to 0 
ICO$whitepaper[is.na(ICO$whitepaper)] <- 0
ICO <- ICO %>%
  arrange(token)

# Create Corpus
pdfCorpus <- Corpus(VectorSource(pdf))

for(i in 1:length(pdfCorpus)){
  pdfCorpus[[i]] <- gsub("\\\\r\\\\n", " ",pdfCorpus[[i]])
}

# Make DTM
dtm <- DocumentTermMatrix(pdfCorpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE,
  wordLengths = c(5,20),
  stripWhitespace = TRUE,
  stemming = TRUE
  #bounds = list(global = c(5, 25))
))

# Term Frequency 
term.freq <- tibble(term = dtm$dimnames$Terms, 
                    freq = col_sums(dtm))

term.freq %>% arrange(desc(freq))

# --------------------------------------------------
# Cosine similarity
# --------------------------------------------------
###

ICO %<>% mutate(sixtydays = date.start + 60)

names(pdf)[1] <- "OPTI"

dtm.all <-
  Corpus(VectorSource(pdf)) %>%
  DocumentTermMatrix(control = list(removeNumbers = TRUE,
                                    stopwords = TRUE,
                                    removePunctuation = TRUE,
                                    wordLengths = c(3, 25),
                                    tolower = TRUE, 
                                    stemming = TRUE,
                                    weighting = function(x) weightTfIdf(x)))

pdf.token <- gsub(dir, "", pdfs)
pdf.token <- gsub(".pdf", "", pdf.token, fixed = TRUE)
pdf.token <- tolower(pdf.token)
pdf.token[1] <- "opti"
rownames(dtm.all) <- pdf.token
rownames(dtm.all)[1] <- "OPTI"

calculate.cosine <- function(token) {
  start.token <- ICO[ICO$token == token, "date.start"]
  f <- which(ICO$success == TRUE & as.numeric(ICO$sixtydays) <= as.numeric(start.token))
  look <- ICO$token[f]
  
  # adjust DTM to unit length by applying formula to each column
  dtm.all <- apply(dtm.all[, 1:ncol(dtm.all)], 2, function(x) x / sqrt(sum(x^2)))
  
  dtm.success <- dtm.all[which(rownames(dtm.all) %in% look), ]
  term.vector <- dtm.all[which(rownames(dtm.all) == coin), ]
  
  cosines <- vector("numeric", nrow(dtm.success))
  for (i in 1:nrow(dtm.success)) {
    cosines[i] <- cosine(dtm.success[i, ], term.vector)
  }
  
  cosines[which(is.nan(cosines))] <- 0
  cosine.success <- mean(cosines, na.rm = TRUE)
  
  return(cosine.success)
}

cosine.coins <- tibble(cosine = 1:length(pdf.token))
cosine.coins$cosine[1] <- calculate.cosine(token = "OPTI")
for (i in 2:length(pdf.token)) { 
  print(i)
  coin <- pdf.token[i]
  cosine.coins$cosine[i] <- calculate.cosine(token = coin)
}

cosine.coins$token <- pdf.token

# Loop through the token and make sure all punctuation removed, and all lowercase
for (i in 1:length(cosine.coins$token)){
  cosine.coins$token[i] <- tokenlowerfunc(cosine.coins$token[i])  
}