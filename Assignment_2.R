rm(list=ls())

library(tidyverse)
library(ggthemes)

# 1. Download data -----------------------------------------------------------

# Downloading Q1 and Q2 2018
download.file(url = "https://www.sec.gov/Archives/edgar/full-index/2018/QTR1/master.idx",  "Q1_2018", mode = "wb")
download.file(url = "https://www.sec.gov/Archives/edgar/full-index/2018/QTR1/master.idx",  "Q2_2018", mode = "wb")

# Loading data using readLines
Q1 <- readLines("Q1_2018", n=30011)
Q2 <- readLines("Q2_2018", n=30011)

# Indexing from position 12-30011 
Q1 <- Q1[12:length(Q1)]
Q2 <- Q2[12:length(Q2)]

# Choose only 10-K's
data <- Q1[grepl("\\|10-K\\|", Q1)]

# Split and store it as a dataframe
split_text <- strsplit(data, "\\|")
split_text <- unlist(split_text)

data <- data.frame(matrix(split_text, ncol=5, byrow=TRUE))
colnames(data) <- c("CIK", "Company", "10-K", "Date", "Filename")

# 2. Download filings -----------------------------------------------------

for(i in 1:length(data$Filename)){
  download.file(url = paste0("https://www.sec.gov/Archives/", data$Filename[i]),
                destfile = paste0(data$CIK[i], ".txt"), mode = "wb")
  Sys.sleep(2)
}

# 3. Cleaning -------------------------------------------------------------

# define function to be used in a loop
calc.sustainability.index <- function(CIK) {
  # read txt file and store in a single string
  connection <- file(paste0(CIK, ".txt")) # create connection
  temp.report <- readChar(con = connection, nchars = 1e6) # store file
  close(connection) # close connection
  
  # remove everything outside <TEXT> tags, works because data is in a single string
  temp.report <- gsub(".*<TEXT>|</TEXT>.*", "", temp.report)
  
  # remove HTML tags
  temp.report <- gsub("<.*?>", "", temp.report)
  
  # remove HTML entities
  temp.report <- gsub("&.*?;", "", temp.report)
  
  # remove numbers
  temp.report <- gsub('[[:digit:]]+', '', temp.report)
  
  # remove empty elements
  temp.report <- temp.report[temp.report != ""]
  
  # split string at space into one string for each word
  temp.report <- strsplit(temp.report, " ", fixed = TRUE)
  temp.report <- unlist(temp.report)
  
  # count number of instances of the words "sustainability" & "sustainable"
  # scaled: (occurrences * 1000) / word.count
  sustainability.score.raw <- sum(stringr::str_count(temp.report, pattern = "sustainability{1}"))
  sustainability.score.scaled <- (sum(stringr::str_count(temp.report, pattern = "sustainability{1}")) * 1000) / length(temp.report)
  
  sustainable.score.raw <- sum(stringr::str_count(temp.report, pattern = "sustainable{1}"))
  sustainable.score.scaled <- (sum(stringr::str_count(temp.report, pattern = "sustainable{1}")) * 1000) / length(temp.report)
  
  # insert scores to df
  sustainability[sustainability$CIK == CIK, "sustainability.score.raw"] <- sustainability.score.raw
  sustainability[sustainability$CIK == CIK, "sustainability.score.scaled"] <- sustainability.score.scaled
  sustainability[sustainability$CIK == CIK, "sustainable.score.raw"] <- sustainable.score.raw
  sustainability[sustainability$CIK == CIK, "sustainable.score.scaled"] <- sustainable.score.scaled
  
  # which is what the function returns
  return(sustainability)
}

# create dataframe for sustainability index, fill CIK
sustainability <- data.frame(CIK = data$CIK)

# add empty columns, to be appended later
sustainability$sustainability.score.raw <- NA
sustainability$sustainability.score.scaled <- NA
sustainability$sustainable.score.raw <- NA
sustainability$sustainable.score.scaled <- NA

# loop through column, i is the index
for (i in 1:length(sustainability$CIK)) {
  # fetch value of row in column via index
  id <- sustainability$CIK[i]
  # the function returns sustainability with inserted values
  # assign this to sustainability, ie. append
  sustainability <- calc.sustainability.index(CIK = id)
}

# remove companies that does not mention "sustainability" or "sustainable"
# arrange df in desc order based on scaled frequencies
sustainability %<>%
  filter(sustainability.score.raw > 0 | sustainable.score.raw > 0)%>%
  arrange(desc(sustainability.score.scaled, sustainable.score.scaled))

# 4. Compare with Yahoo index ---------------------------------------------

# load Yahoo sustainability index
load("yahoo_data.Rdata")

# format as character for later join
sustainability$CIK %<>% as.character()
yahoo.data$CIK %<>% as.character()

# merge yahoo sustainability score to sustainability by CIK
sustainability %<>% left_join(yahoo.data, by = "CIK")

# plot sustainability.score.scaled and sustainability.score.yahoo
sustainability %>% 
  na.omit() %>%  # remove companies not in yahoo index
  filter(sustainability.score.raw > 0) %>% # keep only rows with freq > 0
  ggplot(aes(x = log10(sustainability.score.scaled), y = log10(sustainability.score.yahoo))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_economist() +
  theme(text = element_text(size=12)) + 
  labs(
    title = 'Positive relationship between Yahoo sustainability score\nand occurrences of "sustainability" in annual reports',
    x = 'Occurrences of "sustainability" in the annual report (per 1000 words, logarithmic scale)',
    y = "Yahoo sustainability score (logarithmic scale)",
    caption = "Source: Yahoo, 10-K annual reports"
  )

# plot sustainable.score.scaled and sustainability.score.yahoo
sustainability %>% 
  na.omit() %>%  # remove companies not in yahoo index
  filter(sustainable.score.raw > 0) %>% # keep only rows with freq > 0
  ggplot(aes(x = log10(sustainable.score.scaled), y = log10(sustainability.score.yahoo))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_economist() +
  theme(text = element_text(size=12)) + 
  labs(
    title = 'Positive relationship between Yahoo sustainability score\nand occurrences of "sustainable" in annual reports',
    x = 'Occurrences of "sustainable" in the annual report (per 1000 words, logarithmic scale)',
    y = "Yahoo sustainability score (logarithmic scale)",
    caption = "Source: Yahoo, 10-K annual reports"
  )
