#
# I want to improve the script, but there's a bug here somewhere...
#
# 
##

rm(list = ls())
load("firm_dataset.Rda")

library(ca)
library(tm)
library(lsa)
library(tidyverse)
library(scales)
library(slam) # col_sums in slam package can be handy to illustrate term.freq

# Task 2 ------------------------------------------------------------------

# create function used for creating term vector and calculate cosine similarity
# the function is also used in task 3, with different parameters
oil.lookalikes <- function(text.source, bound.lower, bound.upper, n.firms) {
  # add industry code as column names to text.source
  names(text.source) <- raw.data$industry.fama.french.49
  
  # create corpus
  corpus <- Corpus(VectorSource(text.source))
  
  # create Document Term Matrix
  dtm_all <- DocumentTermMatrix(corpus, 
                                control = list(stopwords = T,
                                               tolower = T,
                                               stripWhitespace = T,
                                               removeNumbers = T,
                                               removePunctuation = T,
                                               wordLengths = c(5, 20),
                                               bounds = list(global = c(bound.lower, bound.upper)),
                                               # set term freq to be binary
                                               weighting = function(x) weightBin(x)))
  
  # First we take our dtm and grp out only the Oil companies, should be 18 documents
  dtm_oil <- dtm_all[grep("30 Oil", rownames(dtm_all)), ]
  
  # adjust term vector to unit length by applying formula to each column
  dtm_all <- apply(dtm_all[, 1:ncol(dtm_all)], 2, function(x) x / sqrt(sum(x^2)))
  dtm_oil <- apply(dtm_oil[, 1:ncol(dtm_oil)], 2, function(x) x / sqrt(sum(x^2)))
  
  # We then make our Term Vector Representative for the Oil companies. Two alternatives for finding term.freq
  term_vec_oil <- colSums(as.matrix(dtm_oil), na.rm = T)
  
  # Find the cosine similarity between the oil companies and the rest. Create an empty vector first
  cosine_tibble <- tibble(cik = raw.data$cik, sector = raw.data$industry.fama.french.49, cosine_oil = NA)
  
  # for each company, calculate cosine between the company and the oil sector
  for(i in 1:nrow(dtm_all)){
    cosine_tibble$cosine_oil[i] <- cosine(dtm_all[i, ], term_vec_oil)
  }
  
  # subset tibble to oil companies and sort by cosine, to be returned by function
  companies.to.invest <- cosine_tibble %>%
    filter(sector!="30 Oil")%>%
    arrange(desc(cosine_oil))%>%
    top_n(n.firms)
  
  return(companies.to.invest)
}

# initial portfolio
initial.portfolio <- oil.lookalikes(text.source = section.1.business,
                                     bound.lower = 5,
                                     bound.upper = 50,
                                     n.firms = 25)

##
# Calculations of statistics is done for all portfolios in task 3, see final.df
##

# Task 3 ------------------------------------------------------------------

# part 1
min50.max100.doc <- oil.lookalikes(text.source = section.1.business,
                                   bound.lower = 50,
                                   bound.upper = 100,
                                   n.firms = 25)

# part 2
more.than.100.doc <- oil.lookalikes(text.source = section.1.business,
                                    bound.lower = 100,
                                    bound.upper = "Inf",
                                    n.firms = 25)

# part 3
section.7.mda <- oil.lookalikes(text.source = section.7.mda,
                                bound.lower = 5,
                                bound.upper = 50,
                                n.firms = 25)

# part 4
fifty.firms <- oil.lookalikes(text.source = section.1.business,
                              bound.lower = 5,
                              bound.upper = 50,
                              n.firms = 50)

# part 5
hundred.firms <- oil.lookalikes(text.source = section.1.business,
                                bound.lower = 5,
                                bound.upper = 50,
                                n.firms = 100)

# create list of portfolios
list.portfolios <- list(
  initial.portfolio,
  min50.max100.doc,
  more.than.100.doc,
  section.7.mda,
  fifty.firms,
  hundred.firms
)

# add name to list elements
names(list.portfolios) <- c(
  "initial.portfolio",
  "min50.max100.doc",
  "more.than.100.doc",
  "section.7.mda",
  "fifty.firms",
  "hundred.firms"
)

# PART 2: Computing returns -----------------------------------------------

compare.portfolios <- function(var.prefix, df) {
  portfolio.this <- paste(var.prefix, "return", sep = ".")
  
  temp.df <- raw.data %>%
    # create factor for which portfolio a company belongs
    mutate(portfolio = factor(ifelse(industry.fama.french.49 == "30 Oil", "oil",
                                     ifelse(cik %in% df$cik, portfolio.this, "unknown")))) %>% 
    select(portfolio, starts_with("return.monthly.NY.m")) %>%
    # group by portfolio so we can calculate their respective returns
    group_by(portfolio) %>% 
    summarise_if(is.numeric, mean, na.rm=T)  %>%
    ungroup() %>%
    select(-portfolio) %>% 
    t() %>%
    `colnames<-`(c("oil", portfolio.this, "unknown")) %>% 
    as.data.frame() %>% 
    select(-unknown)
  
  var.diff <- paste(var.prefix, "diff", sep = ".")
  var.rmse <- paste(var.prefix, "rmse", sep = ".")
  var.mae <- paste(var.prefix, "mae", sep = ".")
  var.cor <- paste(var.prefix, "cor", sep = ".")
  
  # add columns to df with metrics
  temp.df[[var.diff]] <- temp.df$oil - temp.df[[portfolio.this]]
  temp.df[[var.rmse]] <- sqrt(sum(temp.df[[var.diff]]^2) / nrow(temp.df))
  temp.df[[var.mae]] <- sum(abs(temp.df[[var.diff]])) / nrow(temp.df)
  temp.df[[var.cor]] <- cor(temp.df$oil, temp.df[[portfolio.this]])
  temp.df$oil <- NULL
  
  return(temp.df)
}

# create empty list, which should contain df's returned by function above
list.temp.df <- list()
i <- 1

# loop through list of portfolios
for (portfolio in names(list.portfolios)) {
  # store the actual df, not the name
  portfolio.df <- list.portfolios[[portfolio]]
  # names of portfolio is used as variable prefix
  temp.df <- compare.portfolios(var.prefix = portfolio, df = portfolio.df)
  # the counter i is defined above, acts as index of the list of portfolios
  # store df returned by function as an element in the list
  list.temp.df[[i]] <- temp.df
  i <- i + 1
}

# bind all columns/df's returned by the function
final.df <- bind_cols(list.temp.df)
# create column with dates
final.df$months <- seq.Date(as.Date("01.01.2014", format = "%d.%m.%Y"), as.Date("01.12.2014", format = "%d.%m.%Y"), by = "month")
final.df$oil.return <- raw.data %>% filter(industry.fama.french.49 == "30 Oil") %>% select(starts_with("return.monthly.NY.m")) %>% summarise_if(is.numeric, mean, na.rm = T) %>% t() %>% `row.names<-`(NULL)

# plot monthly returns by our portfolios
final.df %>% 
  ggplot(aes(x = months)) + 
  geom_line(aes(y = oil.return * 100, color = "Oil")) +
  geom_line(aes(y = initial.portfolio.return * 100, color = "Oil tracking, initial")) +
  geom_line(aes(y = min50.max100.doc.return * 100, color = "Min 50, max 100 doc")) +
  geom_line(aes(y = more.than.100.doc.return * 100, color = "More than 100 doc")) + 
  geom_line(aes(y = section.7.mda.return * 100, color = "Section 7 MDA")) +
  geom_line(aes(y = fifty.firms.return * 100, color = "50 firms")) +
  geom_line(aes(y = hundred.firms.return * 100, color = "100 firms")) +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%B")) +
  scale_y_continuous(breaks = seq(-20, 10, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly returns of portfolios", x = "2014", y = "Monthly return (%)", color = "Portfolio")

profit.df <- data.frame(row.names = c(names(list.portfolios))) # "oil", 

for (portfolio in names(list.portfolios)) {
  investment = 1000000
  for (month in 1:12) {
    portfolio.return <- paste(portfolio, "return", sep = ".")
    return <- final.df[[portfolio.return]][month]
    investment <- investment + investment * return
    if (month == 12) { profit.df[portfolio, "profit"] <- investment }
  }
}






