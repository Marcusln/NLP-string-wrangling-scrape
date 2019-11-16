# store each line in the source code of URL
btc.prices <- readLines("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20170101&end=20171231")

# keep only lines/rows which contains "<td"
btc.prices <- subset(btc.prices, grepl("<td", btc.prices))

# remove all HTML tags
btc.prices <- gsub("<.*?>", "", btc.prices)

# store as data frame to access index
btc.prices <- as.data.frame(btc.prices)

# every 7th row is data for a new date -> jump through lines and store in columns
btc.prices <- data.frame(date = btc.prices[seq(1, nrow(btc.prices), by = 7), ],
                         open = btc.prices[seq(2, nrow(btc.prices), by = 7), ],
                         high = btc.prices[seq(3, nrow(btc.prices), by = 7), ],
                         low = btc.prices[seq(4, nrow(btc.prices), by = 7), ],
                         close = btc.prices[seq(5, nrow(btc.prices), by = 7), ],
                         volume = btc.prices[seq(6, nrow(btc.prices), by = 7), ],
                         market.cap = btc.prices[seq(7, nrow(btc.prices), by = 7), ])

# format as date
btc.prices$date = as.Date(btc.prices$date, format = "%b %d, %Y")

# remove commas from volume and market.cap (which converts them to character)
btc.prices$volume <- gsub(",", "", btc.prices$volume)
btc.prices$market.cap <- gsub(",", "", btc.prices$market.cap)

# format factors to numeric
btc.prices[, 2:5] <- as.numeric(as.character(unlist(btc.prices[, 2:5])))

# format character cols to numeric
btc.prices[, 6:7] <- as.numeric(unlist(btc.prices[, 6:7]))

library(ggplot2)
library(ggthemes)
ggplot(data = na.omit(btc.prices), aes(x = date, y = close)) +
  geom_line() +
  theme_economist() +
  labs(
    title = "Bitcoin price",
    x = "Date",
    y = "Closing price in USD",
    caption = "Source: coinmarketcap.com"
    )


# Function email check ----------------------------------------------------

email.check <- function(email) {
  address <- email
  
}