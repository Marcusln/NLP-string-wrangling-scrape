#
# Remove info about quotes in posts ---------------------------------------
##

rm(list = ls())
load("BAN432_exam.Rda")

# split string into sentences
forum$text.split <- strsplit(forum$text, '(?<![^!?.])\\s+', perl = TRUE)
forum$text.split2 <- strsplit(forum$text, '(?<![^!?.])\\s+', perl = TRUE)

# variable for how many quotes there are in a post
forum$n.quotes <- str_count(forum$text, "Quote")

has.quote <- which(forum$n.quotes > 0)
quote.regex <- "Quote[:space:]from:.+[:digit:]{2}:[:digit:]{2}:[:digit:]{2}[:space:](AM|PM)"
quote.date.regex <- "[:alpha:]+[:space:][:digit:]{1,2},[:space:]20(17|18)"

# create variable to store dates of the quotes
forum$quote.dates <- NA

# store erros from loop
str.detect.errors <- c()
gsub.errors <- c()

# loop through rows that contain quuotes
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

# these are arguably acceptable
length(str.detect.errors); length(gsub.errors)

# check for missed quote
table(str_detect(forum$text.split, "Quote"))

# View errors
forum %>% 
  .[-c(str.detect.error, gsub.errorss),] %>% 
  filter(str_detect(text.split, "Quote")) %>% 
  View()

# create backup
forum2 <- forum


# Remove the quote itself -------------------------------------------------

table(length(forum$text.split)) # a post is a list with length 1
# NB: burde telle antall unike ord og sjekke om man mister noe

# format dates as numeric
forum$quote.dates <- lapply(forum$quote.dates, function(x) sapply(x, as.numeric))

# store errors from loop
match.post.errors <- c()
final.errors <- c()
removed.quotes <- c()

# if a string in the post matches a string in another post
# which was posted on a date which matches any of the dates
# listed in quote.dates and the post does not contain a quote
# -> remove the string

for (i in 1:10) {
  print(paste0('Progess: ~', round(i / length(has.quote), digits = 2) * 100, '%'))
  
  post <- forum$text.split[i]
  post.len <- length(forum$text.split[i][[1]])
  
  for (s in 1:post.len) {
    string <- forum$text.split[i][[1]][s]
    
    posts <- forum$text.split
    filter <- which(as.numeric(forum$date) %in% forum$quote.dates[[i]] & forum$n.quotes == 0)
    #which.errors <- -c(str.detect.errors, gsub.errors)
    filtered.posts <- posts[filter]

    #ids <- forum$id[match.dates]
    #ids <- ids[-which.errors]

    if (
      any(
        grepl(
          forum$text.split[i][[1]][s],
          filtered.posts,
          fixed = TRUE, useBytes = TRUE
        )
      )
    ) {
      print(paste('Removing string', s, "of", post.len))
      forum$text.split[i][[1]][s] <- ""
      removed.quotes <- append(removed.quotes, i)
    }
  }
}


# start -------------------------------------------------------------------


# if error -> store error in vector and skip iteration
if(inherits(possibleError, "error")) {
  match.post.errors <- append(match.post.errors, i)
  print(paste('str_detect error at row: ', i))
  next
}

possibleError <- tryCatch(
  if (!is.na(forum$quote.dates[i]) & !is.null(string) & exists(string)) {
    if (any(str_detect(forum$text.split[which(as.numeric(forum$date) %in% forum$quote.dates[[i]] & forum$n.quotes == 0)] %>% .[-str.detect.errors] %>% .[-gsub.errors], string))) {
      forum$text.split[i][[1]][s] <- gsub(forum$text.split[i][[1]][s], "", perl = TRUE)
      removed.quotes <- append(removed.quotes, i)
      
      print('Success')
    }
  }, error = function(e) e
)

# if error -> store error in vector and skip iteration
if(inherits(possibleError, "error")) {
  final.errors <- append(final.errors, i)
  print(paste('final.errors error at row: ', i))
  next
  
  
  
  if (any(str_detect(forum$text.split[which(as.numeric(forum$date) %in% forum$quote.dates[[i]] & forum$n.quotes == 0)] %>% .[-str.detect.errors] %>% .[-gsub.errors], string))) {
    forum$text.split[i][[1]][s] <- gsub(forum$text.split[i][[1]][s], "", perl = TRUE)
    removed.quotes <- append(removed.quotes, i)
    
    print('Success')
    

# end ---------------------------------------------------------------------

    

str_which(forum$text.split[which(as.numeric(forum$date) %in% forum$quote.dates[[2]] & forum$n.quotes == 0)] %>% .[-str.detect.errors] %>% .[-gsub.errors], forum$text.split[2][[1]][1])


# if a string in the post matches a string in another post
# which was posted on a date which matches any of the dates
# listed in quote.dates and the post does not contain a quote
# -> remove the string


remove.quotes <- function(row, string) {
  print(paste0('Progress: ', row, '/', nrow(forum)))
  
  posts <- forum$text.split
  post <- forum$text.split[[row]]
  string.in.post <- post[string]
  
  return(
    tryCatch(
      if (any(str_detect(posts[which(as.numeric(forum$date[forum$n.quotes == 0]) %in% forum$quote.dates[[row]] & -c(str.detect.errors, gsub.errors))], string.in.post))) {
        string.in.post <- gsub(string.in.post, "", perl = TRUE)
        removed.quotes <- append(removed.quotes, row)
      }, error = function(e) append(remove.quotes.errors, row)
    )
  )
}

removed.quotes <- c()
remove.quotes.errors <- c()

start <- Sys.time()
forum <- lapply(seq_along(forum$text.split[sample(has.quote, 50)]), function(i) lapply(1:length(forum$text.split[[i]]), function(s) remove.quotes(row = i, string = s)))
print(paste("Time elapsed:", integer(Sys.time() - start), 'sec'))

str_detect(forum$text.split[[2]][which(as.numeric(forum$date[forum$n.quotes == 0]) %in% forum$quote.dates[[2]] & -c(str.detect.errors, gsub.errors))], forum$text.split[[2]][1])

lapply(forum$quote.dates, function(x) sapply(x, as.numeric))

lapply(seq_along(x), function(i) paste(names(x)[[i]], x[[i]]))


"to" %in% list(c("en", "to", "tre"), c("fire", "fem"))
