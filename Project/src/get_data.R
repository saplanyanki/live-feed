#COMMODITIES DATA
#Get todays date and do replacements
today <- Sys.Date()
today <- sub(" UTC", "", today)
today.1 <- sub(" UTC", "", today)
today <- gsub("-", "", today)
API_KEY <- "EysB_6_3uAgy6jgSzY5-"

Quandl.api_key(API_KEY)


Gold <- Quandl("LBMA/GOLD")
colnames(Gold) <- c('Dates','USD','USD1', 'GBP','GBP1','EURO','EURO1')
Gold <- subset(Gold, select = c(Dates, USD))
Oil <- Quandl("OPEC/ORB")

require(tree)

#OIL
#model
oil.tree <- rpart(Value ~., data=Oil, control=rpart.control(cp=.0001))

#predict
oil.pred <- predict(oil.tree, newdata=Oil)

#Accuracy
perf.tree <- data.frame(RMSE= RMSE(oil.pred, Oil$Value))

Oil[nrow(Oil) + 1,] = c(today.1,0)
oil.pred <- predict(oil.tree, newdata=Oil)
last_row = tail(oil.pred, n =1)


#CRYPTO DATA

#Get crypto
coins <- crypto_list(only_active=TRUE)
coins <- coins %>%
  filter(rank <= 10)

#list crypto
coin_history <- crypto_history(coins, limit = '10', start_date="20210101", end_date=today, interval ="weekly")
coin_history_interval <- crypto_history(coins, limit = '10', start_date="20200101", end_date=today, interval ="monthly")

#change scientific to numeric
coin_history$open <- round(coin_history$open, 4)
coin_history$high <- round(coin_history$high, 4)
coin_history$close <- round(coin_history$close, 4)
coin_history$low <- round(coin_history$low, 4)
coin_history$volume <- round(coin_history$volume, 4)

#change scientific to numeric
coin_history_interval$open <- round(coin_history_interval$open, 4)
coin_history_interval$high <- round(coin_history_interval$high, 4)
coin_history_interval$close <- round(coin_history_interval$close, 4)
coin_history_interval$low <- round(coin_history_interval$low, 4)
coin_history_interval$volume <- round(coin_history_interval$volume, 4)
coin_history_interval$timestamp <- format(as.POSIXct(coin_history_interval$timestamp,format='%m/%d/%Y %H:%M:%S'),format='%Y%m/%d/')
coin_history_interval <- coin_history_interval %>%
  group_by(timestamp)
  #sub(" UTC", "", timestamp)
  #gsub("-", "", timestamp)


coin_history.2 <- coin_history_interval
coin_history.2$today <- max(coin_history.2$time_open)
coin_history.2$yesterday <- as.Date(coin_history.2$today) - as.difftime(1, unit="days")
coin_history.2$time_high <- format(as.POSIXct(coin_history.2$time_high,format='%Y%m/%d/ %H:%M:%S'),format='%Y%m/%d')

#Get the latest date
coin_history <- coin_history %>%
  group_by(name)

coin_history <- subset(coin_history, name != "Tether")

#Bitcoin
bitcoin.history <- subset(coin_history, name == 'Bitcoin')
bitcoin.history$timestamp <- format(as.POSIXct(bitcoin.history$timestamp,format='%Y-%m-%d/ %H:%M:%S'),format='%Y-%m-%d')

#Ethereum
eth.history <- subset(coin_history, name == 'Ethereum')
eth.history$timestamp <- format(as.POSIXct(eth.history$timestamp,format='%Y-%m-%d/ %H:%M:%S'),format='%Y-%m-%d')

#Cardano
ada.history <- subset(coin_history, name == 'Cardano')
ada.history$timestamp <- format(as.POSIXct(ada.history$timestamp,format='%Y-%m-%d/ %H:%M:%S'),format='%Y-%m-%d')

#Solana
sol.history <- subset(coin_history, name == 'Solana')
sol.history$timestamp <- format(as.POSIXct(sol.history$timestamp,format='%Y-%m-%d/ %H:%M:%S'),format='%Y-%m-%d')


#Get them into separate data tables
btc <- subset(coin_history, name == 'Bitcoin')
xrp <- subset(coin_history, name == 'XRP')
eth <- subset(coin_history, name == 'Ethereum')
ada <- subset(coin_history, name == 'Cardano')
sol <- subset(coin_history, name == 'Solana')
ava <- subset(coin_history, name == 'Avalanche')


#In depth manipulation
bitcoin.history.max.gauge <- bitcoin.history %>%
  filter(timestamp == max(timestamp))

eth.history.max.gauge <- eth.history %>%
  filter(timestamp == max(timestamp))

ada.history.max.gauge <- ada.history %>%
  filter(timestamp == max(timestamp))

sol.history.max.gauge <- sol.history %>%
  filter(timestamp == max(timestamp))


#Web Scraping Manipulation

# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(newsdf$Description)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

# create a document term matrix to clean
reviewsCorpus <- Corpus(VectorSource(newsdf$Description)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)
custom_stop_words <- tibble(word = c("economy", "the", "and", "for", "how"))

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}

###Another way of doing Sentiment Analysis
text_df <- tibble(id_review = newsdf$Source , text_review = newsdf$Description)
text_df <- text_df %>%  unnest_tokens(word, text_review)

data(stop_words)
#data(afinn)
text_df <- text_df %>% 
  anti_join(stop_words, "word")


Sentiment_Analysis <- text_df %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(id_review, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)


head(Sentiment_Analysis)%>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


Sentiment_Analysis_Word_Count <- text_df %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

term_frequency_review <- text_df %>% count(word, sort = TRUE)
term_frequency_review$total_words <- as.numeric(term_frequency_review %>% summarize(total = sum(n)))
term_frequency_review$document <- as.character("Review")
term_frequency_review <- term_frequency_review %>% 
  bind_tf_idf(word, document, n)

###Another way of doing Sentiment Analysis - for CRYPTO
crypto_df <- tibble(id_review = cryptonews$Source , text_review = cryptonews$Description)
crypto_df <- crypto_df %>%  unnest_tokens(word, text_review)

data(stop_words)
#data(afinn)
crypto_df <- crypto_df %>% 
  anti_join(stop_words, "word")


Sentiment_Analysis.crypto <- crypto_df %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(id_review, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

head(Sentiment_Analysis.crypto)%>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


Sentiment_Analysis_Word_Count.crypto <- crypto_df %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

term_frequency_review.crypto <- crypto_df %>% count(word, sort = TRUE)
term_frequency_review.crypto$total_words <- as.numeric(term_frequency_review.crypto %>% summarize(total = sum(n)))
term_frequency_review.crypto$document <- as.character("Review")
term_frequency_review.crypto <- term_frequency_review.crypto %>% 
  bind_tf_idf(word, document, n)


###Another way of doing Sentiment Analysis - for COMMODITIES
commoditynews_df <- tibble(id_review = commoditynews$Source , text_review = commoditynews$Description)
commoditynews_df <- commoditynews_df %>%  unnest_tokens(word, text_review)

data(stop_words)
#data(afinn)
commoditynews_df <- commoditynews_df %>% 
  anti_join(stop_words, "word")


Sentiment_Analysis.commodity <- commoditynews_df %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(id_review, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

head(Sentiment_Analysis.commodity)%>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


Sentiment_Analysis_Word_Count.commodity <- commoditynews_df %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

term_frequency_review.commodity <- commoditynews_df %>% count(word, sort = TRUE)
term_frequency_review.commodity$total_words <- as.numeric(term_frequency_review.commodity %>% summarize(total = sum(n)))
term_frequency_review.commodity$document <- as.character("Review")
term_frequency_review.commodity <- term_frequency_review.commodity %>% 
  bind_tf_idf(word, document, n)












