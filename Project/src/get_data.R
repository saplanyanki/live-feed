#COMMODITIES DATA
API_KEY <- "EysB_6_3uAgy6jgSzY5-"

Quandl.api_key(API_KEY)


Gold <- Quandl("LBMA/GOLD")
Oil <- Quandl("OPEC/ORB")



#CRYPTO DATA

#Get todays date and do replacements
today <- Sys.Date()
today <- sub(" UTC", "", today)
today <- gsub("-", "", today)

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




