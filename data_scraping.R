# Libraries ####
library(jsonlite)

# Importing card data ####

# Using latest card data
card_data_raw <- fromJSON("https://api.hearthstonejson.com/v1/latest/enUS/cards.json")


# Saving data ####
# Save as .RData files since it's not coercible to a matrix
save(card_data_raw, file="./Data/Raw/latest_cards_raw.RData")

