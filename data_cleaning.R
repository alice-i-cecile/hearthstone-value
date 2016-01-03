# Libraries ####
library(stringr)

# Loading data ####
load("./Data/Raw/latest_cards_raw.RData")
cards <- card_data_raw

# Removing invalid records ####

# Remove heroes and hero powers
cards <- cards[!(cards$type %in% c("HERO", "HERO_POWER")),]

# Removing unneeded fields ####

# flavor is flavor
# artist is flavor
# collectible has already been cleaned for
# howToEarn is collection information
# howToEarnGolden is collection information
# playRequirements is internal
# faction is internal
# entourage is internal
# targetingArrowText is internal
# textInPlay is flavor

cards <- subset(cards, select=-c(flavor,
                                 artist,
                                 collectible,
                                 howToEarn,
                                 howToEarnGolden,
                                 playRequirements,
                                 faction,
                                 entourage,
                                 targetingArrowText,
                                 textInPlay))


# Converting mechanics into a regression friendly format ####

# Creating a column for text that has yet to be explained by the model
cards$unexplained_text <- cards$text
cards$unexplained_text[is.na(cards$unexplained_text)] <- ""
cards$unexplained_text <- str_replace_all(cards$unexplained_text, "\\n", "")

# Adding columns for each mechanic flag
mechanics_list <- unique(unlist(cards$mechanics))
mechanics_dummy_df <- matrix(FALSE, nrow = 1, ncol= length(mechanics_list), dimnames = list(1, mechanics_list))
cards <- cbind(cards, mechanics_dummy_df)

# Populating mechanics columns with boolean flags
for (i in 1:nrow(cards)){
    flags <- unlist(cards$mechanics[i])
    cards[i, flags] <- TRUE
}

# Targeting paradigms ####

# Numeric

# Random

# All


# Minions

# Heroes

# Both


# Self

# Friendly only

# Enemy only

# Both

# Conditional mechanics ####

# Links to a card ID for the effect that is created

# Battlecry ##

# Deathrattle ##

# Enrage ##

# Tribal (on board) ##

# Tribal (in hand) ##

# Only affects tribals ###

# Affects all but tribals ##

# Secrets ##

# Joust ##

# Shuffle into deck ##

# When your opponent casts a spell ##

# When you cast a spell ##

# When you play a card ##

# When a minion is healed ##

# When anything is healed ##

# Whenever a friendly minion dies ####

# When this minion deals damage ####

# Choose one ##

# Self mechanics ####

# Divine shield ##
cards$unexplained_text[cards$DIVINE_SHIELD] <- str_replace_all(cards$unexplained_text[cards$DIVINE_SHIELD], "<b>Divine Shield</b>", "")

# Stealth ##
cards$unexplained_text[cards$STEALTH] <- str_replace_all(cards$unexplained_text[cards$STEALTH], "<b>Stealth</b>", "")

# Taunt ##
cards$unexplained_text[cards$TAUNT] <- str_replace_all(cards$unexplained_text[cards$TAUNT], "<b>Taunt</b>", "")

# Charge ##
cards$unexplained_text[cards$CHARGE] <- str_replace_all(cards$unexplained_text[cards$CHARGE], "<b>Stealth</b>", "")

# Windfury ##
cards$unexplained_text[cards$WINDFURY] <- str_replace_all(cards$unexplained_text[cards$WINDFURY], "<b>Windfury</b>", "")

# Poisonous ##
cards$unexplained_text[cards$POISONOUS] <- str_replace_all(cards$unexplained_text[cards$POISONOUS], "Destroy any minion damaged by this minion.", "")

# Forgetful ##

# Fix missing flags
cards$FORGETFUL[which(str_detect(cards$text, "50% chance to attack the wrong enemy."))] <- TRUE 

# Should be handled as a buff
cards[which(cards$name=="Mogor the Ogre"), "FORGETFUL"] <- FALSE # Missing flag

cards$unexplained_text[cards$FORGETFUL] <- str_replace_all(cards$unexplained_text[cards$FORGETFUL], "50% chance to attack the wrong enemy.", "")

# Spell power ##

# Fix missing flags
cards$SPELLPOWER[which(str_detect(cards$name, "Velen's Chosen"))] <- TRUE

# Replace FALSE with 0 spellpower
cards$SPELLPOWER[cards$SPELLPOWER == FALSE] <- 0

# Extract spellpower values
cards$SPELLPOWER[cards$SPELLPOWER] <- str_extract(
                                        str_extract(cards$text[cards$SPELLPOWER], "Spell Damage \\+[0-9]+"), 
                                      "[0-9]+")

# Remove explained text
cards$unexplained_text[cards$SPELLPOWER] <- str_replace_all(cards$unexplained_text[cards$SPELLPOWER], "Spell Damage \\+[0-9]+", "")

# Untargetable ##

# Overload ##

# Gain mana crystals this turn ##

# Permanently change mana crystals ##

# Targeted mechanics ####

# Draw cards ##

# Discover ##

# Gain random cards ##

# Damage ##

# Healing ##

# Stat buffs ##

# Mechanic buffs ##

# Bounce ##

# Set attack to 1 ##

# Set health to 1 ##

# Freeze ##

# Destroy ##