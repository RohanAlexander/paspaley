library(tidyverse)


# Read in the datasets
the_2018_data <- read_csv("outputs/data/2018_dataset.csv")
the_2017_data <- read_csv("outputs/data/2017_dataset.csv")


# Combine the datasets
the_2018_data$year = 2018
the_2017_data$year = 2017
the_data <- rbind(the_2017_data, the_2018_data)
rm(the_2017_data, the_2018_data)


# Fix price
the_data$price <- str_replace_all(the_data$price, "[\r\n]" , "") %>% 
  str_replace(".*\\$", "") # Get rid of the "Starting from..." ones - we just want a number - this is an issue with some of the 2017 ones
  
the_data$price <- str_replace_all(the_data$price, c("AUD " =  "", "," = "")) %>% 
  as.integer() # Convert to a integer - the 2018 ones start with AUD so remove that first


# Fix SKU
the_data$sku <- str_replace_all(the_data$sku, c("SKU    " =  "")) # The 2018 ones actually start with SKU...


# Identify the collection
the_data <- the_data %>% 
  mutate(collection = case_when(
    str_detect(product, "kimberley") ~ "Kimberley",
    str_detect(product, "lavalier") ~ "Lavalier",
    str_detect(product, "maxima") ~ "Maxima",
    str_detect(product, "monsoon") ~ "Monsoon",
    str_detect(product, "pearls-my-way") ~ "Pearls My Way",
    str_detect(product, "rockpool") ~ "Rockpool",
    str_detect(product, "strand") ~ "Strand",
    str_detect(product, "touchstone") ~ "Touchstone",
    TRUE ~ "Other"
    ))


# Identify whether keshi pearl
the_data <- the_data %>% 
  mutate(keshi = case_when(
    str_detect(product, "keshi") ~ TRUE,
    str_detect(name, "Keshi") ~ TRUE,
    str_detect(description, "Keshi") ~ TRUE,
    str_detect(description, "keshi") ~ TRUE,
    TRUE ~ FALSE
  ))


# Metal
the_data <- the_data %>% 
  mutate(metal = case_when(
    str_detect(name, "White Gold") ~ "White gold",
    str_detect(name, "Yellow Gold") ~ "Yellow gold",
    str_detect(name, "Rose Gold") ~ "Rose gold",
    str_detect(name, "Platinum") ~ "Platinum",
    str_detect(description, "white gold") ~ "White gold",
    str_detect(description, "yellow gold") ~ "Yellow gold",
    str_detect(description, "rose gold") ~ "Rose gold",
    str_detect(description, "platinum") ~ "Platinum",
    TRUE ~ "Other"
  ))


# Type
the_data <- the_data %>% 
  mutate(type = case_when(
    str_detect(product, "accessories") ~ "Accessory",
    str_detect(product, "bracelets") ~ "Bracelet",
    str_detect(product, "clasps") ~ "Clasp",
    str_detect(product, "earrings") ~ "Earrings",
    str_detect(product, "necklaces") ~ "Necklace",
    str_detect(product, "rings") ~ "Ring",
    str_detect(product, "petite-circle-strand") ~ "Necklace",
    str_detect(product, "baroque-strand") ~ "Necklace",
    str_detect(product, "gold-keshi-rhapsody") ~ "Necklace",
    str_detect(product, "keshi-rope-strand-of-the-harvest") ~ "Necklace",
    TRUE ~ "Other"
  ))

# Fix typos and inconsistencies
the_data$description <- str_replace_all(the_data$description, "in18kt", "in 18kt")
the_data$description <- str_replace_all(the_data$description, "18ct white gold", "18kt white gold") # Google says that ct refers to weight and kt refers to purity, so should be 18kt white gold etc?
the_data$description <- str_replace_all(the_data$description, "18ct yellow gold", "18kt yellow gold")
the_data$description <- str_replace_all(the_data$description, "pearl  bracelet", "pearl bracelet")
the_data$description <- str_replace_all(the_data$description, "feturing", "featuring")
the_data$description <- str_replace_all(the_data$description, "Papsaley", "Paspaley") # Frankly, I'm suprised this didn't happen more often
the_data$name <- str_replace_all(the_data$name, "Oval  - Yellow Gold", "Oval - Yellow Gold")
the_data$name <- str_replace_all(the_data$name, "Baroque  - White Gold", "Baroque - White Gold")
the_data$name <- str_replace_all(the_data$name, "Oval  - White Gold", "Oval - White Gold")
the_data$description <- str_replace_all(the_data$description, "extrodinary", "extraordinary")
the_data$description <- str_replace_all(the_data$description, "feauturing", "featuring")
the_data$description <- str_replace_all(the_data$description, "fourty-eight", "forty-eight")
the_data$description <- str_replace_all(the_data$description, "featruing", "featuring")
the_data$description <- str_replace_all(the_data$description, "fotry-eight", "forty-eight")
the_data$description <- str_replace_all(the_data$description, "suprisingly", "surprisingly")
the_data$description <- str_replace_all(the_data$description, "\\.(?=[:upper:][:lower:])", ". ") # There's a bunch with no space following the full stop at the end of the sentence.
the_data$description <- str_replace_all(the_data$description, "\\.(?=A)", ". ")
the_data$description <- str_replace_all(the_data$description, "Freah", "Fresh")
the_data$description <- str_replace_all(the_data$description, "ninty", "ninety")
the_data$description <- str_replace_all(the_data$description, "Introducing Pearls My Way\\. Tailor your look by choosing your preferred Australian South Sea pearls and earring hooks in your favourite shade of gold. Add some colour to your earrings with a variety of precious gemstones including rubies and diamonds\\. Prefer to create your own Pearls My Way earrings\\? Contact the Personal Shopper or visit your nearest boutique. Personal Shopper Recommendation:", "")
the_data$description <- str_replace_all(the_data$description, "Introducing Pearls My Way\\. Tailor your look by choosing your preferred Australian South Sea pearls and earring hooks in your favourite shade of gold\\. Add some colour to your earrings with a variety of precious gemstones including rubies and diamonds\\. Prefer to create your own Pearls My Way earrings\\? Contact Personal Shopper or visit your nearest boutique\\. Personal Shopper Recommendation:", "")


# Pearl type
triangle pearl

# Save the data
write_csv(the_data, "outputs/data/cleaned_dataset.csv")
