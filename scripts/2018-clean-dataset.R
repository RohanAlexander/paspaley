library(tidyverse)


the_data <- read_csv("outputs/2018_dataset.csv")

# Fix price
the_data$price <- str_replace_all(the_data$price, c("AUD " =  "", "," = "")) %>% 
  as.integer()

# Fix SKU
the_data$sku <- str_replace_all(the_data$sku, c("SKU    " =  "")) 

# Fix collection
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

# Test for keshi
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
the_data$description <- str_replace_all(the_data$description, "18ct white gold", "18kt white gold") # Google says that ct refers to weight and kt refers to purity, so should be 18kt white gold etc
the_data$description <- str_replace_all(the_data$description, "18ct yellow gold", "18kt yellow gold")
the_data$description <- str_replace_all(the_data$description, "pearl  bracelet", "pearl bracelet")
the_data$description <- str_replace_all(the_data$description, "feturing", "featuring")
the_data$description <- str_replace_all(the_data$description, "Papsaley", "Paspaley") # Frankly, I'm suprised this didn't happen more often
the_data$name <- str_replace_all(the_data$name, "Oval  - Yellow Gold", "Oval - Yellow Gold")
the_data$name <- str_replace_all(the_data$name, "Baroque  - White Gold", "Baroque - White Gold")
the_data$name <- str_replace_all(the_data$name, "Oval  - White Gold", "Oval - White Gold")

# Pearl type
triangle pearl

# Save the data
write_csv(the_data, "outputs/2018_dataset_cleaned.csv")
