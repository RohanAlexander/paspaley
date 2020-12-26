library(tidyverse)


# Read in the datasets
the_2017_data <- read_csv("outputs/data/2017_dataset.csv")
the_2018_data <- read_csv("outputs/data/2018_dataset.csv")
the_2019_data <- read_csv("outputs/data/2019_dataset.csv")
the_2020_data <- read_csv("outputs/data/2020_dataset.csv")

# Combine the datasets
the_2017_data$year = 2017
the_2018_data$year = 2018
the_2019_data$year = 2019
the_2020_data$year = 2020

the_data <- 
  rbind(the_2017_data, the_2018_data, the_2019_data, the_2020_data)
rm(the_2017_data, the_2018_data, the_2019_data, the_2020_data)


# Fix price
the_data$price <- str_replace_all(the_data$price, "[\r\n]" , "") %>% 
  str_replace(".*\\$", "") # Get rid of the "Starting from..." ones - we just want a number - this is an issue with some of the 2017 ones
  
the_data$price <- str_replace_all(the_data$price, c("AUD " =  "", "," = "")) %>% 
  as.integer() # Convert to a integer - the 2018 ones start with AUD so remove that first


# Fix SKU
the_data$sku <- str_replace_all(the_data$sku, c("SKU    " =  "")) # The 2018 ones actually start with "SKU" but we just want the number

# 

# Identify the collection
# First start by using the collections in the website
links_by_collection_2019 <- read_csv("outputs/misc/2019_links_from_collections.csv")
links_by_collection_2018 <- read_csv("outputs/misc/2018_links_from_collections.csv")
links_by_collection_2017 <- read_csv("outputs/misc/2017_links_from_collections.csv")
links_by_collection <- 
  rbind(links_by_collection_2017, 
        links_by_collection_2018, 
        links_by_collection_2019) %>% 
  select(collection, product, year)
rm(links_by_collection_2017, 
   links_by_collection_2018, 
   links_by_collection_2019)

the_data <- the_data %>% 
  mutate(name_for_matching = str_replace_all(product, c("pearl-accessories-" = "",
                                                   "pearl-bracelets-" = "",
                                                   "pearl-clasps-" = "",
                                                   "pearl-earrings-" = "",
                                                   "pearl-necklaces-" = "",
                                                   "pearl-rings-" = "")))

the_data <- the_data %>% 
  left_join(links_by_collection, by = c("year" = "year",
                                        "name_for_matching" = "product"))


rm(links_by_collection)

# Now, by looking at the content if it wasn't identified already
the_data <- the_data %>% 
  mutate(collection_brute_force = case_when(
    str_detect(product, "kimberley") ~ "kimberley",
    str_detect(product, "lavalier") ~ "lavalier",
    str_detect(product, "maxima") ~ "maxima",
    str_detect(product, "megisti") ~ "megisti",
    str_detect(product, "monsoon") ~ "monsoon",
    str_detect(product, "pearls-my-way") ~ "pearls_my_way",
    str_detect(product, "rockpool") ~ "rockpool",
    str_detect(product, "strand") ~ "strand",
    str_detect(product, "gold-keshi-rhapsody") ~ "strand", # outlier
    str_detect(product, "touchstone") ~ "touchstone",
    TRUE ~ "other"
    ))

the_data$product <- str_replace(the_data$product, "pavé", "pave")

the_data <- the_data %>% 
  mutate(collection = if_else(is.na(collection), collection_brute_force, collection)) %>% 
  select(-collection_brute_force) 

the_data$collection[the_data$collection == "pearls_my_way"] <- "Pearls My Way"
the_data$collection <- str_to_title(the_data$collection , locale = "en")

the_data$collection <- str_remove(the_data$collection , ".htm")

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


# Category
# Again, as before, looking at the website content first
links_by_categories_2019 <- read_csv("outputs/misc/2019_links_from_categories.csv")
links_by_categories_2019$year <- 2019
links_by_categories_2018 <- read_csv("outputs/misc/2018_links_from_categories.csv")
links_by_categories_2017 <- read_csv("outputs/misc/2017_links_from_categories.csv")
links_by_categories <- 
  rbind(links_by_categories_2017, 
        links_by_categories_2018, 
        links_by_categories_2019) %>% 
  select(category, product, year)
rm(links_by_categories_2017, links_by_categories_2018, links_by_categories_2019)

links_by_categories$category <- str_remove(links_by_categories$category, ".htm")

links_by_categories$category %>% unique()

the_data <- the_data %>% 
  left_join(links_by_categories, by = c("year" = "year",
                                        "name_for_matching" = "product"))

the_data <- the_data %>% 
  mutate(category_brute_force = case_when(
    str_detect(product, "accessories") ~ "Accessories",
    str_detect(product, "bracelets") ~ "Bracelets",
    str_detect(product, "bracelet") ~ "Bracelets",
    str_detect(product, "clasps") ~ "Clasps",
    str_detect(product, "clasp") ~ "Clasps",
    str_detect(product, "earrings") ~ "Earrings",
    str_detect(product, "necklaces") ~ "Necklace",
    str_detect(product, "necklace") ~ "Necklace",
    str_detect(product, "ring") ~ "Rings",
    str_detect(product, "rings") ~ "Rings",
    str_detect(product, "petite-circle-strand") ~ "Necklace",
    str_detect(product, "baroque-strand") ~ "Necklace",
    str_detect(product, "gold-keshi-rhapsody") ~ "Necklace",
    str_detect(product, "keshi-rope-strand-of-the-harvest") ~ "Necklace",
    TRUE ~ "Other"
  ))

the_data <- the_data %>% 
  mutate(category = if_else(is.na(category), category_brute_force, category)) %>% 
  select(-category_brute_force) 

the_data$category[the_data$category == "necklaces_and_pendants"] <- "Necklaces and Pendants"
the_data$category[the_data$category == "Necklace"] <- "Necklaces and Pendants"
the_data$category[the_data$category == "necklaces"] <- "Necklaces and Pendants"
the_data$category[the_data$category == "Bracelet"] <- "Bracelets"
the_data$category[the_data$category == "bracelet"] <- "bracelets"

the_data$category[the_data$sku == "A19B02W"]

the_data$category <- str_to_title(the_data$category , locale = "en")

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

# For some reason some are still duplicated
the_data <- 
  the_data %>% 
  distinct(sku, year, .keep_all = TRUE)

# Pearl type
the_data <- the_data %>% 
  mutate(description_lowered = str_to_lower(description)) %>% 
  mutate(pearl_type = case_when(
    str_detect(description_lowered, "mother-of-pearl") ~ "Mother of pearl",
    str_detect(description_lowered, "mother of pearl") ~ "Mother of pearl",
    str_detect(description_lowered, "triangle pearl") ~ "Triangle",
    str_detect(description_lowered, "round pearl") ~ "Round",
    str_detect(description_lowered, "button pearl") ~ "Button",
    str_detect(description_lowered, "semi-round pearl") ~ "Semi-round",
    str_detect(description_lowered, "drop pearl") ~ "Drop",
    str_detect(description_lowered, "circle pearl") ~ "Circle",
    str_detect(description_lowered, "oval pearl") ~ "Oval",
    str_detect(description_lowered, "round australian south sea pearl") ~ "Round",
    str_detect(description_lowered, "round paspaley pearls") ~ "Round",
    str_detect(description_lowered, "oval drop australian south sea pearls") ~ "Oval",
    str_detect(description_lowered, "oval australian south sea pearl") ~ "Oval",
    str_detect(description_lowered, "button australian south sea pearl") ~ "Button",
    TRUE ~ "Unsure"
  )) %>% 
  select(-description_lowered)

# Combine the description and the flowery description (2018 has the split, but 2017 doesn't)
the_data <- the_data %>% 
  unite(description, description, flowerydescription, sep = " ") 


# Change Bespoke to "Pearls My Way"
the_data$collection[the_data$collection == "Bespoke"] <- "Pearls My Way"

# Check if any SKU is changing collection or category
the_data %>% 
  select(sku, collection) %>% 
  distinct() %>% 
  group_by(sku) %>% 
  count() %>% 
  filter(n>1)

the_data %>% 
  filter(sku %in% c("DC16E10WPQ14", "DC16E10YPO12", "DC18P04YPO13", "DC18P06YPR12", "DC19E08WPC12", "DC19E14YPR12")
         )


the_data$collection[the_data$sku %in% c("DC16E10WPQ14", "DC16E10YPO12", 
                                        "DC18P04YPO13", "DC18P06YPR12", 
                                        "DC19E08WPC12", "DC19E14YPR12")] <- "Touchstone"

  
check <- 
  the_data %>% 
  select(sku, category) %>% 
  distinct() %>% 
  group_by(sku) %>% 
  count() %>% 
  filter(n>1) %>% 
  pull(sku)

check_me <- the_data %>% 
  filter(sku %in% check) %>% 
  arrange(sku) %>% 
  select(product, year, category, sku)

the_data$category %>% table()

the_data$category[the_data$sku == "C00L01YPR15"] <- "Clasps"

the_data$category[the_data$sku %in% c("DH13N01W83C", "DH13N01Y83C", "DH14N20WC", 
                                      "DH14N20YC", "DH16N01RC", "SCPC1012138", 
                                      "W18N06R")] <- "Necklaces And Pendants"

the_data$category[the_data$sku %in% c("W18E06R", "DW18E05RPR13")] <- "Earrings"



# Save the data
write_csv(the_data, "outputs/data/cleaned_dataset.csv")
