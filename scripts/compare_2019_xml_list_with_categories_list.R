library(tidyverse)

categories_based <- read_csv("outputs/misc/2019_links_from_categories.csv") %>% 
  select(product)
collections_based <- read_csv("outputs/misc/2019_links_from_collections.csv") %>% 
  select(product)
xml_based <- read_csv("outputs/misc/2019_links_from_xml.csv") %>% 
  rename(product = product_link) %>% 
  select(product) %>% 
  mutate(product = str_remove(product, "https://www.paspaley.com/jewellery/"),
         product = str_remove(product, "https://www.paspaley.com/pearl-collections/"))

setdiff(xml_based, categories_based) # in XML, but not in Categories
setdiff(collections_based, categories_based) # in Collections, but not in Categories

# Missing
baroque-strand
gold-keshi-rhapsody
keshi-rope-strand-of-the-harvest
lavalier
pearls-my-way
pendants-my-way
petite-circle-pearl-strand
rhapsody-keshi-and-circle-strand-necklace
rhapsody-strand-bracelet
round-pearl-strand-bracelet
round-pearl-strand-necklace

