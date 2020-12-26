library(tidyverse)

categories_based <- read_csv("outputs/misc/2020_links_from_categories.csv")
collections_based <- read_csv("outputs/misc/2020_links_from_collections.csv")
xml_based <- read_csv("outputs/misc/2020_links_from_xml.csv") 

collections_based <- 
  collections_based %>% 
  mutate(not_in_categories = !product %in% categories_based$product) %>% 
  filter(not_in_categories == TRUE)

xml_based <- 
  xml_based %>% 
  mutate(product = product_link, 
         product = str_remove(product, "https://www.paspaley.com/jewellery/"),
         product = str_remove(product, "https://www.paspaley.com/pearl-collections/")
         ) %>% 
  mutate(not_in_categories = !product %in% categories_based$product) %>% 
  filter(not_in_categories == TRUE)

# Follow up on these:
xml_based$product_link
collections_based$link



