library(rvest)
library(tidyverse)
library(heapsofpapers)


the_categories_data <- read_csv("outputs/misc/2021_links_from_categories.csv") %>% 
  select(link, text)
the_collections_data <- read_csv("outputs/misc/2021_links_from_collections.csv") %>% 
  select(link, text)
the_xml_data <- read_csv("outputs/misc/2021_links_from_xml.csv") %>% 
  rename(text = product_name,
         link = product_link)

all <- rbind(the_categories_data, the_xml_data)

all <- 
  all %>% 
  arrange(text) %>% 
  distinct(text, .keep_all = TRUE)

all$product_name <- 
  str_replace_all(all$link, "https://www.paspaley.com/jewellery/", "")
all$product_name <- 
  str_replace_all(all$product_name, "/", "-")
all$name_of_file <- paste0(all$product_name, ".html") 

heapsofpapers::get_and_save(
  data = all,
  links = "link",
  save_names = "name_of_file",
  dir = "inputs/2021/products"
)


# Clean up
rm(i, name_of_file, product_data, product_name)


# Throws an error:
# https://www.paspaley.com/jewellery/pearl-necklaces/oval-bella-petite-pendant-white-gold


# Based on compare_2021_xml_list_with_categories_list.R we also need to go and get these ones:

get_me_alsoasdf <- 
  get_me_also %>% 
  mutate(
    product_name = link,
    product_name = str_remove(product_name, "https://www.paspaley.com/jewellery/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/touchstone/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/megisti/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/rockpool/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/maxima/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/dive/"),
    
    product_name = str_remove(product_name, "https://www.paspaley.com/catalog/product/view/id/4004/s/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/catalog/product/view/id/3336/s/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/catalog/product/view/id/3116/s/"),
    
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/catalog/product/view/id/"),
    product_name = str_remove(product_name, "https://www.paspaley.com/pearl-collections/"),
    
    product_name = str_remove(product_name, "&apos;"),
    
    
      
    product_name = str_replace_all(product_name, "/", "-"),
    name_of_file = paste0(product_name, ".html") 
  )


heapsofpapers::get_and_save(
  data = get_me_alsoasdf,
  links = "link",
  save_names = "name_of_file",
  dir = "inputs/2021/products"
)








