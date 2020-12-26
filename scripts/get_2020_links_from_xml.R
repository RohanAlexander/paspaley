library(rvest)
library(tidyverse)

content <- read_html("inputs/2020/sitemap.xml")

each_product <- content %>%
  xml_find_all("//url") # Instead of HTML/CSS here we're using XML. It's fairly similar, but just a slightly different syntax. Each product is within a url tag.

all_products <- 
  data_frame(
    product_link = html_node(each_product, "loc") %>% html_text(trim = TRUE), # We're first trying to get the URL for each product
    product_name_existence_test = html_node(each_product, "image") %>% html_text(trim = TRUE), # There's a bunch of URLs that aren't products so we want to test for whether there's a name first
    product_name = html_node(each_product, "image") %>% html_node("title") %>% html_text(trim = TRUE), # We're first trying to get the URL for each product
  )


all_products <- 
  all_products %>% 
  filter(!is.na(product_name_existence_test)) %>% # Drop it if not a product
  select(-product_name_existence_test)

write_csv(all_products, "outputs/misc/2020_links_from_xml.csv")
