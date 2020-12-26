library(rvest)
library(tidyverse)

content <- read_html("inputs/2019/sitemap.xml")

each_product <- content %>%
  xml_find_all("//url") # Instead of HTML/CSS here we're using XML. It's fairly similar, but just a slightly different syntax. Each product is within a url tag.

# Thanks to Jenny Bryan https://github.com/jennybc/manipulate-xml-with-purrr-dplyr-tidyr
all_products <- data_frame(row = seq_along(each_product), # This just adds a row numbering which makes it easier to go back and check.
                      nodeset = each_product) # Now we're pulling the relevant XML for each product into a new column. So for each product (row) the relevant nodes will be in that column.

# Thanks to James Goldie https://rensa.co/writing/drilling-into-non-rectangular-data-with-purrr/
all_products <- all_products %>% 
  mutate(product_link = nodeset %>% # We're first trying to get the URL for each product
           html_node("loc") %>%
           html_text(trim = TRUE)) %>% 
  mutate(product_name_existence_test = nodeset %>% # There's a bunch of URLs that aren't products so we want to test for whether there's a name first
           html_node("image")) %>% 
  filter(!is.na(product_name_existence_test)) %>% # Drop it if not a product
  mutate(product_name = nodeset %>% # Grab the name of the product
           html_node("image") %>% 
           html_node("title") %>% 
           html_text(trim = TRUE)) %>% 
  select(-product_name_existence_test, -nodeset)

write_csv(all_products, "outputs/misc/2019_links_from_xml.csv")


