library(purrr)
library(rvest)
library(tidyverse)


get_links <- function(file_to_look_at){
  # file_to_look_at <- "inputs/2017/lists/lists_by_categories/bracelets.htm"
  
  content <- read_html(file_to_look_at) %>% 
    html_nodes(".collection-products") %>% 
    html_nodes(".product-list-item") %>% 
    html_nodes("a")
  
  link <- content %>%
    html_attr('href')
  
  text <- content %>%
    html_text(trim = TRUE)
  
  category <- str_replace(file_to_look_at, "inputs/2017/lists/lists_by_categories/", "") %>% 
    str_replace(".htm", "")
  
  return(tibble(link = link, text = text, category = category))
}


the_files <- c(
  "inputs/2017/lists/lists_by_categories/accessories.htm",
  "inputs/2017/lists/lists_by_categories/bracelets.htm",
  "inputs/2017/lists/lists_by_categories/clasps.htm",
  "inputs/2017/lists/lists_by_categories/earrings.htm",
  "inputs/2017/lists/lists_by_categories/necklaces_and_pendants.htm",
  "inputs/2017/lists/lists_by_categories/rings.htm",
  "inputs/2017/lists/lists_by_categories/strands.htm"
  )


the_data <- purrr::map_df(the_files, get_links)

the_data$product <- str_replace_all(the_data$link, c("/collections/accessories/products/" = "",
                                                     "/collections/bracelets/products/" = "",
                                                     "/collections/clasps/products/" = "",
                                                     "/collections/earrings/products/" = "",
                                                     "/collections/necklaces-pendants/products/" = "",
                                                     "/collections/rings/products/" = "",
                                                     "/collections/strands/products/" = ""))

the_data <- distinct(the_data) %>% 
  filter(text != "Click Here")

the_data$year = 2017

write_csv(the_data, "outputs/misc/2017_links_from_categories.csv")
