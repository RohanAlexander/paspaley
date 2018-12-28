library(purrr)
library(rvest)
library(tidyverse)


get_links <- function(file_to_look_at){
  # file_to_look_at <- "inputs/2017/lists/lists_by_collections/maxima.htm"
  
  content <- read_html(file_to_look_at) %>% 
    html_nodes(".collection-products") %>% 
    html_nodes(".product-list-item-title") 
  
  link <- content %>%
    html_nodes("a") %>% 
    html_attr('href')

  text <- content %>%
    html_text(trim = TRUE)
  
  collection <- str_replace(file_to_look_at, "inputs/2017/lists/lists_by_collections/", "") %>% 
    str_replace(".htm", "")
  
  return(tibble(link = link, text = text, collection = collection))
}


the_files <- c(
  "inputs/2017/lists/lists_by_collections/kimberley.htm",
  "inputs/2017/lists/lists_by_collections/lavalier.htm",
  "inputs/2017/lists/lists_by_collections/maxima.htm",
  "inputs/2017/lists/lists_by_collections/pearls_my_way.htm",
  "inputs/2017/lists/lists_by_collections/rockpool.htm",
  "inputs/2017/lists/lists_by_collections/touchstone.htm"
)


the_data <- purrr::map_df(the_files, get_links)

the_data$product <- str_replace_all(the_data$link, c("/collections/the-kimberley-bracelet/products/" = "",
                                                     "/collections/lavalier/products/" = "",
                                                     "/collections/maxima/products/" = "",
                                                     "/collections/pearls-my-way/products/" = "",
                                                     "/collections/rockpool/products/" = "",
                                                     "/collections/signature-collections/products/" = "",
                                                     "/products/" = ""
                                                     ))

the_data$year = 2017

write_csv(the_data, "outputs/misc/2017_links_from_collections.csv")
