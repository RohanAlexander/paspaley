library(purrr)
library(rvest)
library(tidyverse)


get_links <- function(file_to_look_at){
  content <- read_html(file_to_look_at) %>% 
    html_nodes(".product-item-info") %>% 
    html_nodes(".product") %>% 
    html_nodes("a")
  
  link <- content %>%
    html_attr("href")
  
  text <- content %>%
    html_text(trim = TRUE)
  
  return(tibble(link = link, text = text))
}


the_files <- c(
  "inputs/2018/lists/lists_by_collection/kimberley.htm",
  "inputs/2018/lists/lists_by_collection/lavalier.htm",
  "inputs/2018/lists/lists_by_collection/maxima.htm",
  "inputs/2018/lists/lists_by_collection/monsoon.htm",
  "inputs/2018/lists/lists_by_collection/rockpool.htm",
  "inputs/2018/lists/lists_by_collection/strand.htm",
  "inputs/2018/lists/lists_by_collection/touchstone.htm"
)


the_data <- purrr::map_df(the_files, get_links)


write_csv(the_data, "outputs/2018_links_from_collections.csv")
