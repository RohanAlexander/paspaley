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
  "inputs/2018/lists/lists_by_categories/accessories.htm",
  "inputs/2018/lists/lists_by_categories/bracelets.htm",
  "inputs/2018/lists/lists_by_categories/clasps.htm",
  "inputs/2018/lists/lists_by_categories/earrings.htm",
  "inputs/2018/lists/lists_by_categories/necklaces.htm",
  "inputs/2018/lists/lists_by_categories/rings.htm"
)


the_data <- purrr::map_df(the_files, get_links)


write_csv(the_data, "outputs/2018_links.csv")
