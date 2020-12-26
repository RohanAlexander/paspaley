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
  
  collection <- str_replace(file_to_look_at, "inputs/2020/lists/lists_by_collection/", "") %>% 
    str_replace(".html", "")
  
  return(tibble(link = link, text = text, collection = collection))
}


the_files <- c(
  "inputs/2020/lists/lists_by_collection/bespoke.htm",
  "inputs/2020/lists/lists_by_collection/kimberley.htm",
  "inputs/2020/lists/lists_by_collection/lavalier.htm",
  "inputs/2020/lists/lists_by_collection/maxima.htm",
  "inputs/2020/lists/lists_by_collection/megisti.htm",
  "inputs/2020/lists/lists_by_collection/monsoon.htm",
  "inputs/2020/lists/lists_by_collection/rockpool.htm",
  "inputs/2020/lists/lists_by_collection/strand.htm",
  "inputs/2020/lists/lists_by_collection/touchstone.htm"
)


the_data <- 
  purrr::map_df(the_files, get_links)


the_data <- the_data %>% 
  mutate(product = 
           str_replace_all(link, 
                           c("https://www.paspaley.com/pearl-collections/pearls-my-way/" = "",
    "https://www.paspaley.com/pearl-collections/the-kimberley-bracelet/" = "",
                                           "https://www.paspaley.com/pearl-collections/lavalier/" = "",
                                           "https://www.paspaley.com/pearl-collections/maxima/" = "",
    "https://www.paspaley.com/pearl-collections/megisti/" = "",
                                           "https://www.paspaley.com/pearl-collections/monsoon/" = "",
                                           "https://www.paspaley.com/pearl-collections/rockpool/" = "",
                                           "https://www.paspaley.com/pearl-collections/strands/" = "",
                                           "https://www.paspaley.com/pearl-collections/touchstone/" = "")),
         year = 2020)

write_csv(the_data, "outputs/misc/2020_links_from_collections.csv")
