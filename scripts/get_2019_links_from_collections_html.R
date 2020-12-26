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
  
  collection <- str_replace(file_to_look_at, "inputs/2019/lists/lists_by_collection/", "") %>% 
    str_replace(".html", "")
  
  return(tibble(link = link, text = text, collection = collection))
}


the_files <- c(
  "inputs/2019/lists/lists_by_collection/bespoke.html",
  "inputs/2019/lists/lists_by_collection/kimberley.html",
  "inputs/2019/lists/lists_by_collection/lavalier.html",
  "inputs/2019/lists/lists_by_collection/maxima.html",
  "inputs/2019/lists/lists_by_collection/megisti.html",
  "inputs/2019/lists/lists_by_collection/monsoon.html",
  "inputs/2019/lists/lists_by_collection/rockpool.html",
  "inputs/2019/lists/lists_by_collection/strand.html",
  "inputs/2019/lists/lists_by_collection/touchstone.html"
)


the_data <- purrr::map_df(the_files, get_links)


the_data <- the_data %>% 
  mutate(product = str_replace_all(link, c("https://www.paspaley.com/pearl-collections/pearls-my-way/" = "",
    "https://www.paspaley.com/pearl-collections/the-kimberley-bracelet/" = "",
                                           "https://www.paspaley.com/pearl-collections/lavalier/" = "",
                                           "https://www.paspaley.com/pearl-collections/maxima/" = "",
    "https://www.paspaley.com/pearl-collections/megisti/" = "",
                                           "https://www.paspaley.com/pearl-collections/monsoon/" = "",
                                           "https://www.paspaley.com/pearl-collections/rockpool/" = "",
                                           "https://www.paspaley.com/pearl-collections/strands/" = "",
                                           "https://www.paspaley.com/pearl-collections/touchstone/" = "")),
         year = 2019)

write_csv(the_data, "outputs/misc/2019_links_from_collections.csv")
