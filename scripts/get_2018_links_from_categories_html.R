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
  
  category <- str_replace(file_to_look_at, "inputs/2018/lists/lists_by_categories/", "") %>% 
    str_replace(".htm", "")
  
  return(tibble(link = link, text = text, category = category))
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


the_data$product <- str_replace_all(the_data$link, c("https://www.paspaley.com/jewellery/pearl-accessories/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-bracelets/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-clasps/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-earrings/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-necklaces/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-rings/" = ""))

the_data$year = 2018

write_csv(the_data, "outputs/misc/2018_links_from_categories.csv")
