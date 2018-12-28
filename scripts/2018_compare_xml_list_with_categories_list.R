library(tidyverse)

list_based <- read_csv("outputs/products_list.csv") %>% 
  mutate(which_are_you_in = "list")
xml_based <- read_csv("outputs/products_xml.csv") %>% 
  mutate(whichd_are_you_in = "xml")

xml_based_test <- xml_based %>% 
  left_join(list_based)


list_based_test <- list_based %>% 
  left_join(xml_based)

# So the list based one is missing:
# baroque-strand
# gold-keshi-rhapsody
# keshi-rope-strand-of-the-harvest
# monsoon-earring-enhancer-l-r-white-gold
# monsoon-earring-enhancer-l-r-yellow-gold
# monsoon-earring-enhancer-left-white-gold
# monsoon-earring-enhancer-left-yellow-gold
# monsoon-earring-enhancer-right-white-gold
# monsoon-earring-enhancer-right-yellow-gold
# petite-circle-strand





library(tidyverse)

list_based <- read_csv("outputs/products_list_categories.csv") %>% 
  mutate(which_are_you_in = "list")
xml_based <- read_csv("outputs/products_xml.csv") %>% 
  mutate(whichd_are_you_in = "xml")

xml_based_test <- xml_based %>% 
  left_join(list_based)


list_based_test <- list_based %>% 
  left_join(xml_based)

