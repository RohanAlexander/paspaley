library(tidyverse)

the_data <- read_csv("outputs/2018_dataset_cleaned.csv")

ggplot(data = the_data, aes(x = price)) +
  geom_histogram(binwidth = 1000) +
  facet_wrap(vars(collection), nrow = 9)

ggplot(data = the_data, aes(x = price, colour = collection)) +
  geom_point(y = 1)

ggplot(data = the_data, aes(x = price)) +
  geom_density(aes(fill=factor(collection)), alpha=0.8)

ggplot(data = the_data, aes(x = log(price))) +
  geom_density(aes(fill=factor(collection)), alpha=0.8)

ggplot(data = the_data, aes(collection, log(price))) +
  geom_boxplot()

ggplot(data = the_data, aes(collection, log(price))) +
  geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center')