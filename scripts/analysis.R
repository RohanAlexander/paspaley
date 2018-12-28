library(tidyverse)

# Read in the data
the_data <- read_csv("outputs/data/cleaned_dataset.csv")

# Try some plots
ggplot(data = the_data, aes(x = price)) +
  geom_histogram(binwidth = 1000) +
  facet_wrap(vars(collection), nrow = 9)

ggplot(data = the_data, aes(x = price, colour = collection)) +
  geom_point(y = 1)

ggplot(data = the_data, aes(x = price)) +
  geom_density(aes(fill=factor(collection)), alpha=0.8)

ggplot(data = the_data, aes(x = log(price))) +
  geom_density(aes(fill=factor(collection)), alpha=0.8) +
  facet_wrap(vars(year), nrow = 2)

ggplot(data = the_data, aes(collection, log(price))) +
  geom_boxplot() +
  facet_wrap(vars(year), nrow = 2)

# Try some summary statistics
# Counts and average price by collection by year
the_data %>% 
  group_by(year, collection) %>% 
  summarise(average = mean(price) %>% as.integer(),
            maximum = max(price) %>% as.integer(),
            minimum = min(price) %>% as.integer(),
            number = n())

the_data %>% 
  group_by(year, category) %>% 
  summarise(average = mean(price) %>% as.integer(),
            maximum = max(price) %>% as.integer(),
            minimum = min(price) %>% as.integer(),
            number = n())

# Try comparing the prices for the products that are the same
price_comparison <- the_data %>% 
  select(sku, collection , category, year, price) %>% 
  group_by(sku) %>% 
  spread(key = year, value = price) %>% 
  rename(price_in_2017 = `2017`, price_in_2018 = `2018`)

price_comparison <- price_comparison[complete.cases(price_comparison), ] %>% 
  mutate(change = ((price_in_2018 - price_in_2017) / price_in_2017) * 100,
         change = round(change, digits = 2))

the_2018_names <- the_data %>% 
  filter(year == 2018) %>% 
  select(product, sku)

price_comparison <- price_comparison %>% 
  left_join(the_2018_names)

rm(the_2018_names)

price_comparison %>% 
  arrange(change) %>% 
  ggplot(aes(y = product, x = change)) +
  geom_point()
