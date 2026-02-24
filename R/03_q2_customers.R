library(tidyverse)
library(scales)

# Load cleaned data
df <- readRDS("data/orders_joined.rds")

#Remove rows with missing CustomerID (important!)
df <- df %>% filter(!is.na(CustomerID))

#Total spending per customer
customer_spend <- df %>%
  group_by(CustomerID) %>%
  summarise(TotalSpend = sum(Revenue, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(TotalSpend))

#Top 5 customers
top5_customers <- customer_spend %>% slice_head(n = 5)

print(top5_customers)

#What products do these top 5 buy most?
top5_products <- df %>%
  filter(CustomerID %in% top5_customers$CustomerID) %>%
  group_by(CustomerID, Description) %>%
  summarise(TotalQuantity = sum(Quantity, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(CustomerID, desc(TotalQuantity))

print(top5_products)