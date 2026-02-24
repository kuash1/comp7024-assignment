library(tidyverse)
library(scales)

# Load cleaned data
df <- readRDS("data/orders_joined.rds")

# Calculate revenue per product
product_revenue <- df %>%
  group_by(StockCode, Description) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(TotalRevenue))

# Top 10 products
top10 <- product_revenue %>% slice_head(n = 10)

# Bottom 10 products
bottom10 <- product_revenue %>% slice_tail(n = 10)

# Top 3 revenue share
total_revenue <- sum(product_revenue$TotalRevenue)

top3_share <- product_revenue %>%
  slice_head(n = 3) %>%
  summarise(Top3Percent = sum(TotalRevenue) / total_revenue * 100)

print(top10)
print(bottom10)
print(top3_share)