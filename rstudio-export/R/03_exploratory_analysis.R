# ---- packages ----
library(tidyverse)

# ---- load clean data ----
clean_path <- "data/clean/airbnb_clean.csv"
airbnb <- readr::read_csv(clean_path, show_col_types = FALSE)

# ---- quick overview ----
glimpse(airbnb)
summary(airbnb$price)

# ---- Average price by neighbourhood group ----
avg_price_group <- airbnb |>
  group_by(neighbourhood_group) |>
  summarize(
    avg_price = mean(price, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(avg_price))

print(avg_price_group)

# ---- Plot 1: Average price by neighbourhood group ----
ggplot(avg_price_group, aes(
  x = reorder(neighbourhood_group, avg_price),
  y = avg_price,
  fill = neighbourhood_group
)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average Airbnb Price by Borough",
    x = "Borough",
    y = "Average Price (USD)"
  ) +
  theme_minimal(base_size = 13)

# ---- Plot 2: Price distribution by room type ----
ggplot(airbnb, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Price Distribution by Room Type",
    x = "Room Type",
    y = "Price (USD per night)"
  ) +
  theme_minimal(base_size = 13)

# ---- Plot 3: Rating vs Price ----
ggplot(airbnb, aes(x = review_scores_rating, y = price)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Relationship Between Rating and Price",
    x = "Review Score Rating",
    y = "Price (USD)"
  ) +
  theme_minimal(base_size = 13)
