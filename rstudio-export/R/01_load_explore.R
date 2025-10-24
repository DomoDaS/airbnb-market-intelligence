# ---- packages ----
library(tidyverse)   # dplyr, readr, ggplot2, etc.

# ---- paths ----
raw_path <- "data/raw/listings.csv.gz"

# ---- load ----
# readr can read .csv.gz directly
listings <- readr::read_csv(raw_path, show_col_types = FALSE)

# Basic shape & columns
dim(listings)
glimpse(listings, width = 80)

# ---- keep only useful columns for market intelligence ----
cols_keep <- c(
  "id", "host_id",
  "neighbourhood_group", "neighbourhood",
  "latitude", "longitude",
  "room_type", "price",
  "minimum_nights", "availability_365",
  "number_of_reviews", "reviews_per_month",
  "review_scores_rating",
  "calculated_host_listings_count",
  "last_review"
)

listings_small <- listings |>
  select(any_of(cols_keep)) |>
  # ensure price is numeric even if it has $ or commas in some cities
  mutate(price = readr::parse_number(as.character(price)))

# Quick sanity checks
listings_small |>
  summarize(
    n_rows = n(),
    n_cols = ncol(cur_data()),
    price_min = min(price, na.rm = TRUE),
    price_p50 = median(price, na.rm = TRUE),
    price_p95 = quantile(price, 0.95, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE)
  )

# Top categories
listings_small |>
  count(room_type, sort = TRUE) |>
  print(n = 10)

listings_small |>
  count(neighbourhood_group, sort = TRUE) |>
  print(n = 10)

# Peek at missingness in a few key fields
listings_small |>
  summarize(
    miss_price = sum(is.na(price)),
    miss_room  = sum(is.na(room_type)),
    miss_score = sum(is.na(review_scores_rating))
  )

# Optional: quick histogram of price (will show in Plots pane)
ggplot(listings_small, aes(price)) +
  geom_histogram(bins = 60) +
  coord_cartesian(xlim = c(0, quantile(listings_small$price, 0.99, na.rm = TRUE))) +
  labs(title = "Listing Price Distribution (trimmed at 99th percentile)",
       x = "Price (USD per night)", y = "Count")
