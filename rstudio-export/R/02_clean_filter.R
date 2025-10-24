library(tidyverse)

raw_path <- "data/raw/listings.csv.gz"
listings  <- readr::read_csv(raw_path, show_col_types = FALSE)

# ---- harmonize column names that differ by city/version ----
# 1) Create neighbourhood_group from whichever exists
if (!"neighbourhood_group" %in% names(listings)) {
  if ("neighbourhood_group_cleansed" %in% names(listings)) {
    listings <- listings |> mutate(neighbourhood_group = neighbourhood_group_cleansed)
  } else if ("neighbourhood_cleansed" %in% names(listings)) {
    # fallback: use cleansed neighbourhood as a proxy group
    listings <- listings |> mutate(neighbourhood_group = neighbourhood_cleansed)
  } else if ("neighbourhood" %in% names(listings)) {
    listings <- listings |> mutate(neighbourhood_group = neighbourhood)
  } else {
    listings <- listings |> mutate(neighbourhood_group = NA_character_)
  }
}

# 2) Ensure we also have a neighbourhood column (prefer cleansed)
if (!"neighbourhood" %in% names(listings) && "neighbourhood_cleansed" %in% names(listings)) {
  listings <- listings |> mutate(neighbourhood = neighbourhood_cleansed)
}

# ---- keep a consistent working set of columns ----
cols_keep <- c(
  "id","host_id","neighbourhood_group","neighbourhood",
  "latitude","longitude","room_type","price",
  "minimum_nights","availability_365","number_of_reviews",
  "reviews_per_month","review_scores_rating",
  "calculated_host_listings_count","last_review"
)

df <- listings |>
  select(any_of(cols_keep)) |>
  mutate(
    price = readr::parse_number(as.character(price)),
    minimum_nights = as.numeric(minimum_nights),
    review_scores_rating = as.numeric(review_scores_rating)
  )

# ---- clean ----
cleaned <- df |>
  # realistic bounds; tweak as needed
  filter(price > 0, price < 800, minimum_nights < 60) |>
  # drop rows missing key fields (don't require neighbourhood_group anymore)
  drop_na(room_type, price)

# ---- feature engineering ----
cleaned <- cleaned |>
  mutate(
    price_per_review = price / (number_of_reviews + 1),
    rating_category = case_when(
      review_scores_rating >= 90 ~ "Excellent",
      review_scores_rating >= 80 ~ "Good",
      review_scores_rating >= 70 ~ "Average",
      TRUE ~ "Low"
    ),
    neighbourhood_group = replace_na(neighbourhood_group, "Unknown")
  )

# ---- save ----
write_csv(cleaned, "data/clean/airbnb_clean.csv")

# quick check
cleaned |>
  summarize(
    n_rows = n(),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(review_scores_rating, na.rm = TRUE)
  )

