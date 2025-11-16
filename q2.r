library(readr)
library(ggplot2)

s20 <- read_csv("kaggle_survey_2020_responses.csv", show_col_types = FALSE)
s20 <- s20[-1, ] # Drop first row (it only contains the question text, not answers)

s21 <- read_csv("kaggle_survey_2021_responses.csv", show_col_types = FALSE)
s21 <- s21[-1, ] # Drop first row (question text)

# Convert Q6 categories to numeric midpoints
# Named vector: each Q6 category mapped to a numeric midpoint
lookup <- c(
  "I have never written code" = 0.0,   # no coding experience
  "< 1 years"                 = 0.5,   # about half a year
  "1-2 years"                 = 1.5,   # midpoint of 1–2
  "1-3 years"                 = 2.0,   # midpoint of 1–3
  "2-3 years"                 = 2.5,   # midpoint of 2–3
  "3-5 years"                 = 4.0,   # midpoint of 3–5
  "5-10 years"                = 7.5,   # midpoint of 5–10
  "10-20 years"               = 15.0,  # midpoint of 10–20
  "20+ years"                 = 25.0   # a chosen value for 20+ years
)

# Helper function: map a vector of Q6 strings to numeric midpoints
map_years <- function(v) {
  out <- unname(lookup[v])   # look up each category; unmatched stay NA
  as.numeric(out)            # return numeric vector
}

y20 <- map_years(s20$Q6) # Apply mapping to 2020 Q6 responses
y20 <- y20[!is.na(y20)] # Remove NAs (responses that did not match any category)
y21 <- map_years(s21$Q6) # Apply mapping to 2021 Q6 responses
y21 <- y21[!is.na(y21)] # Remove NAs

# Statistical test: compare means between 2020 and 2021
# two-sample t-test on numeric midpoints (unequal variances allowed)
t_years <- t.test(y20, y21, var.equal = FALSE)

cat("Years of coding\n") # Print summary statistics and test results
cat("n2020 =", length(y20), " n2021 =", length(y21), "\n") # Sample sizes for each year
cat("mean2020 =", mean(y20), " median2020 =", median(y20), "\n") # Mean and median number of years in 2020
cat("mean2021 =", mean(y21), " median2021 =", median(y21), "\n") # Mean and median number of years in 2021
cat("t-test on midpoints: t =", t_years$statistic, " p =", t_years$p.value, "\n\n") # t-test statistic and p-value

# Build data frame for ECDF plot
# Combine 2020 and 2021 vectors into one data frame with a year label
ecdf_df <- rbind(
  data.frame(year = "2020", years_mid = y20),
  data.frame(year = "2021", years_mid = y21)
)

# ECDF plot: compare full distributions
ggplot(ecdf_df, aes(x = years_mid, color = year)) +
  stat_ecdf(size = 1) + # ECDF shows cumulative proportion up to each x value
  labs(title = "Years of coding experience: 2020 vs 2021",  # Add title and axis labels
       x = "Years (midpoint of category)", 
       y = "ECDF") +
  theme_minimal() # Use a simple clean theme
