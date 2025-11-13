library(readr)
library(ggplot2)

# Load CSVs and drop the first row of question text
s20 <- read_csv("Desktop/Math167R/Project/kaggle_survey_2020_responses.csv", show_col_types = FALSE)
s20 <- s20[-1, ]
s21 <- read_csv("Desktop/Math167R/Project/kaggle_survey_2021_responses.csv", show_col_types = FALSE)
s21 <- s21[-1, ]

# Map Q6 bins to numeric midpoints
lookup <- c(
  "I have never written code" = 0.0,
  "< 1 years"                 = 0.5,
  "1-2 years"                 = 1.5,
  "1-3 years"                 = 2.0,
  "2-3 years"                 = 2.5,
  "3-5 years"                 = 4.0,
  "5-10 years"                = 7.5,
  "10-20 years"               = 15.0,
  "20+ years"                 = 25.0
)

map_years <- function(v) {
  out <- unname(lookup[v])   # unmatched become NA
  as.numeric(out)
}

y20 <- map_years(s20$Q6); y20 <- y20[!is.na(y20)]
y21 <- map_years(s21$Q6); y21 <- y21[!is.na(y21)]

# two-sample t-test on numeric midpoints
t_years <- t.test(y20, y21, var.equal = FALSE)

# Print results
cat("Question 3: Years of coding\n")
cat("n2020 =", length(y20), " n2021 =", length(y21), "\n")
cat("mean2020 =", mean(y20), " median2020 =", median(y20), "\n")
cat("mean2021 =", mean(y21), " median2021 =", median(y21), "\n")
cat("t-test on midpoints: t =", t_years$statistic, " p =", t_years$p.value, "\n\n")

# ECDF plot (shows full distribution)
ecdf_df <- rbind(
  data.frame(year = "2020", years_mid = y20),
  data.frame(year = "2021", years_mid = y21)
)

ggplot(ecdf_df, aes(x = years_mid, color = year)) +
  stat_ecdf(size = 1) +
  labs(title = "Years of coding experience: 2020 vs 2021",
       x = "Years (midpoint of category)", y = "ECDF") +
  theme_minimal()