library(readr)      
library(ggplot2)    

# Load 2020 CSV and drop the first row (question text, not data)
s20 <- read_csv("kaggle_survey_2020_responses.csv", show_col_types = FALSE)
s20 <- s20[-1, ]

# Load 2021 CSV and drop the first row (question text)
s21 <- read_csv("kaggle_survey_2021_responses.csv", show_col_types = FALSE)
s21 <- s21[-1, ]

# Create TRUE/FALSE flags for language use in 2020
# Respondent uses language if Q7_Part_* is not NA
uses_python_20 <- !is.na(s20$Q7_Part_1)   # Python in 2020
uses_r_20      <- !is.na(s20$Q7_Part_2)   # R in 2020
uses_sql_20    <- !is.na(s20$Q7_Part_3)   # SQL in 2020

# Create TRUE/FALSE flags for language use in 2021
uses_python_21 <- !is.na(s21$Q7_Part_1)   # Python in 2021
uses_r_21      <- !is.na(s21$Q7_Part_2)   # R in 2021
uses_sql_21    <- !is.na(s21$Q7_Part_3)   # SQL in 2021

# Store sample sizes for each year
n20 <- nrow(s20)
n21 <- nrow(s21)

# Compute sample proportions for each language in each year
p_py_2020  <- sum(uses_python_20) / n20
p_py_2021  <- sum(uses_python_21) / n21
p_r_2020   <- sum(uses_r_20)      / n20
p_r_2021   <- sum(uses_r_21)      / n21
p_sql_2020 <- sum(uses_sql_20)    / n20
p_sql_2021 <- sum(uses_sql_21)    / n21

# Two-sample proportion tests for each language (2020 vs 2021)
# Inputs are counts of "users" and total sample sizes
tp_py  <- prop.test(c(sum(uses_python_20), sum(uses_python_21)), c(n20, n21), correct = FALSE)
tp_r   <- prop.test(c(sum(uses_r_20),      sum(uses_r_21)),      c(n20, n21), correct = FALSE)
tp_sql <- prop.test(c(sum(uses_sql_20),    sum(uses_sql_21)),    c(n20, n21), correct = FALSE)

# Print proportions and p-values to the console
cat("Language usage\n")
cat("Python  ", "p2020 =", round(p_py_2020, 4), " p2021 =", round(p_py_2021, 4),
    " p-value =", tp_py$p.value, "\n")
cat("R       ", "p2020 =", round(p_r_2020, 4),  " p2021 =", round(p_r_2021, 4),
    " p-value =", tp_r$p.value, "\n")
cat("SQL     ", "p2020 =", round(p_sql_2020, 4), " p2021 =", round(p_sql_2021, 4),
    " p-value =", tp_sql$p.value, "\n\n")

# Build small data frame for plotting proportions
plot_lang <- data.frame(
  year = rep(c("2020","2021"), each = 3),          # x-axis groups
  language = rep(c("Python","R","SQL"), times = 2),# bar colors
  prop_used = c(p_py_2020, p_r_2020, p_sql_2020,   # y values for 2020
                p_py_2021, p_r_2021, p_sql_2021)   # y values for 2021
)

# Bar plot comparing language usage by year
ggplot(plot_lang, aes(x = year, y = prop_used, fill = language)) +
  geom_col(position = "dodge") +                   # side-by-side bars
  labs(title = "Language usage prevalence, 2020 vs 2021",
       x = "Year", y = "Share of respondents selecting language") +
  theme_minimal()                                  # simple clean theme
