library(readr)
library(ggplot2)

# Load CSVs and drop the first row of question text
s20 <- read_csv("kaggle_survey_2020_responses.csv", show_col_types = FALSE)
s20 <- s20[-1, ]
s21 <- read_csv("kaggle_survey_2021_responses.csv", show_col_types = FALSE)
s21 <- s21[-1, ]

# Build TRUE/FALSE indicators for language selection from Q7 parts
uses_python_20 <- !is.na(s20$Q7_Part_1)   
uses_r_20      <- !is.na(s20$Q7_Part_2)   
uses_sql_20    <- !is.na(s20$Q7_Part_3)   

uses_python_21 <- !is.na(s21$Q7_Part_1)
uses_r_21      <- !is.na(s21$Q7_Part_2)
uses_sql_21    <- !is.na(s21$Q7_Part_3)

n20 <- nrow(s20)
n21 <- nrow(s21)

# Proportions
p_py_2020  <- sum(uses_python_20) / n20
p_py_2021  <- sum(uses_python_21) / n21
p_r_2020   <- sum(uses_r_20)      / n20
p_r_2021   <- sum(uses_r_21)      / n21
p_sql_2020 <- sum(uses_sql_20)    / n20
p_sql_2021 <- sum(uses_sql_21)    / n21

# Two-proportion tests 
tp_py  <- prop.test(c(sum(uses_python_20), sum(uses_python_21)), c(n20, n21), correct = FALSE)
tp_r   <- prop.test(c(sum(uses_r_20),      sum(uses_r_21)),      c(n20, n21), correct = FALSE)
tp_sql <- prop.test(c(sum(uses_sql_20),    sum(uses_sql_21)),    c(n20, n21), correct = FALSE)

# Print results
cat("Question 2: Language usage\n")
cat("Python  ", "p2020 =", round(p_py_2020, 4), " p2021 =", round(p_py_2021, 4), " p-value =", tp_py$p.value, "\n")
cat("R       ", "p2020 =", round(p_r_2020, 4),  " p2021 =", round(p_r_2021, 4),  " p-value =", tp_r$p.value, "\n")
cat("SQL     ", "p2020 =", round(p_sql_2020,4), " p2021 =", round(p_sql_2021,4), " p-value =", tp_sql$p.value, "\n\n")

# Plot of proportions by year
plot_lang <- data.frame(
  year = rep(c("2020","2021"), each = 3),
  language = rep(c("Python","R","SQL"), times = 2),
  prop_used = c(p_py_2020, p_r_2020, p_sql_2020,
                p_py_2021, p_r_2021, p_sql_2021)
)

ggplot(plot_lang, aes(x = year, y = prop_used, fill = language)) +
  geom_col(position = "dodge") +
  labs(title = "Language usage prevalence, 2020 vs 2021",
       x = "Year", y = "Share of respondents selecting language") +
  theme_minimal()