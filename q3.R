# Packages
# install.packages("tidyverse") # if needed
library(tidyverse)

# s20 and s21 are already loaded and cleaned per your snippet:
s20 <- read_csv("kaggle_survey_2020_responses.csv", show_col_types = FALSE); s20 <- s20[-1, ]
s21 <- read_csv("kaggle_survey_2021_responses.csv", show_col_types = FALSE); s21 <- s21[-1, ]

# Harmonize a minimal schema across years, country, and gender
df20 <- s20 %>% transmute(year = "2020", country = Q3, gender = Q2)
df21 <- s21 %>% transmute(year = "2021", country = Q3, gender = Q2)
both <- bind_rows(df20, df21)
# Note: In these datasets, Q3 = Country, Q2 = Gender.

# Graph 1: Top 10 countries per year between 2020 and 2021
# 1. Count respondents by (year, country)
# 2. Take top 10 per year
# 3. Plot side-by-side via facets
top_countries <- both %>%
  filter(!is.na(country), country != "") %>%
  count(year, country, sort = TRUE) %>%
  group_by(year) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup()

ggplot(top_countries, aes(x = reorder(country, n), y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ year, scales = "free_y") +
  labs(
    title = "Top 10 Respondent Countries: 2020 vs 2021",
    x = "Country",
    y = "Number of respondents"
  )

# Graph 2: Gender distribution by year (percent) (2020 and 2021)
# 1. Lump less-common labels into "Other" to keep top 4 distributions
# 2. Compute percent within per year
# 3. Plot stacked bars with percent labels
gender_long <- both %>%
  filter(!is.na(gender), gender != "") %>%
  mutate(gender = forcats::fct_lump_n(as.factor(gender), n = 4, other_level = "Other")) %>%
  count(year, gender) %>%
  group_by(year) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

ggplot(gender_long, aes(x = year, y = pct, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Gender Distribution of Respondents by Year",
    x = NULL,
    y = "Percent of respondents",
    fill = "Gender"
  )
# your code that creates p1 and p2 to view
p1 <- ggplot(top_countries, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_col(color = "white") +
  coord_flip() +
  facet_wrap(~ year, scales = "free_y") +
  scale_fill_brewer(palette = "Set3") +     # try "Paired", "Set2", etc.
  guides(fill = "none") +                   # hide giant legend
  labs(title = "Top 10 Respondent Countries: 2020 vs 2021",
       x = "Country", y = "Number of respondents")
p2 <- ggplot(gender_long, aes(x = year, y = pct, fill = gender)) +
  geom_col(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gender Distribution of Respondents by Year",
       x = NULL, y = "Percent of respondents", fill = "Gender")

print(p1)
print(p2)
#Optional to save graphs to local drive
ggsave("graph1.png", p1, width = 9, height = 6, dpi = 300)
ggsave("graph2.png", p2, width = 9, height = 6, dpi = 300)

