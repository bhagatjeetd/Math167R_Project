# Q6: Did the education level of data professionals change from 2020 to 2021?

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read in both Kaggle survey datasets (skipping the metadata row)
data20 <- read.csv("kaggle_survey_2020_responses.csv", skip = 1, stringsAsFactors = FALSE)
data21 <- read.csv("kaggle_survey_2021_responses.csv", skip = 1, stringsAsFactors = FALSE)

# Identify the column that contains education level information
edu_colname <- "What.is.the.highest.level.of.formal.education.that.you.have.attained.or.plan.to.attain.within.the.next.2.years."

# Extract education level responses for each year
edu20 <- na.omit(data20[[edu_colname]])   # remove missing values
edu21 <- na.omit(data21[[edu_colname]])

# Create two new dataframes with a Year column added
edu20_df <- data.frame(Education = edu20, Year = "2020")
edu21_df <- data.frame(Education = edu21, Year = "2021")

# Combine both dataframes together for comparison
edu_combined <- rbind(edu20_df, edu21_df)

# Summarize the data to get proportions for each education level per year
edu_summary <- edu_combined %>%
  group_by(Year, Education) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

# Keep only education levels that appear in BOTH years (to keep the chart clean)
common_edu <- intersect(
  edu_summary$Education[edu_summary$Year == "2020"],
  edu_summary$Education[edu_summary$Year == "2021"]
)
edu_summary <- edu_summary %>% filter(Education %in% common_edu)

# Shorten the long label to make the graph look better
edu_summary$Education <- gsub(
  "Some college/university study without earning a bachelor’s degree",
  "Some college (no bachelor’s)",
  edu_summary$Education
)

# Order education levels so the bars stay consistent across years
edu_summary$Education <- factor(
  edu_summary$Education,
  levels = unique(edu_summary$Education[order(edu_summary$Proportion, decreasing = TRUE)])
)

# Create the bar chart comparing proportions for 2020 and 2021
edu_plot <- ggplot(edu_summary, aes(x = Education, y = Proportion, fill = Year)) +
  geom_col(position = "dodge") +                        # side-by-side bars
  coord_flip() +                                        # flip for easier reading
  labs(title = "Highest Education Level of Respondents: 2020 vs 2021",
       x = "Education Level",
       y = "Proportion of Respondents") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9),           # make text smaller
        plot.title = element_text(size = 13, face = "bold"))

# Display the plot
print(edu_plot)

# Save the plot as a smaller, high-quality image for the report
ggsave("Q6_Education_Level_Comparison.png", plot = edu_plot, width = 6, height = 4, dpi = 300)
