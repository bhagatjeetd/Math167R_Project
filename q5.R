# Q5: Did the distribution of job titles among data professionals change from 2020 to 2021?

# Load libraries
library(ggplot2)
library(dplyr)

# Read in both Kaggle survey datasets (skipping the metadata row at the top)
data20 <- read.csv("kaggle_survey_2020_responses.csv", skip = 1, stringsAsFactors = FALSE)
data21 <- read.csv("kaggle_survey_2021_responses.csv", skip = 1, stringsAsFactors = FALSE)

# Identify the column that contains job title information
job_colname <- "Select.the.title.most.similar.to.your.current.role..or.most.recent.title.if.retired.....Selected.Choice"

# Extract job title data
jobs20 <- na.omit(data20[[job_colname]])
jobs21 <- na.omit(data21[[job_colname]])

# Create separate dataframes with 'Year' column
jobs20_df <- data.frame(JobTitle = jobs20, Year = "2020")
jobs21_df <- data.frame(JobTitle = jobs21, Year = "2021")

# Combine into one dataframe for comparison
jobs_combined <- rbind(jobs20_df, jobs21_df)

# Summarize data to calculate how often each job title appears in each year
job_summary <- jobs_combined %>%
  group_by(Year, JobTitle) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

# Keep only top 8 most common roles
top_jobs <- job_summary %>%
  group_by(JobTitle) %>%
  summarise(Total = sum(Count)) %>%
  top_n(8, Total) %>%
  pull(JobTitle)

job_summary <- job_summary %>%
  filter(JobTitle %in% top_jobs)

# Bar plot comparing proportions for 2020 and 2021
job_plot <- ggplot(job_summary, aes(x = reorder(JobTitle, Proportion), y = Proportion, fill = Year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Distribution of Job Titles: 2020 vs 2021",
       x = "Job Title",
       y = "Proportion of Respondents") +
  theme_minimal()


print(job_plot)
