library(dplyr)
library(ggplot2)

#load the cleaned dataset
galaxies <- read_csv("clean_galaxy_data.csv")

#The stellar mass is a good measure of the size of a galaxy. 
#So we would use the log_lk variable to analyze the catalog

# Round the log_lk variable to the specified decimal places
galaxies <- galaxies |> 
  mutate(rounded_log_lk = round(log_lk, digits = 0))

# Count the frequency of each rounded value
value_counts <- table(galaxies$rounded_log_lk)

# Calculate the percentage of each category
percentage_data <- galaxies %>%
  group_by(rounded_log_lk) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create a bar plot using ggplot2 with percentage labels
ggplot(percentage_data, aes(x = as.factor(rounded_log_lk), y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) + # Add percentage labels
  labs(
    title = "Count of galaxy by Stellar Mass",
    x = "Rounded log_lk Values",
    y = "Percentage"
  )

#as we can see small galaxies(log_lk value <7) are only a tiny fraction (8%)of 
#the data set. One reason can be that smaller galaxies can be hard to detect in 
#the vast nothingness of the space !