library(tidyverse)
library(ipumsr)

data <- read_csv("STA304-5/usa_00003.csv")


# Filter for respondents with a doctoral degree (EDUCD == "Doctoral degree")
doctoral_data <- data %>% 
  filter(EDUCD == "116") %>%
  group_by(STATEICP) %>%
  summarize(count_doctoral = n())


total_california <- 391171

# Calculate ratio for California
ratio_california <- doctoral_data %>% 
  filter(STATEICP == "71") %>% 
  pull(count_doctoral) / total_california

# Apply the ratio to estimate total respondents in other states
estimates <- doctoral_data %>%
  mutate(estimate_total = count_doctoral / ratio_california)

# See estimate
print(estimates)

# Make a comparison
comparison <- estimates %>%
  rename(estimated_doctoral = count_doctoral) %>%
  left_join(doctoral_data, by = "STATEICP") %>%
  mutate(difference = estimate_total - count_doctoral)

# Output result
print(comparison)

write_csv(comparison, "STA304-5/comparison_results.csv")