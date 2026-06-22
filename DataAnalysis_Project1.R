library(tidyverse)
library(infer)

# Check the current working directory

# setwd()
# getwd()

raw_data <- read_csv("student_placement_career_success_dataset.csv")
# View(raw_data)

# turn all categorical columns into factor
transformed_data <- raw_data %>%
  mutate(across(where(is.character) & !student_id, as.factor)) %>%
  mutate(college_tier = factor(college_tier, 
                               levels = c(1, 2, 3), 
                               labels = c("Top Tier", "Middle Tier", "Low Tier"))) %>%
  mutate(monthly_salary = 0.082 * salary_lpa * 100000 / 12)

# Transform salary in lpa to HKD per month

glimpse(transformed_data)
View(transformed_data)

theme_set(theme_minimal())

ggplot(transformed_data, aes(group = college_tier, y = monthly_salary)) +
  geom_boxplot(aes(fill = college_tier)) +
  labs(title = "Boxplot of Monthly Salary in HKD grouped by college tier",
       x = "college tier (highest 1, lowerest 3)",
       y = "salary lpa") +
  theme(axis.text.x = element_blank())

# Perform the F test to check whether the mean
# of difference groups are significantly difference
monthly_salary_vs_college_tier <- lm(monthly_salary ~ college_tier, data = transformed_data)
anova(monthly_salary_vs_college_tier)

# Since p < 0.05, we reject H0, the mean salary between
# difference college tier are not the same

ggplot(transformed_data, aes(x = cgpa)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue1") +
  labs(title  = "Grade distribution of students",
       x = "CGPA",
       y = "Number of students")
  # stat_function(fun = dnorm,
  #               args = list(mean = mean(transformed_data$cgpa), 
  #                           sd = sd(transformed_data$cgpa)))

ggplot(transformed_data, aes(x = monthly_salary)) +
  geom_histogram(fill = "pink1", bins = 30) +
  geom_vline(xintercept = mean(transformed_data$monthly_salary)) +
  labs(title  = "Monthly Salary distribution of students",
       x = "Monthly Salary in HKD",
       y = "Number of students")

ggplot(transformed_data, aes(x = cgpa,
                             y = monthly_salary)) +
  geom_point(color = "lightblue2", alpha = 0.4) +
  geom_smooth() +
  labs(title  = "CGPA vs Monthly Salary in HKD",
       x = "CGPA",
       y = "Monthly Salary")

cor(transformed_data$cgpa, transformed_data$monthly_salary)

# r < 0.5, There is a Weak relationship between monthly salary and cgpa

salary_vs_company_type <- transformed_data %>%
  select(monthly_salary,company_type) %>%
  filter(company_type != "None")

pairwise.t.test(salary_vs_company_type$monthly_salary, 
                salary_vs_company_type$company_type,
                p.adjust.method = "bonferroni")

# There is no difference between the mean salary
# across difference types of company

set.seed(115)

students_without_placement <- transformed_data %>%
  filter(placement_status == "Not Placed") %>%
  slice_sample(n = 100)

students_with_placement <- transformed_data %>%
  filter(placement_status == "Placed") %>%
  slice_sample(n = 100)

sample_students <- bind_rows(students_with_placement, students_without_placement)

# Take 100 samples from students_with_placement and students_without_placement
ggplot(sample_students, aes(x = cgpa, fill = placement_status, colour = placement_status)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  labs(title  = "CGPA distribution in two groups of students")

ggplot(sample_students, aes(x = DSA_problems_solved, fill = placement_status, colour = placement_status)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  labs(title  = "Numbers DSA Problems solved in two groups of students")

# In conclusion, the only thing that really matters to student's
# salary is the college tier, GPA does affect it, but not as important as
# college tier.
