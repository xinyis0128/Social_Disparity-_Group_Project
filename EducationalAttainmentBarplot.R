library(tidyverse)
library(utils)

## Load educational attainment data from the last five years
education_data <- read.csv("EducationalAttainmentData.csv")
education_data$HighSchoolOrLess <- rowSums(education_data[,3:10])
education_data$CollegeDropout <- rowSums(education_data[,11])
## Add column for count of college graduates
education_data$CollegeGraduate <- rowSums(education_data[,12:17])

## Load US population data, caluclate populations by race
us_population_data <- read.csv("USPopulationData.csv")
us_pop_by_year <- select(us_population_data, TotalPopulation)
hispanic_pop <- select(us_population_data, Hispanic) * us_pop_by_year
black_pop <- select(us_population_data, Black) * us_pop_by_year
white_pop <- select(us_population_data, White) * us_pop_by_year
asian_pop <- select(us_population_data, Asian) * us_pop_by_year

## Calculate number and percentage of college grads by race
## Numbers are in thousands, so need to adjust
white_grads <- education_data %>% 
  filter(Race == "White") %>% 
  select(CollegeGraduate) * 1000
white_grad_percentage <- white_grads / white_pop
black_grads <- education_data %>% 
  filter(Race == "Black") %>% 
  select(CollegeGraduate) * 1000
black_grad_percentage <- black_grads / black_pop
asian_grads <- education_data %>% 
  filter(Race == "Asian") %>% 
  select(CollegeGraduate) * 1000
asian_grad_percentage <- asian_grads / asian_pop
hispanic_grads <- education_data %>% 
  filter(Race == "Hispanic") %>% 
  select(CollegeGraduate) * 1000
hispanic_grad_percentage <- hispanic_grads / hispanic_pop

## Prepare data for bar plot
bar_plot_table <- matrix(c(as.vector(unlist(asian_grad_percentage)),
                  as.vector(unlist(white_grad_percentage)),
                  as.vector(unlist(black_grad_percentage)),
                  as.vector(unlist(hispanic_grad_percentage))),
                  ncol = 4, byrow = TRUE)
colnames(mytab) <- c(2016, 2017, 2018, 2019)
rownames(mytab) <- c("Asian", "White", "Black", "Hispanic")

barplot(bar_plot_table, main = "Percentage of Population with College Degrees",
        xlab = "Year", ylab = "Percentage", 
        args.legend = list(x = 'topright', bty = 'n', inset = c(-0.025, -0.05)),
        col = brewer.pal(n = 4, name = "YlOrRd"), 
        legend = rownames(mytab), beside = TRUE)
