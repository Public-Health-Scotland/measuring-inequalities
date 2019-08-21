# WORK IN PROGRESS
# Code for calculating different index of inequality
# For more information on the measures, please read: 
# 
# INSTRUCTIONS:
# First run the code in the packages and data sections. You might need to install these packages.
# Second, select the measure of your interest and run the code in that section.

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(purrr)
library(tidyr)
library(broom)

###############################################.
## Data ----
###############################################.
# Creating some example data to show how calculations work
# -- value -could be a rate, percentage or a numerator
# -- overall_value - value for the whole area. Used for RII calculation
# -- quintile - represents a fith of your population from most deprived (1) to least 
#    deprived (5). This could be another type of classification but for most of the 
#    measure used here they will have to be a ranked measure.
# -- population - the population of each quintile or category. In this example as 
#    they are quintiles they all have the same population
# -- area - you could modify this code easily to include multiple areas  
test_data <- data.frame(value = c(100, 70, 45, 20, 5),
                        overall_value = 48,
                        quintile = c(1:5), 
                        population = 1000,
                        area = "Scotland") %>% 
  # Create a colum with the values for the most deprived and for the
  # least deprived quintiles. This is used for the ranges and PAR
  mutate(most_depr_value = case_when(quintile == 1 ~ value, TRUE ~ 0),
         least_depr_value = case_when(quintile == 5 ~ value, TRUE ~ 0)) %>% 
  group_by(area) %>% 
  mutate(most_depr_value = max(most_depr_value), 
         least_depr_value = max(least_depr_value)) %>% 
  #These variables are used for SII, RII and PAR calculation
  mutate(total_pop = sum(population), # calculate the total population for each area (without SIMD).
         proportion_pop = population/total_pop) # proportion of the population in each SIMD out of the total population.

###############################################.
## Absolute and relative range ----
###############################################.
# The absolute and relative ranges look at the differences and ratio between
# the most and leats deprived quintiles
test_data <- test_data %>% 
  mutate(abs_range = most_depr_value - least_depr_value,
         rel_range = most_depr_value / least_depr_value) %>% ungroup()

View(test_data)

###############################################.
## Population attributable risk (PAR) ----
###############################################.
#Calculation PAR
#Formula here: https://pdfs.semanticscholar.org/14e0/c5ba25a4fdc87953771a91ec2f7214b2f00d.pdf
# https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf

test_data <- test_data %>%  
  mutate(par_rr = (value/least_depr_value - 1) * proportion_pop,
    par = sum(par_rr)/(sum(par_rr) + 1) * 100) %>% select(-par_rr) 

View(test_data)

###############################################.
## Slope of index on inequality (SII) ----
###############################################.
# The calculations below are those of the linear SII, you will have to amend the
# model if you wanted to calculate the Poisson SII
# This code will produce the results of the model, including confidence intervals
sii_model <- test_data %>%  group_by(area) %>% 
  mutate(cumulative_pro = cumsum(proportion_pop),  # cumulative proportion population for each area
       relative_rank = case_when(
         quintile == 1 ~ 0.5*proportion_pop,
         quintile != 1 ~ lag(cumulative_pro) + 0.5*proportion_pop),
       sqr_proportion_pop = sqrt(proportion_pop), #square root of the proportion of the population in each SIMD
       relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
       value_sqr_proppop = sqr_proportion_pop * value) %>% #value based on population weights
  nest() %>% #creating one column called data with all the variables not in the grouping
  # Calculating linear regression for all the groups, then formatting the results
  # and calculating the confidence intervals
  mutate(model = map(data, ~ lm(value_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
         #extracting sii from model, a bit fiddly but it works
         sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")),
         cis = map(model, confint_tidy)) %>% #calculating confidence intervals
  ungroup() %>% unnest(cis) %>% #Unnesting the CIs 
  #selecting only even row numbers which are the ones that have the sii cis
  filter(row_number() %% 2 == 0) %>% 
  mutate(lowci_sii = -1 * conf.high, #fixing interpretation
         upci_sii = -1 * conf.low) %>% 
  select(-conf.low, -conf.high) #non-needed variables

View(sii_model)

#Merging sii results with main data set
test_data <- left_join(test_data, sii_model, by = "area")

View(test_data)

###############################################.
## Relative index on inequality (RII) ----
###############################################.
#This is the calculation of the linear RII which is based on the SII values,
#so that section needs to be run before this one.
test_data <- test_data %>% mutate(rii = sii / overall_value,
           lowci_rii = lowci_sii / overall_value,
           upci_rii = upci_sii / overall_value,
  #Transforming RII into %. This way is interpreted as "most deprived areas are
  # xx% above the average" For example: Cancer mortality rate is around 55% higher 
  # in deprived areas relative to the mean rate in the population
           rii_int = rii * 0.5 *100,
           lowci_rii_int = lowci_rii * 0.5 *100,
           upci_rii_int = upci_rii * 0.5 *100)

View(test_data)

## END
  
  