# Script for running the zero-inflated beta regression model used in 
library(brms)
library(knitr)
library(dplyr)

df_dep <- read.csv('df_dep.csv', header = TRUE)
df <- read.csv('df_exp.csv', header = TRUE)

# Add one of the dependent variable columns to the explanatory dataframe (change column for each model)
df$percentage <- df_dep$curr_flexible_work_pct / 100

# Remove any entries with $1M annual income (outlier value causes issues)
df<- df[df$income < 1000, ]  

# Also scale the income parameter as good practice
df$income <- df$income / 10

# Should be text, otherwise model assumes continuous variable
df$female <- as.character(df$female) 
df$occupation <- as.character(df$occupation) 
df$work_industry <- as.character(df$work_industry)
df$race_ethnicity <- as.character(df$race_ethnicity) 
df$living_area <- as.character(df$living_area)

zoib_model <- bf(
  percentage ~ female + living_area + age_quant + educ_years + income, # + occupation + work_industry + race_ethnicity
  phi ~ female + living_area + age_quant + educ_years + income, # + occupation + work_industry + race_ethnicity
  zoi ~ female + living_area + age_quant + educ_years + income, # + occupation + work_industry + race_ethnicity
  coi ~ female + living_area + age_quant + educ_years + income, # + occupation + work_industry + race_ethnicity
  family = zero_one_inflated_beta()
)

fit <- brm(
  formula = zoib_model,
  data = df
)

summary(fit)

results <- summary(fit)[["fixed"]]
# write.csv(results, "zoib_results_planned.csv")


posterior_samples(fit, pars = "b_") %>% 
  mutate_at(c("b_phi_Intercept", "b_phi_female1", "b_phi_living_area2", "b_phi_living_area3",
              "b_phi_age_quant", "b_phi_educ_years", "b_phi_income"), exp) %>% 
  mutate_at(vars(-c("b_phi_Intercept", "b_phi_female1", "b_phi_living_area2", "b_phi_living_area3",
                    "b_phi_age_quant", "b_phi_educ_years", "b_phi_income")), plogis) %>% 
  posterior_summary() %>% 
  as.data.frame() %>% 
  kable(digits = 2) 


chts <- read.csv("chts_input.csv")
chts <- chts[, c("sex", "educ_cont", "income_cont", "age", "density_suburban", "density_urban")]
names(chts) <- c("female", "educ_years", "income", "age_quant", "density_suburban", "density_urban")

chts$living_area <- 3
chts["living_area"][chts["density_urban"] == 1] <- 1
chts["living_area"][chts["density_suburban"] == 1] <- 2
chts$density_suburban <- NULL
chts$density_urban <- NULL
chts$income <- chts$income / 10 # Scale income to match SWAA input

predicted_vals <- posterior_epred(fit, newdata=chts)
avg_vals <- colMeans(predicted_vals)
write.csv(avg_vals, "zoib_predictions_current.csv", row.names=FALSE)
