library(haven)
library(dplyr)
#pls be patient w/ file size, it is very large (226mb) 
#took a while to understand & clean - andres

#The x codes refer to specific columns from the p22i6 file, 
#do NOT edit them, it will otherwise f^ck up the file and data entirely 

#reference for p22i6 file, 
# x4002 column is boolean if we have term insurance (1=yes, 5=no)
# x4003 column is amount of household term insurance covered


# we filtered this file to only take households w/ term insurance
# dta file is used since U.S. gov didn't have csv file
life_data <- read_dta("p22i6.dta", col_select = c(y1, yy1, x4002, x4003))
trimmed_data <- life_data %>%
  select(y1, yy1, x4002,x4003) %>% filter(x4002 == 1,x4003 > 0)

scf_summary <- read.csv("SCFP2022.csv") #dataset for all else

names(scf_summary) <- tolower(names(scf_summary)) #make columns lowercase

# Keep eligible term-insurance records and attach summary data using both IDs.
# we only use p22i6 for term insurance, scfp2022 for all else
ultimate_data <- left_join(trimmed_data,scf_summary, by = c("y1", "yy1"))
ultimate_data$implicate <- ultimate_data$y1 %% 10

# Keep implicate 1 so each family appears once in this analysis.
implicate_1 <- filter(ultimate_data, implicate == 1)

# Use the first implicate for modeling. Summary monetary values are in 2025 dollars.
# Convert only raw term coverage from 2022 dollars to the same price basis.
# Source: Federal Reserve SCF bulletin.macro.txt (saved with the project).
scf_price_index_2022 <- 4376
scf_price_index_2025 <- 4789
coverage_to_2025 <- scf_price_index_2025 / scf_price_index_2022

implicate_1 <- implicate_1 %>%
  mutate(term_coverage_2025 = x4003 * coverage_to_2025)

# Keep the existing data and add the agreed model variables.
analysis_data <- implicate_1 %>%
  mutate(
    normal_income_c = norminc / 100000 - 1,
    debt_100k = debt / 100000,
    assets_100k = (fin - cashli) / 100000,
    age_c = (age - 50) / 10,

    children = factor(
      ifelse(kids > 0, "Yes", "No"),
      levels = c("No", "Yes")
    ),

    partnered = factor(
      ifelse(married == 1, "Yes", "No"),
      levels = c("No", "Yes")
    ),

    homeowner = factor(
      ifelse(hhouses == 1, "Yes", "No"),
      levels = c("No", "Yes")
    ),

    # EDCL 4 = bachelor's degree or higher; 1-3 = below bachelor's.
    education = factor(
      ifelse(edcl == 4, "Bachelor+", "Below bachelor"),
      levels = c("Below bachelor", "Bachelor+")
    ),

    # OCCAT2: 1-3 are occupation groups; 4 means not working.
    employment = factor(
      ifelse(occat2 == 4, "Not working", "Working"),
      levels = c("Not working", "Working")
    )
  )


# Agreed preliminary model: nine predictors plus the interaction.
model <- lm(
  term_coverage_2025 ~ normal_income_c * children +
    debt_100k + partnered + age_c + assets_100k +
    education + employment + homeowner,
  data = analysis_data,
  na.action = na.fail
)

summary(model)
