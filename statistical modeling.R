# Run from the STA302Project folder to prepare the current analysis data.
source("STA302File1.R")

# Agreed preliminary model: nine predictors plus the interaction.
model <- lm(
  term_coverage_2025 ~ normal_income_c * children +
    debt_100k + partnered + age_c + assets_100k +
    education + employment + homeowner,
  data = analysis_data,
  na.action = na.fail
)

summary(model)
