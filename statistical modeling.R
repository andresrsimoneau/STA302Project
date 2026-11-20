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

ln_model <- lm(
  log(term_coverage_2025) ~ normal_income_c * children +
    debt_100k + partnered + age_c + log(assets_100k + 0.00001) +
    education + employment + homeowner,
  data = analysis_data,
  na.action = na.fail
)
summary(ln_model)

table(
  Negative = analysis_data$assets_100k < 0,
  Zero = analysis_data$assets_100k == 0,
  Positive = analysis_data$assets_100k > 0
)


table(
  Negative = analysis_data$normal_income_c < 0,
  Zero = analysis_data$normal_income_c == 0,
  Positive = analysis_data$normal_income_c > 0
)