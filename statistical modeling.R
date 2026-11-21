# Open STA302Project.Rproj and run from the project folder.
source("STA302File1.R")

# Original Part 1 model: keep this for preliminary-plots.R and comparison.
model <- lm(
  term_coverage_2025 ~ normal_income_c * children +
    debt_100k + partnered + age_c + assets_100k +
    education + employment + homeowner,
  data = analysis_data,
  na.action = na.fail
)

# Chosen working transformations; all money is in 2025 US dollars.
# Coverage is positive and needs no offset. Square roots preserve true zeros.
# 0.00001 in $100,000 units is $1, so assets are log((assets + $1) / $100k).
# This offset is a modelling convention; sensitivity has not been established.
analysis_data <- analysis_data %>%
  mutate(
    log_coverage = log(term_coverage_2025),
    sqrt_income_c = sqrt(norminc / 100000) - 1,
    sqrt_debt_100k = sqrt(debt_100k),
    log_assets = log(assets_100k + 0.00001)
  )

transformed_model <- lm(
  log_coverage ~ sqrt_income_c * children +
    sqrt_debt_100k + partnered + age_c + log_assets +
    education + employment + homeowner,
  data = analysis_data,
  na.action = na.fail
)

# Preserve the former candidate name for existing console references.
income_debt_sqrt_model <- transformed_model
stopifnot(nobs(model) == 2048, nobs(transformed_model) == nobs(model),
          identical(rownames(model.frame(model)),
                    rownames(model.frame(transformed_model))))

# The chosen forms do not establish that every model assumption holds.
# Earlier eight-candidate results remain in results/transformations/.
summary(model)
summary(transformed_model)
