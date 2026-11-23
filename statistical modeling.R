# Run from the STA302Project R project.
source("STA302File1.R")

# Retain the Part 1 baseline for preliminary-plots.R.
model <- lm(term_coverage_2025 ~ normal_income_c * children + debt_100k +
              partnered + age_c + assets_100k + education + employment + homeowner,
            data = analysis_data, na.action = na.fail)

analysis_data <- as.data.frame(analysis_data)
stopifnot(nrow(analysis_data) == 2048L, !anyDuplicated(analysis_data$yy1),
          all(analysis_data$term_coverage_2025 > 0), all(analysis_data$norminc >= 0),
          all(analysis_data$debt_100k >= 0), all(analysis_data$assets_100k >= 0))
rownames(analysis_data) <- analysis_data$yy1
analysis_data <- transform(analysis_data,
  log_coverage = log(term_coverage_2025),
  log_income_c = log((norminc + 1000) / 101000),
  cube_income_c = (norminc / 100000)^(1/3) - 1,
  sqrt_debt = sqrt(debt_100k),
  log_assets = log(assets_100k + 1/100000))
final_model <- lm(log_coverage ~ log_income_c * children + sqrt_debt + log_assets +
                    age_c + I(age_c^2) + partnered + education + employment + homeowner,
                  data = analysis_data, na.action = na.fail)
sensitivity_model <- update(final_model, . ~ . - log_income_c * children + cube_income_c * children)
stopifnot(identical(rownames(model.frame(final_model)), rownames(model.frame(sensitivity_model))),
          final_model$rank == 12L, sensitivity_model$rank == 12L)
# Existing interactive callers can use the reviewed specification under this name.
transformed_model <- final_model

coverage_ratios <- function(fit) {
  # The covariates are additive and cancel from all four cell comparisons.
  d <- analysis_data[rep(1, 4), , drop = FALSE]
  income <- rep(c(50000, 100000), each = 2)
  d$log_income_c <- log((income + 1000) / 101000)
  d$cube_income_c <- (income / 100000)^(1/3) - 1
  d$children <- factor(rep(c("No", "Yes"), 2), levels = c("No", "Yes"))
  X <- model.matrix(delete.response(terms(fit)), d, contrasts.arg = fit$contrasts)
  L <- rbind("Income 100k / 50k, no children" = X[3,] - X[1,],
             "Income 100k / 50k, listed children" = X[4,] - X[2,],
             "Children / none at 50k" = X[2,] - X[1,],
             "Children / none at 100k" = X[4,] - X[3,],
             "Ratio of income ratios" = X[4,] - X[2,] - X[3,] + X[1,])
  estimate <- drop(L %*% coef(fit))
  se <- sqrt(diag(L %*% vcov(fit) %*% t(L)))
  margin <- qt(.975, df.residual(fit)) * se
  data.frame(ratio = exp(estimate), lower = exp(estimate-margin), upper = exp(estimate+margin))
}
print(coverage_ratios(final_model))
