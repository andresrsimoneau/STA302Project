# Diagnostics for the chosen working transformed model.
# Run this file in RStudio; figures appear directly in the Plots pane.
# Six pages: core fit, numeric predictors, categories, predictor relationships,
# influence, and scale-location. Use the Plots arrows to browse.
source("statistical modeling.R")

# In RStudio, explicitly use the Plots pane rather than an open file device.
if (interactive() && identical(Sys.getenv("RSTUDIO"), "1")) {
  devices <- dev.list()
  rstudio_device <- devices[names(devices) == "RStudioGD"]
  if (length(rstudio_device)) {
    dev.set(rstudio_device[1])
  } else {
    RStudioGD()
  }
}

# Plot the same response and predictor forms used by transformed_model.
residuals <- resid(transformed_model)
fitted_values <- fitted(transformed_model)
coverage <- analysis_data$log_coverage
income <- analysis_data$sqrt_income_c
debt <- analysis_data$sqrt_debt_100k
assets <- analysis_data$log_assets
age <- analysis_data$age
child_colour <- ifelse(analysis_data$children == "Yes", "orange3", "steelblue")

# 1. Core overall grid: three full-range checks and one useful zoom.
par(mfrow = c(2, 2), mar = c(6, 4.5, 3, 1), ann = TRUE)
plot(fitted_values, residuals,
     xlab = "Fitted log coverage", ylab = "Residual (log coverage)",
     main = "Residuals vs fitted: full range")
abline(h = 0, col = "red")
lines(lowess(fitted_values, residuals), col = "blue")

qqnorm(rstandard(transformed_model), main = "Normal Q-Q", ylab = "Standardized residual")
qqline(rstandard(transformed_model), col = "red")

plot(fitted_values, coverage,
     xlab = "Fitted log coverage",
     ylab = "Observed log coverage", main = "Observed vs fitted: full range")
abline(0, 1, col = "red")

# Only the displayed limits change; the model and smoother use all families.
plot(fitted_values, residuals,
     xlim = c(min(fitted_values), quantile(fitted_values, 0.95)),
     ylim = quantile(residuals, c(0.01, 0.99)),
     xlab = "Fitted log coverage", ylab = "Residual (log coverage)",
     main = "Residuals vs fitted: zoom",
     sub = "x: min-95th percentile; y: 1st-99th")
abline(h = 0, col = "red")
lines(lowess(fitted_values, residuals), col = "blue")

# 2. Residuals against numeric predictors: full range
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(income, residuals, col = child_colour,
     xlab = "sqrt(income / $100k) - 1", ylab = "Residual (log coverage)",
     main = "Residuals vs income")
abline(h = 0, col = "red")
legend("topright", c("No children", "Children"),
       col = c("steelblue", "orange3"), pch = 1, bty = "n", cex = 0.8)

plot(debt, residuals,
     xlab = "sqrt(debt / $100k)", ylab = "Residual (log coverage)",
     main = "Residuals vs debt")
abline(h = 0, col = "red")

plot(age, residuals,
     xlab = "Age (years)", ylab = "Residual (log coverage)",
     main = "Residuals vs age")
abline(h = 0, col = "red")

plot(assets, residuals,
     xlab = "log((assets + $1) / $100k)",
     ylab = "Residual (log coverage)", main = "Residuals vs assets")
abline(h = 0, col = "red")

# 3. Residuals against categorical predictors: full range
par(mfrow = c(2, 3), mar = c(6, 4, 3, 1))
boxplot(residuals ~ analysis_data$children,
        xlab = "Children", ylab = "Residual (log coverage)", main = "Child presence")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$partnered,
        xlab = "Partnered", ylab = "Residual (log coverage)", main = "Partnership")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$education,
        names = c("Below bachelor", "Bachelor+"),
        cex.axis = 0.7, xlab = "Education", ylab = "Residual (log coverage)", main = "Education")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$employment,
        names = c("Not working", "Working"),
        cex.axis = 0.7, xlab = "Employment", ylab = "Residual (log coverage)", main = "Employment")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$homeowner,
        xlab = "Homeowner", ylab = "Residual (log coverage)", main = "Homeownership")
abline(h = 0, col = "red")
plot.new() # Leave the sixth panel empty.

# 4. Relationships between numeric predictors: full range
# Blue = no children; orange = children. Assets exclude insurance cash value.
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
plot(income, debt, col = child_colour,
     xlab = "sqrt(income / $100k) - 1", ylab = "sqrt(debt / $100k)", main = "Income and debt")
legend("topright", c("No children", "Children"),
       col = c("steelblue", "orange3"), pch = 1, bty = "n", cex = 0.8)
plot(income, age, col = child_colour,
     xlab = "sqrt(income / $100k) - 1", ylab = "Age (years)", main = "Income and age")
plot(income, assets, col = child_colour,
     xlab = "sqrt(income / $100k) - 1", ylab = "log((assets + $1) / $100k)", main = "Income and assets")
plot(debt, age, col = child_colour,
     xlab = "sqrt(debt / $100k)", ylab = "Age (years)", main = "Debt and age")
plot(debt, assets, col = child_colour,
     xlab = "sqrt(debt / $100k)", ylab = "log((assets + $1) / $100k)", main = "Debt and assets")
plot(age, assets, col = child_colour,
     xlab = "Age (years)", ylab = "log((assets + $1) / $100k)", main = "Age and assets")

# 5. Review influence. Standard plot labels are analysis rows, not family IDs.
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 2))
plot(transformed_model, which = 5, ask = FALSE, sub.caption = "")
plot(transformed_model, which = 4, ask = FALSE, sub.caption = "")

# Return to one plot per page for subsequent plotting.
par(mfrow = c(1, 1))

# 6. Check error spread directly on the transformed scale.
plot(transformed_model, which = 3, ask = FALSE, sub.caption = "")
