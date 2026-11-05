# Open STA302Project.Rproj, then run this file in RStudio.
# Five figures: four core grids and one influence grid.
# You can run one numbered section at a time.
# Keep the full-range plots; only the overall grid includes a zoom.
source("statistical modeling.R")

# Amounts are displayed in millions of 2025 dollars.
# These plotting variables do not change the fitted model.
residuals <- resid(model) / 1000000
fitted_values <- fitted(model) / 1000000
coverage <- analysis_data$term_coverage_2025 / 1000000
income <- analysis_data$norminc / 1000000
debt <- analysis_data$debt / 1000000
assets <- (analysis_data$fin - analysis_data$cashli) / 1000000
age <- analysis_data$age
child_colour <- ifelse(analysis_data$children == "Yes", "orange3", "steelblue")

# 1. Core overall grid: three full-range checks and one useful zoom.
par(mfrow = c(2, 2), mar = c(6, 4.5, 3, 1))
plot(fitted_values, residuals,
     xlab = "Fitted coverage ($millions)", ylab = "Residual ($millions)",
     main = "Residuals vs fitted: full range")
abline(h = 0, col = "red")
lines(lowess(fitted_values, residuals), col = "blue")

qqnorm(rstandard(model), main = "Normal Q-Q", ylab = "Standardized residual")
qqline(rstandard(model), col = "red")

plot(fitted_values, coverage,
     xlab = "Fitted coverage ($millions)",
     ylab = "Observed coverage ($millions)", main = "Observed vs fitted: full range")
abline(0, 1, col = "red")

# Only the displayed limits change; the model and smoother use all families.
plot(fitted_values, residuals,
     xlim = c(min(fitted_values), quantile(fitted_values, 0.95)),
     ylim = quantile(residuals, c(0.01, 0.99)),
     xlab = "Fitted coverage ($millions)", ylab = "Residual ($millions)",
     main = "Residuals vs fitted: zoom",
     sub = "x: min-95th percentile; y: 1st-99th")
abline(h = 0, col = "red")
lines(lowess(fitted_values, residuals), col = "blue")

# 2. Residuals against numeric predictors: full range
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(income, residuals, col = child_colour,
     xlab = "Usual income ($millions)", ylab = "Residual ($millions)",
     main = "Residuals vs income")
abline(h = 0, col = "red")
legend("topright", c("No children", "Children"),
       col = c("steelblue", "orange3"), pch = 1, bty = "n", cex = 0.8)

plot(debt, residuals,
     xlab = "Debt ($millions)", ylab = "Residual ($millions)",
     main = "Residuals vs debt")
abline(h = 0, col = "red")

plot(age, residuals,
     xlab = "Age (years)", ylab = "Residual ($millions)",
     main = "Residuals vs age")
abline(h = 0, col = "red")

plot(assets, residuals,
     xlab = "Financial assets excluding cash value ($millions)",
     ylab = "Residual ($millions)", main = "Residuals vs assets")
abline(h = 0, col = "red")

# 3. Residuals against categorical predictors: full range
par(mfrow = c(2, 3), mar = c(6, 4, 3, 1))
boxplot(residuals ~ analysis_data$children,
        xlab = "Children", ylab = "Residual ($millions)", main = "Child presence")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$partnered,
        xlab = "Partnered", ylab = "Residual ($millions)", main = "Partnership")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$education,
        names = c("Below bachelor", "Bachelor+"),
        cex.axis = 0.7, xlab = "Education", ylab = "Residual ($millions)", main = "Education")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$employment,
        names = c("Not working", "Working"),
        cex.axis = 0.7, xlab = "Employment", ylab = "Residual ($millions)", main = "Employment")
abline(h = 0, col = "red")

boxplot(residuals ~ analysis_data$homeowner,
        xlab = "Homeowner", ylab = "Residual ($millions)", main = "Homeownership")
abline(h = 0, col = "red")
plot.new() # Leave the sixth panel empty.

# 4. Relationships between numeric predictors: full range
# Blue = no children; orange = children. Assets exclude insurance cash value.
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
plot(income, debt, col = child_colour,
     xlab = "Income ($millions)", ylab = "Debt ($millions)", main = "Income and debt")
legend("topright", c("No children", "Children"),
       col = c("steelblue", "orange3"), pch = 1, bty = "n", cex = 0.8)
plot(income, age, col = child_colour,
     xlab = "Income ($millions)", ylab = "Age (years)", main = "Income and age")
plot(income, assets, col = child_colour,
     xlab = "Income ($millions)", ylab = "Financial assets ($millions)", main = "Income and assets")
plot(debt, age, col = child_colour,
     xlab = "Debt ($millions)", ylab = "Age (years)", main = "Debt and age")
plot(debt, assets, col = child_colour,
     xlab = "Debt ($millions)", ylab = "Financial assets ($millions)", main = "Debt and assets")
plot(age, assets, col = child_colour,
     xlab = "Age (years)", ylab = "Financial assets ($millions)", main = "Age and assets")

# 5. Compact influence grid: retain because several records are influential.
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 2))
plot(model, which = 5, ask = FALSE)
plot(model, which = 4, ask = FALSE)

# Return to one plot per page for subsequent plotting.
par(mfrow = c(1, 1))

# Optional additional check; not drawn automatically because it overlaps
# with the residual-versus-fitted plot. Run this line separately if needed:
# plot(model, which = 3)  # Scale-location plot
