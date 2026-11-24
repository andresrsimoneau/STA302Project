source("statistical modeling.R")

plot_diagnostics <- function(fit, data, label) {
  old <- par(mfrow = c(2, 3), mar = c(4, 4, 2.5, 1), oma = c(0, 0, 2, 0))
  on.exit(par(old))
  for (which_plot in c(1, 2, 3, 5)) {
    plot(fit, which = which_plot, id.n = 0, ask = FALSE, sub.caption = "",
         pch = 16, cex = .45, col = "#24496765")
  }
  r <- rstandard(fit)
  for (g in levels(data$children)) {
    use <- data$children == g
    x <- log10(1 + data$norminc[use]/1000)
    plot(x, r[use], pch = 16, cex = .45, col = "#24496765",
         main = paste("Listed children:", g), xlab = "Usual income (2025 USD)",
         ylab = "Standardized residual", xaxt = "n")
    ticks <- c(0, 5e4, 5e5, 5e6, 5e7)
    axis(1, at = log10(1+ticks/1000), labels = c("0","50k","500k","5M","50M"))
    abline(h = 0, lty = 2)
    lines(lowess(x, r[use]), col = "#A5481B", lwd = 2)
  }
  mtext(label, outer = TRUE, font = 2)
}
plot_diagnostics(final_model, analysis_data, "Final model: shifted-log income")
plot_diagnostics(sensitivity_model, analysis_data, "Sensitivity: cube-root income")
