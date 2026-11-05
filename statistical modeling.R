library(dplyr)

# Load prepared dataset
implicate_1 <- readRDS("modeling_data.rds")
View(implicate_1)

# multiple linear regression model for graphing
model <- lm(
  x4003 ~ income + debt + age + kids +
    married + housecl + edcl + asset + lf,
  data = implicate_1
)

summary(model)

# INCOME
# DEBT
#AGE
#kIDS (indicator; data must be filtered)
#- 1 = kids, 2 = no kids
#MARRIED (indicator)
#- 1 = married, 2 = not married
#home ownership - HOUSECL (indicator)
#- 1 = owns home, 2 = does not own home
#Education - EDCL
#- 1 = no HS diploma/GED
#- 2 = HS diploma/GED
#- 3 = some college
#- 4 = college degree
#Assets (excluding insurance value)
#Labor force participation of reference person (LF; indicator) 
#- 0 = not working, 1 = working