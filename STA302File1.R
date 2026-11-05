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
life_data <- read_dta("p22i6.dta")
trimmed_data <- life_data %>%
  select(y1, yy1, x4002,x4003) %>% filter(x4002 == 1,x4003 > 0)

saveRDS(trimmed_data,"~/STA302Project/trimmed_data.rds")
View(trimmed_data) #this is the dataset w/ info on everyone w/ term insurance

scf_summary <- read.csv("SCFP2022.csv") #dataset for all else
View(scf_summary)

names(scf_summary) <- tolower(names(scf_summary)) #make columns lowercase


# only merges rows where both share same y1 and yy1 value 
# we only use p22i6 for term insurance, scfp2022 for all else
ultimate_data <- left_join(trimmed_data,scf_summary, by = c("y1", "yy1"))
ultimate_data$implicate <- ultimate_data$y1 %% 10

#filter to get indicate_1 so we do not overcount 
implicate_1 <- filter(ultimate_data, implicate == 1)

#use ultimate_data for MLR modeling
View(implicate_1)


model <- lm(x4003 ~ income + debt + age + kids + married + housecl + edcl + asset +
   lf, data = implicate_1)

summary(model)
