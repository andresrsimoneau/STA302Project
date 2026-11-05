library(haven)
library(dplyr)
# pls be patient w/ file size, it is very large (226mb) 
# the p22i6.dta file is in the scf2022s.zip, unpack it
# and place it in the file folder so the model functions properly 

# use SCFP2022.csv as it is w/o change inside the file folder
# this R file is for cleaning purposes, statstical modeling R does the 
# MLR modeling 


# The x codes refer to specific columns from the p22i6 file, 
# do NOT edit them, it will otherwise mess up the file, data, and model entirely 

# reference for p22i6 file, 
# x4002 column is boolean if we have term insurance (1=yes, 5=no)
# x4003 column is amount of household term insurance covered


# we filtered this file to only take households w/ term insurance
# dta file is used since U.S. gov didn't have csv file
life_data <- read_dta("p22i6.dta")
trimmed_data <- life_data %>%
  select(y1, yy1, x4002,x4003) %>% filter(x4002 == 1,x4003 > 0)

# saves trimmed_data to the folder
saveRDS(trimmed_data,"~/STA302Project/trimmed_data.rds")

scf_summary <- read.csv("SCFP2022.csv") #dataset for all else

names(scf_summary) <- tolower(names(scf_summary)) #make columns lowercase


# only merges rows where both share same y1 and yy1 value 
# we only use p22i6 for term insurance, scfp2022 for all else
ultimate_data <- left_join(trimmed_data,scf_summary, by = c("y1", "yy1"))
ultimate_data$implicate <- ultimate_data$y1 %% 10

# filter to get indicate_1 so we do not overcount 
implicate_1 <- filter(ultimate_data, implicate == 1)

# saves implicate_1 to the folder 
saveRDS(implicate_1, "modeling_data.rds")
