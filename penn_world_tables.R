# Data Analysis on Penn World Tables

# Load worksheet into table
library("readxl")
wb = read_excel('pwt100.xlsx',
                sheet = 'Data',
                col_names = TRUE, 
                na = "NA")


legend = read_excel('pwt100.xlsx',
                sheet = 'Legend',
                col_names = TRUE, 
                na = "NA")

# Load table into dataframe
install.packages("dplyr")
library("dplyr")
df = data.frame(wb)

# Peeking
head(df) 
# View(df)
dim(df)
glimpse(df)

# Subset
df_ind = filter(df, country=='India')
df_ind = select(df_ind, year, rgdpo, pop, emp, hc)
dim(df_ind)

# Summary
summary(df_ind) # summary stats
colSums(is.na(df_ind)) # null counts
#count(df_ind, year) distinct counts per category

# New Cols

df_ind = mutate(df_ind, rgdp_pc = rgdpo/pop)
head(df_ind)
lag(df_ind.year)
