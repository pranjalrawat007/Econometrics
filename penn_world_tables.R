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
# df2 = filter(df, country=='Germany')
df = select(df, country, year, rgdpo, pop, emp, hc)
dim(df)

# Summary
summary(df2) # summary stats
colSums(is.na(df2)) # null counts

# Time sieres 
df2 = mutate(df2, rgdp_pc = (rgdpo-lag(rgdpo))*100/lag(rgdpo))
df2 = mutate(df2, pop_pc = (pop-lag(pop))*100/lag(pop))
df2 = mutate(df2, emp_pc = (emp-lag(emp))*100/lag(emp))

# Extract single column
df_year = df$year

df %>% rownames_to_column() %>% as_tibble()

install.packages("ggplot")
library(ggplot)

