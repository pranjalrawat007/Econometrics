# Load the file
setwd('/Users/pranjal/Desktop/Statistical-Learning')
df = read.csv('/Users/pranjal/Desktop/Statistical-Learning/adv_data_analysis/data/lgtwgt2021yr4.csv')
head(df)
df = read.csv('/Users/pranjal/Desktop/Statistical-Learning/adv_data_analysis/data/lgtwgt2021yr4.csv',header=TRUE,sep="|")
head(df)

# Save and load the file
save(df,file='storage.rda')
load('storage.rda')


