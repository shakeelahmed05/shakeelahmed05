library(dplyr)
# Create two example data frames
df1 <- data.frame(ID = c(1, 2, 3, 4),
                  Name = c("Ali", "Chahat", "Danial", "Eshal"))
df1
df2 <- data.frame(ID = c(2, 3, 4, 5),
                  Age = c(30, 25, 40, 35),
                  Gender = c("Male", "Female", "Male", "Female"))
df2
# Perform a simple inner join on the "ID" column using dplyr
merged_df_simple <- inner_join(df1, df2, by = "ID")


