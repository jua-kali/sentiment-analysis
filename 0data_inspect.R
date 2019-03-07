pacman::p_load(plotly, data.table, reshape2, ggplot2, dplyr)

dt_all <- fread('data/iphone_smallmatrix_labeled_8d.csv')

str(dt)
summary(dt)

plot_ly(dt, x= ~df$iphonesentiment, type='histogram')



#### Check for zero rows #### ---------------------------------------------
phone <- "iphone"

dtP <- dt %>% dplyr:: select(starts_with(phone))

dt <- dtP

# Total observations = 12973
nrow(dt)

# Number of rows that are all zero = 0
nrow(dt[rowSums(dt) == 0])

# Drop the phone column
f0 <- select(dt, -c(phone))

# Rows with no value = 592
nrow(dt[rowSums(f0) == 0])

# Total columns = 59
ncol(dt)

# Number of columns with zero values = 0
sum(colSums(dt) == 0)

# Number of NA values = 0
sum(is.na(dt))

# Max values
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMax(dt)

#### Boxplot #### ------------------------------------------------------------
# Two columns with variable and value
dt2 <- melt(dt)

plot_ly(dt2, x = ~variable, y = ~value, type ="box") 


#### CDF plot #### -----------------------------------------------------------
p <- ggplot(dt2, aes(value, color = variable)) + stat_ecdf(geom = "point")
p
ggplotly(p)



#### Sandbox #### ------------------------------------------------------------
