pacman::p_load(plotly, data.table, reshape2, ggplot2, dplyr, 
               caret, caretEnsemble, doParallel)

dt_all <- fread('data/iphone_smallmatrix_labeled_8d.csv')

str(dt)
summary(dt)

dt <- dt_all

#### Histogram target variable #### ---------------------------------------

plot_ly(dt, x= ~dt$iphonesentiment, type='histogram')

#### MVP Model #### -----------------------------------------------
phone <- "iphone"
set.seed(42)

dt <- dt_all %>% dplyr:: select(starts_with(phone))

tar_name <- paste0(phone, 'sentiment')

rm(X)

# Select all non-target columns
# Convert to data frame, caret doesn't seem to deal well with data tables
X <- dt %>% select(-c(tar_name)) %>% setDF()
y <- dt %>% select(c(tar_name)) %>% setDF()



part <- createDataPartition(dt[, get(tar_name)], 
                            p = 0.75,
                            list = FALSE)
X_train <- X[part, ]
X_test <- X[-part, ]
y_train <- y[part, ]
y_test <- y[-part, ]

str(X_train)
str(X_test)
str(y_train)
str(y_test)

registerDoParallel(2)
getDoParWorkers()


control <- trainControl(method = 'cv',
                        number = 5,
                        savePredictions = "final",
                        allowParallel = TRUE)

model_list <- caretList(x=X_train, y=y_train,
                        trControl = control,
                        methodList = c('lm', 'rf'),
                        tuneList = NULL,
                        continue_on_fail = FALSE)
                        # preProcess = c('center', 'scale'))

#### Check for zero rows #### ---------------------------------------------


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

#### Histogram Plot Target ####


#### Sandbox #### ------------------------------------------------------------
