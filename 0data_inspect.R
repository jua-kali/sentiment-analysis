pacman::p_load(plotly, data.table, reshape2, ggplot2, dplyr, 
               caret, caretEnsemble, doParallel, corrplot)

dt_all <- fread('data/iphone_smallmatrix_labeled_8d.csv')

str(dt_all)
summary(dt_all)  #Boxplot is the visual version of this

dt <- dt_all

#### Histogram target variable #### ---------------------------------------

plot_ly(dt, x= ~dt$iphonesentiment, type='histogram')


#### Filter Data Table to Phone #### ----------------------------------------------------------------
phone <- "iphone"
set.seed(42)

dt <- dt_all %>% dplyr:: select(starts_with(phone))
df <- setDF(dt)

#### Correlations ####
corrplot(cor(dt), method="number", type = "upper")

str(cor(dt))

corrplot.mixed(cor(dt))

names(dt)

keep <- c("iphonesentiment", 
          "iphonecamneg", "iphonecampos",
          "iphoneperunc", "iphonedisunc", 
          "iphonedispos", "iphone")
dt <- dt[, keep, with=FALSE]
corrplot.mixed(cor(dt))



#### Zero variance ####
nzvMetrics <- nearZeroVar(dt, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(dt, saveMetrics = FALSE)
nzv

# Remove near zero variance variable
iphoneNZV <- df[, -nzv]



#### Set up model variables ####
tar_name <- paste0(phone, 'sentiment')

# Convert to data frame, caret doesn't seem to deal well with data tables
# df <- setDF(dt)

# Select all non-target columns
X <- df %>% select(-c(tar_name)) 
y <- df %>% select(c(tar_name)) 


part <- createDataPartition(df[, tar_name], 
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

#### MVP Model #### -----------------------------------------------

control <- trainControl(method = 'cv',
                        number = 5,
                        index = createFolds(y_train, 5),
                        savePredictions = "final",
                        allowParallel = TRUE)

model_list <- caretList(x=X_train, y=y_train,
                        trControl = control,
                        methodList = c( 'rf', 'knn'),  #'lm'
                        tuneList = NULL,
                        continue_on_fail = FALSE)
                        # preProcess = c('center', 'scale'))


model_list$rf
model_list$knn


#### Recursive Feature Elimination ####

# Sample data
iphoneSample <-df[sample(1:nrow(df), 1000, replace = FALSE), ]

# Set up rfeControl with randomforest
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedCV",
                   repeats = 5,
                   verbose = FALSE,
                   allowParallel = TRUE)

names(iphoneSample)
vars = length(iphoneSample)

rfeResults <- rfe(iphoneSample[, 1:(vars -1)], 
                  iphoneSample$iphonesentiment,
                  sizes=(1:(vars-1)),
                  rfeControl=ctrl)

rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

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






#### Sandbox #### ------------------------------------------------------------
