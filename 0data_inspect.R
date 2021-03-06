pacman::p_load(plotly, data.table, reshape2, ggplot2, dplyr, 
               caret, caretEnsemble, doParallel, corrplot)

# Read as a data table
# However, caret seems to struggle with data table input
dt_all <- fread('data/iphone_smallmatrix_labeled_8d.csv')

str(dt_all)
summary(dt_all)  #Boxplot is the visual version of this

dt <- dt_all

# Set target as factor for classification
dt$iphonesentiment <- as.factor(dt$iphonesentiment)


#### Histogram target variable #### ---------------------------------------

plot_ly(dt, x= ~dt$iphonesentiment, type='histogram')

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

df <- setDF(dt)
# Remove near zero variance variable
iphoneNZV <- df[, -nzv]


#### Recursive Feature Elimination ####
rfe_df <- df

# Sample data
set.seed(13)
iphoneSample <-rfe_df[sample(1:nrow(rfe_df), 1000, replace = FALSE), ]

registerDoParallel(2)
getDoParWorkers()

# Set up rfeControl with randomforest
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedCV",
                   repeats = 5,
                   verbose = FALSE,
                   allowParallel = TRUE)



vars <- length(iphoneSample)

rfeResults <- rfe(iphoneSample[, 1:(vars -1)], 
                  iphoneSample$iphonesentiment,
                  sizes=(1:(vars-1)),
                  rfeControl=ctrl)

rfeResults

# saveRDS(rfeResults, "models/rfe_full.rds")
# test <- readRDS("models/rfe_full.rds")

# Plot results
plot(rfeResults, type=c("g", "o"))

# Create a new dataset with recommended predictors
iphoneRFE <- setDF(dt_all)[, predictors(rfeResults)]

# Add dependent variable
iphoneRFE$iphonesentiment <- dt_all$iphonesentiment
str(iphoneRFE)

# In this case, the best number is 10, but it's because 
# I had reduced already

# # create new data set with rfe recommended features
# iphoneRFE <- iphoneDF[,predictors(rfeResults)]
# 
# # add the dependent variable to iphoneRFE
# iphoneRFE$iphonesentiment <- iphoneDF$iphonesentiment
# 
# # review outcome
# str(iPhoneRFE)



#### Set up model variables ####
tar_name <- paste0(phone, 'sentiment')

# Choose which dataframe to work with
# Caret doesn't seem to deal well with data tables!

df <- setDF(dt_all) #iphoneRFE
df$iphonesentiment <- as.factor(df$iphonesentiment)

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

#### PCA #### 


#### Modelling #### -----------------------------------------------


# Set up PCA with 90% of variance captured in model
preprocessParams <- preProcess(X_train, 
                               method=c("center", "scale", "pca"), 
                               thresh = 0.9)

print(preprocessParams)

# Create PCA predictors to train/test model
X_train_pca <- predict(preprocessParams, X_train)
X_test_pca <- predict(preprocessParams, X_test)


control <- trainControl(method = 'cv',
                        number = 5,
                        index = createFolds(y_train, 5),
                        savePredictions = "final",
                        allowParallel = TRUE)

model_list <- caretList(x=X_train_pca, y=y_train,
                        trControl = control,
                        methodList = c( 'rf', 'knn'),  #'lm'
                        tuneList = NULL,
                        continue_on_fail = FALSE) 
                        # preProcess = c('center', 'scale'))


model_list$rf
model_list$knn


# Ground truth with test set
pred_rf <- predict(model_list$rf, X_test_pca)
confusionMatrix(pred_rf, y_test)
# Error metrics
postResample(pred_rf, y_test)



#### Sandbox #### ------------------------------------------------------------


