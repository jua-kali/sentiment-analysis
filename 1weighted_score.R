pacman::p_load(arules, data.table)


#### Load Data ####

dt_raw <- fread('data/large_matrix.csv')

dt_raw$V1 <- NULL
dt_raw$id <- NULL

#### Weight Dictionary ####
weightDict = list()

# device
weightDict['iphone'] = 0
weightDict['samsunggalaxy'] = 0

# OS
weightDict['ios'] = 0
weightDict['googleandroid'] = 0

# camera
weightDict['iphonecampos'] = 10
weightDict['samsungcampos'] = 10
weightDict['iphonecamneg'] = -10
weightDict['samsungcamneg'] = -10
weightDict['iphonecamunc'] = 1
weightDict['samsungcamunc'] = 1

# display
weightDict['iphonedispos'] = 10
weightDict['samsungdispos'] = 10
weightDict['iphonedisneg'] = -10
weightDict['samsungdisneg'] = -10
weightDict['iphonedisunc'] = 1
weightDict['samsungdisunc'] = 1

# device performance
weightDict['iphoneperpos'] = 10
weightDict['samsungperpos'] = 10
weightDict['iphoneperneg'] = -10
weightDict['samsungperneg'] = -10
weightDict['iphoneperunc'] = 1
weightDict['samsungperunc'] = 1

# OS performance
weightDict['iosperpos'] = 10
weightDict['googleperpos'] = 10
weightDict['iosperneg'] = -10
weightDict['googleperneg'] = -10
weightDict['iosperunc'] = 1
weightDict['googleperunc'] = 1

# Apply Weight Dictionary to Data #

dt <- dt_raw[, names(weightDict), with=FALSE]*weightDict

f0 <- dt_raw[iphone != 0 | samsunggalaxy !=0]


# dt[99076, , ]

bins <- c(-5000, -100, -1, 1.01, 100, 5000)

#### Calculate iPhone Score ####

iphone_score <- c('iphonecampos', 'iphonecamneg', 'iphonecamunc',
                  'iphonedispos', 'iphonedisneg', 'iphonedisunc',
                  'iphoneperpos', 'iphoneperneg', 'iphoneperunc',
                  'iosperpos', 'iosperneg', 'iosperunc')


dt_i_score <- dt[, iphone_score, with=FALSE]
dt_i_score$iphonesentiment <- rowSums(dt_i_score)

dt_i_score$iphonebins <- discretize(dt_i_score$iphonesentiment,
                                     method="fixed",
                                     breaks= bins,
                                     labels = c("very negative", "negative", "neutral", "positive", "very positive")
                                    )

a <- table(dt_i_score$iphonebins)


esquisse::esquisser()

boxplot(dt_i_score$iphonesentiment)

#### Calculate Samsung Score ####
sam_score <- c('samsungcampos', 'samsungcamneg', 'samsungcamunc',
                  'samsungdispos', 'samsungdisneg', 'samsungdisunc',
                  'samsungperpos', 'samsungperneg', 'samsungperunc',
                  'googleperpos', 'googleperneg', 'googleperunc')


dt_s_score <- dt[, sam_score, with=FALSE]
dt_s_score$samsungsentiment <- rowSums(dt_s_score)


dt_s_score <- dt_s_score[samsungsentiment != 0 ]

dt_s_score$samsungbins <- discretize(dt_s_score$samsungsentiment,
                                     method="fixed",
                                     breaks= bins,
                                     labels = c("very negative", "negative", "neutral", "positive", "very positive")
                                     )
                                     

esquisse::esquisser()

boxplot(dt_s_score$samsungsentiment)

b <- table(dt_s_score$samsungbins)

b

#### Sandbox ####
DT = data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
