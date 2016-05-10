setwd('/Users/ivanliu/Downloads/Expedia_kaggle/expedia-hotel-recommendations')
library(data.table)
train <- fread('../data/train.csv')
test <- fread('../data/test.csv')
destination <- fread('../data/destinations.csv')
submit <- fread('../data/sample_submission.csv')

train

# 1. cluster distribution
    plot(table(train[,hotel_cluster]))

# 2. user ids
    table(unique(test[,user_id]) %in% unique(train[,user_id]))

# 3. time & dates
    train_sample <- train[is_booking == 1] # is_booking == 1
    train_sample[,c('srch_date', 'srch_time') := do.call(Map, c(f = c, strsplit(date_time, ' ')))]
    train_sample[,srch_date := as.IDate(srch_date)] #date
    train_sample[,srch_time := as.ITime(srch_time)] #time
    train_sample[,srch_month := month(srch_date)] #month
    train_sample[,srch_year := year(srch_date)] #year
    train_sample[,srch_qtr := quarter(srch_date)] #quarter
    train_sample[,srch_mday := mday(srch_date)] #monthdays
    train_sample[,srch_wday := wday(srch_date)] #weekdays
    train_sample[,srch_hour := hour(srch_time)] #hour
    
    train_sample[,srch_ci := as.IDate(srch_ci)] #srch_ci
    train_sample[,srch_ci_month := month(srch_ci)] #month
    train_sample[,srch_ci_year := year(srch_ci)] #year
    train_sample[,srch_ci_qtr := quarter(srch_ci)] #quarter
    train_sample[,srch_ci_mday := mday(srch_ci)] #monthdays
    train_sample[,srch_ci_wday := wday(srch_ci)] #weekdays
    
    train_sample[,srch_co := as.IDate(srch_co)] #srch_co
    train_sample[,srch_co_month := month(srch_co)] #month
    train_sample[,srch_co_year := year(srch_co)] #year
    train_sample[,srch_co_qtr := quarter(srch_co)] #quarter
    train_sample[,srch_co_mday := mday(srch_co)] #monthdays
    train_sample[,srch_co_wday := wday(srch_co)] #weekdays
    
    train_sample[,len_of_stay := as.numeric(srch_co - srch_ci)] #length of stay
    train_sample[,srch_in_advance := as.numeric(srch_ci - srch_date)] #search before stay

# 4. destinations features
    train_sample <- merge(train_sample, destination, by = 'srch_destination_id', all.x = T)
    dim(train_sample)

# 5. na values
    apply(train_sample, 2, FUN = function(x) mean(is.na(x)))
    train_sample[is.na(orig_destination_distance), orig_destination_distance := -1]
    train_sample[,orig_destination_distance]
    
    # d1-d149

# 6. data leak














# is_booking is always to be 1 in test
# test user ids is a subset of train user ids
