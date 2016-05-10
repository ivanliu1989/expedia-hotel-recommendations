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
    append_1 <- 3 + 17 * is_booking
    append_2 <- 1 + 5 * is_booking
    best_hotels_od_ulc <- data.frame(user_location_city = -1, orig_destination_distance = -1, hotel_cluster = -1, score = -1)
    best_hotels_search_dest <- data.frame(srch_destination_id = -1, hotel_country = -1, hotel_market = -1, hotel_cluster = -1, score = -1)
    best_hotels_search_dest1 <- data.frame(srch_destination_id = -1, hotel_cluster = -1, score = -1)
    best_hotel_country <- data.frame(hotel_country = -1, hotel_cluster = -1, score = -1)
    popular_hotel_cluster <- data.frame(hotel_cluster = -1, score = -1)
    
    # train mapping
    for(r in 1:nrow(train)){
        if(r %% 100000 == 0) cat(paste0(i, ' /n'))
        book_year <- train_sample[r, srch_year]
        user_location_city <- train_sample[r, user_location_city]
        orig_destination_distance <- train_sample[r, orig_destination_distance]
        srch_destination_id <- train_sample[r, srch_destination_id]
        is_booking <- train_sample[r, is_booking]
        hotel_country <- train_sample[r, hotel_country]
        hotel_market <- train_sample[r, hotel_market]
        hotel_cluster <- train_sample[r, hotel_cluster]
        
        # 6.1 best_hotels_od_ulc
        if(!is.na(user_location_city)&!is.na(orig_destination_distance)) 
            best_hotels_od_ulc <- rbind(best_hotels_od_ulc, c(user_location_city, orig_destination_distance, hotel_cluster, 1))
        
        # 6.2 best_hotels_search_dest
        if(!is.na(srch_destination_id)&!is.na(hotel_country)&!is.na(hotel_market)&book_year==2014) 
            best_hotels_search_dest <- rbind(best_hotels_search_dest, c(srch_destination_id, hotel_country, hotel_market, hotel_cluster, append_1))
        
        # 6.3 best_hotels_search_dest1
        if(!is.na(srch_destination_id)) 
            best_hotels_search_dest1 <- rbind(best_hotels_search_dest1, c(srch_destination_id, hotel_cluster, append_1))
        
        # 6.4 best_hotel_country
        if(!is.na(hotel_country)) 
            best_hotel_country <- rbind(best_hotel_country, c(hotel_country, hotel_cluster, append_2))
        
        popular_hotel_cluster <- rbind(popular_hotel_cluster, c(hotel_cluster, 1))
    }
    best_hotels_od_ulc <- best_hotels_od_ulc[best_hotels_od_ulc$orig_destination_distance != -1, ]
    best_hotels_od_ulc <- as.data.table(best_hotels_od_ulc)
    best_hotels_od_ulc2 <- best_hotels_od_ulc[, score := sum(score), by = .(user_location_city, orig_destination_distance, hotel_cluster)]
    
    # test mapping
    submission <- data.frame(id = -1, hotel_cluster = -1)
    for(r in 1:nrow(test)){
        if(r %% 100000 == 0) cat(paste0(i, ' /n'))
        user_location_city <- train_sample[r, user_location_city]
        orig_destination_distance <- train_sample[r, orig_destination_distance]
        srch_destination_id <- train_sample[r, srch_destination_id]
        hotel_country <- train_sample[r, hotel_country]
        hotel_market <- train_sample[r, hotel_market]
        
        # s1 <- c(user_location_city, orig_destination_distance)
        pred_s1 <- best_hotels_od_ulc[best_hotels_od_ulc$user_location_city == user_location_city & best_hotels_od_ulc$orig_destination_distance == orig_destination_distance, ]
        
        # s2 <- c(srch_destination_id, hotel_country, hotel_market)
        pred_s2 <- best_hotels_search_dest[best_hotels_search_dest$srch_destination_id == srch_destination_id & best_hotels_search_dest$hotel_country == hotel_country & best_hotels_search_dest$hotel_market == hotel_market, ]
        pred_s2_2 <- best_hotels_search_dest1[best_hotels_search_dest1$srch_destination_id == srch_destination_id, ]
        
        # s3 <- hotel_country
        pred_s3 <- best_hotel_country[best_hotel_country$hotel_country == hotel_country, ]
        
        # final prediction
        pred <- c(pred_s1, pred_s2, pred_s2_2, pred_s3)[1:5]
        submission <- rbind(submission, pred)
    }
    

# is_booking is always to be 1 in test
# test user ids is a subset of train user ids
