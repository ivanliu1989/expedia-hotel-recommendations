setwd('/Users/ivanliu/Downloads/Expedia_kaggle/expedia-hotel-recommendations')
library(data.table)
train <- fread('../data/train.csv')
test <- fread('../data/test.csv')
destination <- fread('../data/destinations.csv')
submit <- fread('../data/sample_submission.csv')

######################
# Leakage Solution ###
######################
leaky_cols <- c('user_location_country', 'user_location_region', 'user_location_city', 'hotel_market', 'orig_destination_distance')
leaky_cols_2 <- c('user_location_city', 'orig_destination_distance')
target_col <- 'hotel_cluster'

train[,cnt := 1]
train[,leaky_1 := .N, by = c(leaky_cols,'is_booking',target_col)]
train[,leaky_2 := .N, by = c(leaky_cols,'is_booking',target_col)]
train_leaky <- train[,.(user_location_country, user_location_region, user_location_city, hotel_market, 
                        orig_destination_distance, hotel_cluster, is_booking, leaky_1, leaky_2)]
setkey(train_leaky)
train_leaky <- unique(train_leaky)
train_leaky <- train_leaky[!is.na(orig_destination_distance)]

train_leaky_1 <- train_leaky[is_booking == 1]
train_leaky_1 <- train_leaky_1[, is_booking := NULL]
test_leaky <- test[,.(id, user_location_country, user_location_region, user_location_city, hotel_market, orig_destination_distance)]

train_leaky_1 <- train_leaky_1[base:::order(user_location_country, user_location_region, user_location_city, hotel_market, orig_destination_distance, -hotel_cluster)]

predict <- data.frame(id = -1, hotel_cluster_1 = -1, hotel_cluster_2 = -1, hotel_cluster_3 = -1, hotel_cluster_4 = -1, hotel_cluster_5 = -1)
for(r in 1:nrow(test_leaky)){
    res <- train_leaky_1[user_location_city == test_leaky[r,user_location_city] & orig_destination_distance == test_leaky[r,orig_destination_distance]]
    if(nrow(res)>0){
        predict <- rbind(predict, c(test_leaky[r, id], res[1,hotel_cluster], res[2,hotel_cluster], res[3,hotel_cluster], res[4,hotel_cluster], res[5,hotel_cluster]))
        cat(paste0(r, ': ', res[,hotel_cluster], ' \n', res[,leaky_1], ' \n'))
    }
}