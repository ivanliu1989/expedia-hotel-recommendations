# expedia-hotel-recommendations

### feature engineering
1. entropy - hotel_country / hotel_market / hotel_cluster / month / weekday / is_package / channel / is_mobile / user_location
2. search date - checkin date
3. search / checkin weekdays, month, hours
4. sequenced features

### tips
1. 1 * is_booking results + 0.15 * not is_booking results => find popular cluster
2. calculate entropy of each cluster by customer
3. Finding similarity between users, then adjusting hotel cluster scores based on similarity.
4. Using similarity between destinations to group multiple destinations together.
5. Exploring the link between hotel clusters and regions more.