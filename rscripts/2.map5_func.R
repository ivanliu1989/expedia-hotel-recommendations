library(Metrics)
map5 <- function(preds, dtrain) {
    labels <- as.list(getinfo(dtrain,"label"))
    num.class = 100
    pred <- matrix(preds, nrow = num.class)
    top <- t(apply(pred, 2, function(y) order(y)[num.class:(num.class-4)]-1))
    top <- split(top, 1:NROW(top))
    
    map <- mapk(5, labels, top)
    return(list(metric = "map5", value = map))
}


map5_2 <- function(preds, dtrain) {
    labels = getinfo(dtrain, 'label')
    preds = t(matrix(preds, ncol = length(labels)))
    preds = t(apply(preds, 1, order, decreasing = T))[, 1:5] - 1
    succ = (preds == labels)
    w = 1 / (1:5)
    map5 = mean(succ %*% w)
    return (list(metric = 'map5', value = map5))
}