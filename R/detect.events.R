detect.events <-
function(X, var.weights){

    events <- rep(1, nrow(X));

    for(j in 1:ncol(X))
        X[,j] <- X[,j] * var.weights[j];

    for(i in 2:nrow(X)){
        events[i] <- events[i-1];
        delta <- sum((X[i,] - X[i-1,])^2)
        if(delta>1)
            events[i] <- events[i]+1;
    }
    return(invisible(events));
}
