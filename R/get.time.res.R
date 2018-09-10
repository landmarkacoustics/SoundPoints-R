get.time.res <-
function(spg){
    return(diff(attr(spg,"time")[1:2]));
}
