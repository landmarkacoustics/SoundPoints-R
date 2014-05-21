get.freq.res <-
function(spg){
    return(diff(attr(spg,"frequency")[1:2]));
}
