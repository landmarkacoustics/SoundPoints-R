sample.rate.spg <-
function(spg){

    ifelse(is(spg) == "spg", diff(attr(spg,"frequency")[1:2])*ncol(spg), NaN);
}
