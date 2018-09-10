arsmooth <-
function(spg, ...){

    if( ! ("spg" %in% class(spg)) )
        return(invisible(spg));

    cix <- ncol(spg):1;

    for(i in 1:nrow(spg)){

        A <- ar(ts(spg[i,cix]), ...);

        smoov <- spg[i,cix] - A$resid;

        spg[i,cix] <- smoov;
    }

    invisible(spg);
}
