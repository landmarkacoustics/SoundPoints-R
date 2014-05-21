cepstrum <-
function(spg){

    if(is(spg) == "spg"){

        sample.rate <- sample.rate.spg(spg);

        NR <- nrow(spg);

        ix <- (ncol(spg) / 2):1;

        cpg <- matrix(NA, NR, length(ix));

        H <- hamming(ncol(spg));
        
        for(i in 1:NR){

            cpg[i,] <- spk(scale(spg[i,],scale=F)*H);
        }

        attributes(cpg) <- c(attributes(cpg), list(time = attr(spg, "time"), quefrency = sample.rate / ix));

        class(cpg) <- c("cepstrum", class(cpg));

        invisible(cpg);
    }
}
