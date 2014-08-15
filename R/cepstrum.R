cepstrum <-
function(spg){

    if(is(spg) == "spg"){

        sample.rate <- sample.rate.spg(spg);

        NR <- nrow(spg);

        NC <- ncol(spg) / 2;
        
        ix <- NC + NC:1;

        cpg <- matrix(NA, NR, length(ix));

#        H <- hamming(ncol(spg));
        
        for(i in 1:NR){
            cpg[i,] <- spk( scale(spg[i,], T, 10) ); # scale of 10 bc of decibels in spk()
        }

        attributes(cpg) <- c(attributes(cpg), list(time = attr(spg, "time"), frequency = 1:NC / sample.rate));

        class(cpg) <- c("cepstrum", class(cpg));

        invisible(cpg);
    }
}
