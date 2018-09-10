cepstrum <-
function(spg){

    if(is(spg) == "spg"){

        sample.rate <- sample.rate.spg(spg);

        NR <- nrow(spg);

        NC <- ncol(spg);
        
        cpg <- matrix(complex(1), NR, 2 * NC);

#        H <- hamming(ncol(spg));
        
        for(i in 1:NR){
            cpg[i,] <- fft( c(spg[i,], rev(spg[i,])), T ); # scale of 10 bc of decibels in spk()
        }

        attributes(cpg) <- c(attributes(cpg), list(time = attr(spg, "time"), frequency = 1:NC / sample.rate));

        class(cpg) <- c("cepstrum", class(cpg));

        invisible(cpg);
    }
}
