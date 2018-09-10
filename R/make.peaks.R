make.peaks <-
function(spg, passband, amplitude.cutoff){

    freqs <- attr(spg,"frequency");

    g <- freqs > min(passband);

    below <- sum(!g);

    g <- g & freqs < max(passband);

    peak.values <- data.frame(t(apply(spg[,g], 1, collect.energy, amplitude.cutoff)));

    names(peak.values) <- c("lo","hi","sum");

    peak.values[,1:2] <- peak.values[,1:2] + below;

    peaks <- data.frame(time = attr(spg,"time"),
                        min.frequency = freqs[peak.values$lo],
                        max.frequency = freqs[peak.values$hi],
                        amplitude.sum = peak.values$sum);

    for(i in 1:nrow(peaks)){
        ix <- peak.values$lo[i]:peak.values$hi[i];
        amps <- spg[i,ix];
        peaks$mean.frequency[i] <- weighted.mean(freqs[ix], amps);
        peaks$mean.amplitude[i] <- mean(amps);
        tmp <- range(amps);
        peaks$max.amplitude[i] <- tmp[2];
        peaks$amplitude.range[i] <- diff(tmp);
    }

    return(invisible(peaks))
}
