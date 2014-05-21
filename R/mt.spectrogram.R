mt.spectrogram <-
function(path, time.res = 1/1000, freq.res = 100){

    require(sound);

    require(multitaper);

    wav <- loadSample(path);

    sample.rate <- wav$rate;

    W <- 2^ceiling(log(sample.rate,2) - log(freq.res,2) - 1);

    span <- 1:W;

    S <- floor(time.res * sample.rate);

    wav <- as.numeric(wav$sound);

    t.max <- length(wav);

    N <- 1 + floor(t.max / S);

    spg <- matrix(NA, N, W + 1);

    envelope <- matrix(NA, N, 2);

    rms <- numeric(N);

    DPSS <- dpss(W, k = 4, nw = 4);

    for(i in 1:N){

        ix <- S * (i - 1) + span;

        ix[ix < 1 | ix > t.max] <- NA;

        envelope[i,] <- range(wav[ix], na.rm = T);

        win <- wav[ix];

        win[is.na(win)]	<- 0;

        win <- ts(win,
                  start = ix[1]/sample.rate,
                  deltat= 1/sample.rate);


        rms[i] <- sqrt(mean(win^2));

        tmp <- spec.mtm(win, plot=F, k = 4, nw = 4, dpssIN = DPSS, log = "n");

        spg[i,] <- tmp$spec;
    }

    attributes(spg) <- c(attributes(spg), list(time = 0:(N-1) * S / sample.rate, frequency = tmp$freq, RMS = rms, envelope = envelope));

    class(spg) <- c("spg", class(spg));

    invisible(spg);
}
