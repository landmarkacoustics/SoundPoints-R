spectrogram <-
function(path, time.res = 1/1000, freq.res = 100){

    require(tuneR);

    wav <- readWave(path);

    sample.rate <- attr(wav, "samp.rate");

    W <- 2^ceiling(log(sample.rate,2) - log(freq.res,2) - 1);

    span <- -(W - 1):W;

    S <- floor(time.res * sample.rate);

    wav <- as.numeric(attr(wav,"left"));

    t.max <- length(wav);

    N <- 1 + floor(t.max / S);

    spg <- matrix(NA, N, W);

    envelope <- matrix(NA, N, 2);

    rms <- numeric(N);

    H <- hamming(2 * W);

    for(i in 1:N){

        ix <- S * (i - 1) + span;

        ix[ix < 1 | ix > t.max] <- NA;

        envelope[i,] <- range(wav[ix], na.rm = T);

        win <- H * wav[ix];

        win[is.na(win)]	<- 0;

        rms[i] <- sqrt(mean(win^2));

        spg[i,] <- log(spk(win),10);
    }

    attributes(spg) <- c(attributes(spg), list(time = 0:(N-1) * S / sample.rate, frequency = sample.rate * 0:(W-1) / (2 * W), RMS = rms, envelope = envelope));

    class(spg) <- c("spg", class(spg));

    invisible(spg);
}