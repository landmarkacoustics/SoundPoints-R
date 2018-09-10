spectrogram <-
function(path, time.res = 1/1000, freq.res = 100){

    wav <- readWave(path);

    sample.rate <- wav@samp.rate;

    W <- 2^ceiling(log(sample.rate,2) - log(freq.res,2) - 1);

    span <- -(W - 1):W;

    S <- floor(time.res * sample.rate);

    wav <- wav@left / 2^(wav@bit - 1); # this makes it range from 0-1

    t.max <- length(wav);

    N <- 1 + floor(t.max / S);

    spg <- matrix(NA, N, W+1);

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

        spg[i,] <- spk(win);
    }

    attributes(spg) <- c(attributes(spg), list(time = 0:(N-1) * S / sample.rate, frequency = sample.rate * 0:W / (2 * W), RMS = rms, envelope = envelope));

    class(spg) <- c("spg", class(spg));

    invisible(spg);
}
