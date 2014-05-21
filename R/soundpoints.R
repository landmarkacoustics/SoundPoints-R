soundpoints <-
function(file.name, time.res, freq.res,                  # for fft
             passband, amplitude.cutoff,                     # for peak extraction
             loudness.column, half.life, initial.amplitude,  # for moving average
             amplitude.threshold, var.columns, var.weights,  # for event detection
             n.landmarks                                     # for landmark creation
             ){

    spg <- spectrogram(file.name,
                       time.res,
                       freq.res);

    peaks <- make.peaks(spg,
                        passband,
                        amplitude.cutoff);

    peaks$smooth.amplitude <- smooth.average(peaks[,loudness.column],
                                             get.time.res(spg),
                                             half.life,
                                             initial.amplitude);

    not.noise <- peaks[,loudness.column] > peaks$smooth.amplitude + amplitude.threshold;

    ids <- data.frame(event = rep(NA,
                                  nrow(peaks)),
                      landmark = NA);

    ids$event[not.noise] = detect.events(peaks[not.noise,var.columns],
                                         var.weights);

    ids$landmark[not.noise] <- assign.to.landmarks(ids$event[not.noise],
                                                   n.landmarks);

    no.smooth <- -ncol(peaks);

    landmarks <- aggregate(peaks[not.noise, no.smooth],
                           list(event = ids$event[not.noise]),
                           mean);

    names(landmarks)[-1] <- paste("mean",
                                  names(landmarks)[-1],
                                  sep = ".");

    landmarks$duration <- aggregate(peaks$time[not.noise],
                                    list(event=ids$event[not.noise]),
                                    function(x){diff(range(x))})$x;
    for(j in 1:n.landmarks){

        f <- not.noise & ids$landmark == j;

        if(any(f)){
            temp <- aggregate(peaks[f, no.smooth], list(event = ids$event[f]), mean);

            names(temp)[-1] <- paste(names(temp)[-1],j,sep=".");

            landmarks <- cbind(landmarks, temp[match(landmarks$event, temp$event),-1]);
        }
    }

    landmarks <- landmarks[is.finite(apply(landmarks,1,sum)),];

    ix <- grep("time", names(landmarks))[-1];

    landmarks[,ix] <- landmarks[,ix] - landmarks$mean.time;

    peaks$event <- ids$event;
    peaks$landmark <- ids$landmark;

    answer <- list(file = basename(file.name),
                   spg = spg,
                   peaks = peaks,
                   landmarks = landmarks);

    return(invisible(answer));
}
