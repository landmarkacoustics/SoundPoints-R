fast.soundpoints <-
function(file.name,                  # for fft
             passband, amplitude.cutoff, # for peak extraction
             loudness.column, half.life, # for moving average
             var.columns, var.weights,   # for event detection
             n.landmarks                 # for landmark creation
             ){

    spg <- spectrogram(file.name,
                       107/44100,
                       100);

    peaks <- make.peaks(spg,
                        passband,
                        amplitude.cutoff);

    initial.amplitude <- quantile(peaks$mean.amplitude,.85);
    amplitude.threshold <- log(10);
    time.res <- get.time.res(spg);
    rm(spg);
    
    peaks$smooth.amplitude <- smooth.average(peaks[,loudness.column],
                                             time.res,
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

    answer <- list(file = basename(file.name),
                   landmarks = landmarks);

    return(invisible(answer));
}
