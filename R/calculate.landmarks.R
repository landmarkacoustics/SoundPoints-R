calculate.landmarks <-
function(peaks, evs.and.lmrks){

    fa <- is.finite(evs.and.lmrks$event);

    landmarks <- aggregate(peaks$time[fa],
                           evs.and.lmrks[fa,],
                           min);

    names(landmarks)[3] <- "start.time";

    landmarks <- cbind(landmarks,
                       aggregate(peaks[fa,1:8],
                                 evs.and.lmrks[fa,],
                                 mean)[,-(1:2)]);

    landmarks$time <- landmarks$time - landmarks$start.time;

    landmarks <- landmarks[order(landmarks$event,
                                 landmarks$landmark),];

    return(invisible(landmarks));
}
