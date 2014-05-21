smooth.average <-
function(amplitude, time.res, half.life, initial.value = 1){

    N <- length(amplitude);

    smooth.amplitude <- rep(initial.value, N);

    k <- time.res/half.life;

    for(i in 2:N){
        smooth.amplitude[i] <- (1 - k) * smooth.amplitude[i - 1] + k * amplitude[i]
    };

    return(invisible(smooth.amplitude));
}
