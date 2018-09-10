spk <-
function(x){

    n <- length(x);

    k <- floor(0.5 * n);

    y <- Mod(fft(x)[1:k+1])
    
    10 * ( 2 * log( y, 10) - log(n,10) );
}
