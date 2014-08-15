spk <-
function(x){

    n <- floor(length(x)/2);

    s <- fft(x);

    10 * log(Re(Conj(s) * s)[n + n:1] / length(x),10);
}
