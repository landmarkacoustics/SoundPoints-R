spk <-
function(x){

    n <- floor(length(x)/2);

    s <- fft(x);

    Re(Conj(s) * s)[1:n];
}
