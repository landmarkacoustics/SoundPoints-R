hamming <- function(N,k=21,K=46){

    N <- N-1;

    k <- k/K;
    
    (1.0 - k) - k * cos((2.0*pi/N)* 0:N);
}
