hamming <-
function(N)
{
    0.53836 - 0.46164 * cos(2 * pi * 0:(N - 1) / (N - 1) );
}
