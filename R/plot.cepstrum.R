plot.cepstrum <-
function(x, pixel.colors = gray.colors(16,1,0), ...){

    if(is(x)=="cepstrum"){
        image(attr(x, "time"), attr(x, "quefrency"), x, col = pixel.colors, xlab = "Time (s)", ylab = "Quefrency (Hz)", ...);
    }
}
