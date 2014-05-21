plot.spg <-
function(x, pixel.colors = gray.colors(16,1,0), ...){

    image(attr(x, "time"), attr(x, "frequency"), x, col = pixel.colors, xlab = "Time (s)", ylab = "Frequency (Hz)", ...);
}
