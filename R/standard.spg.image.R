standard.spg.image <-
function(spg, path = "default.png", amplitude.threshold = 0, time.offset = 0, ...){

    if(is(spg) == "spg"){
        png(path, 5 * 180, 3 * 180, pointsize = 10, res = 180);

        par(mar = c(10,10,2,2)/3, mgp = 2 * c(3,1,0) / 3);

        temp <- -spg;

        temp[temp > amplitude.threshold] <- amplitude.threshold;

        image(attr(spg, "time"), attr(spg,"frequency") / 1000, temp, col = gray.colors(512,0,1), xlab = "Time (s)", xlim = c(0,3) + time.offset, ylab = "Frequency (kHz)", ylim = c(0,10), ...);

        dev.off();
    }

}
