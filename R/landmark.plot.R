landmark.plot <-
function(landmarks, ix){
    points(t(landmarks$mean.time + landmarks[,11+ix]),
           t(landmarks[,15+ix]),
           pch = 16, cex = 2/3,
           col = rep(2:4, each = length(ix)));
}
