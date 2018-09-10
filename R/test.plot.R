test.plot <-
function(sp, ix, ...){
    par(mar=c(10,10,2,2)/3, mgp = 2 * c(3,1,0)/3);
    plot(sp$spg, ...);
    landmark.plot(sp$landmarks, ix);
}
