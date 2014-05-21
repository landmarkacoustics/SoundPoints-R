collect.absolute.energy <-
function(spk, desired.sum){

    total <- max(spk);

    N <- length(spk);

    lo <- hi <- match(total, spk)[1];

    while(total < desired.sum){

        action.case <- switch(1 + 2 * (is.na(lo) || lo < 2 || spk[lo - 1] < 0)
                              + (is.na(hi) || is.na(spk[hi+1]) || spk[hi + 1] < 0),
                              ifelse(spk[lo-1] < spk[hi+1], 1, 2),
                              1,
                              2,
                              3);

        if(action.case == 1){
            lo <- lo - 1;
            total <- total + spk[lo];
        }
        else if(action.case == 2){
            hi <- hi + 1;
            total <- total + spk[hi];
        }
        else {
            break;
        }
    }

    if(is.na(lo) || lo < 1)
        lo <- 1;
    if(is.na(hi) || hi > N)
        hi <- N;

    return(c(lo,hi,total));
}
