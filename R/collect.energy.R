collect.energy <-
function(spk, desired.sum){

    ends <- rep(match(max(spk), spk),2);

    N <- length(spk);

    total <- 0;

    current.step <- 1;

    while(total < desired.sum){

        temp.next <- c(ifelse(ends[1] > 1, spk[ends[1] - 1], -Inf),
                       ifelse(ends[2] < N, spk[ends[2] + 1], -Inf));

        now <- 1 + (temp.next[1] < temp.next[2]);

        if(is.infinite(temp.next[now]))
            break;

        height <- spk[ends[now]] - temp.next[now];

        width <- ifelse(height > 0, current.step, 1);

        ends[now] <- ends[now] + ifelse(now == 1,-1,1);

        total <- total + width * height;

        current.step <- current.step + 1;
    }

    return(c(ends, total));
}
