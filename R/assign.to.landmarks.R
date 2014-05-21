assign.to.landmarks <-
function(ids, n.landmarks){

    landmarks <- rep(NA,length(ids));

    for(e in sort(unique(ids))){

        fe <- ids == e & is.finite(ids);

        n <- sum(fe);

        landmarks[fe] <- ceiling(n.landmarks*(1:n)/n);
    }
    return(invisible(landmarks));
}
