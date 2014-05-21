make.tmp <-
function(sp){
    tmp <- data.frame(file.name=sp$file, sp$landmarks);
    tmp$file.name <- as.character(tmp$file.name);
    invisible(tmp);
}
