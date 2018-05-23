## Functions to create opposing bench-style parliaments for ggparliament
## Zoe Meers
## 
## 



ggplot_opposingbenches <- function(dat = NA, rows = NULL, seatspp = NA, partyvar = NA) {
    westminster <- expand.grid(
        y = 1:rows,
        x = seq_len(ceiling(sum(seatspp) / rows)),
        z = partyvar
    )
    
    vec <- rep(partyvar, seatspp)
    westminster$party <- c(vec, rep(NA, nrow(westminster) - length(vec)))
    
    # Plot it
    ggplot(westminster, aes(x = x, y = y)) + geom_point(color = westminster$party) + coord_flip() + theme_void()
}


combine_opposingbenches <- function(left=NA, right=NA) {
    left + plot_spacer() + right
}
