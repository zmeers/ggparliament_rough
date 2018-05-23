## Functions to create opposing bench-style parliaments for ggparliament
## Zoe Meers
## 
## 



ggplot_opposingbenches <- function(data = NA, rows = NULL, seatspp = NA, partyvar = NA) {
    westminster <- expand.grid(
        y = 1:rows,
        x = seq_len(ceiling(sum(seatspp) / rows)),
        z = partyvar
    )
    
    vec <- rep(partyvar, seatspp)
    westminster$party <- c(vec, rep(NA, nrow(westminster) - length(vec)))
    
    # Plot it
    a <- geom_point(data=westminster, aes(x = x, y = y), color = westminster$party)  + coord_polar(theta="x")+scale_y_discrete(expand=c(0.8,0))+ theme_void()
}


combine_opposingbenches <- function(left=NA, right=NA) {
    left + plot_spacer() + right
}
