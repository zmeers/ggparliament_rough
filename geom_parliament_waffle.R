## Functions to create parliaments for ggparliament
## Zoe Meers
##
##

#' A ggplot2 geom for parliament plots
#' @param type type of parliament (horseshow, semicircle, circle, classroom, opposing benches)
#' @param totalseats the total number of seats in parliament
#' @param parlrows number of rows in parliament
#' @param seatspp seats per party
#' @param party_names names of political parties in parliament
#'
#' @example
#' df <- data.frame(Party = c("GUE/NGL", "S&D", "Greens/EFA", "ALDE", "EPP", "ECR", "EFD", "NA"),Number = c(35, 184, 55, 84, 265, 54, 32, 27))
#' df1 <- parliament_data(df)
#' ggplot() + geom_parliament_dots(type="semicircle, seatspp=df1$Number, parlrows=6, totalseats=sum(df1$Number))
#'
#' @author
#' Zoe Meers
geom_parliament_waffle<- function(totalseats=NULL, parlrows=NULL, highlightgovernment=FALSE, government= NULL, seatspp=NULL, size = NULL, party_names=NULL, type=c("opposing_benches")) {
    
        result <- expand.grid(
            x = 1:parlrows,
            y = seq_len(ceiling(sum(seatspp) / parlrows))
        )
        
        vec <- rep(party_names, seatspp)
        result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
        
        # Plot it
        geom_tile(data = result, aes(x, y, fill=as.character(party)),colour="white", size=0.8)
      
}




#' A ggplot2 theme for parliament plots


theme_parliament <- function() {
    theme_void()
}

#' Combine left and right bench for opposing bench-style parliaments
#' @param left left hand side
#' @param right right hand side
#' @author Zoe Meers
combine_opposingbenches <- function(left=NA, right=NA) {
    left + patchwork::plot_spacer() + right
}