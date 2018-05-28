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
geom_parliament_dots <- function(totalseats=NULL, parlrows=NULL, highlightgovernment=FALSE, government= NULL, seatspp=NULL, size = NULL, party_names=NULL, type=c("horseshoe", "semicircle", "circle", "classroom", "opposing_benches")) {
 
  if (type == "horseshoe") {
    seats <- function(N, M) {
      radii <- seq(5.5, 7, len = M)
      
      counts <- numeric(M)
      pts <- do.call(
        rbind,
        lapply(1:M, function(i) {
          counts[i] <<- round(N * radii[i] / sum(radii[i:M]))
          theta <- seq(0, pi, len = counts[i])
          N <<- N - counts[i]
          data.frame(
            x = radii[i] * cos(theta), y = radii[i] * sin(theta), r = i,
            theta = theta
          )
        })
      )
      pts <- pts[order(-pts$theta, -pts$r), ]
      pts
    }
    
    
    election <- function(seats, counts) {
      stopifnot(sum(counts) == nrow(seats))
      seats$party <- rep(1:length(counts), counts)
      seats
    }
    layout <- seats(totalseats, parlrows)
    result <- election(layout, seatspp)
    
    
    geom_point(data = result, aes(x, y, colour = as.character(party)), size=3)
  }
  else if (type == "circle") {
    result <- expand.grid(
      x = 1:parlrows,
      y = seq_len(ceiling(sum(seatspp) / parlrows))
    )
    
    vec <- rep(party_names, seatspp)
    result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
    
    # Plot it
    geom_point(data = result, aes(x, y, colour = as.character(party)), size=3)
  }
  else if (type == "classroom") {
    result <- expand.grid(
      y = 1:parlrows,
      x = seq_len(ceiling(sum(seatspp) / parlrows))
    )
    
    vec <- rep(party_names, seatspp)
    result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
    
    # Plot it
    geom_point(data = result, aes(x, y, colour = as.character(party)), size=3)
    
  }
  else if (type == "opposing_benches") {
    result <- expand.grid(
      x = 1:parlrows,
      y = seq_len(ceiling(sum(seatspp) / parlrows))
    )
    
    vec <- rep(party_names, seatspp)
    result$party <- c(vec, rep(NA, nrow(result) - length(vec)))
    
    # Plot it
    geom_point(data = result, aes(x, y, colour = as.character(party)), size=3)
   
  
  }
  else {
    
    seats <- function(N, M) {
      radii <- seq(1, 2, len = M)
      
      counts <- numeric(M)
      pts <- do.call(
        rbind,
        lapply(1:M, function(i) {
          counts[i] <<- round(N * radii[i] / sum(radii[i:M]))
          theta <- seq(0, pi, len = counts[i])
          N <<- N - counts[i]
          data.frame(
            x = radii[i] * cos(theta), y = radii[i] * sin(theta), r = i,
            theta = theta
          )
        })
      )
      pts <- pts[order(-pts$theta, -pts$r), ]
      pts
    }
    
    
    election <- function(seats, counts) {
      stopifnot(sum(counts) == nrow(seats))
      seats$party <- rep(1:length(counts), counts)
      seats
    }
    layout <- seats(totalseats, parlrows)
    result <- election(layout, seatspp)
    
    
    geom_point(data = result, aes(x, y, colour = as.character(party)), size=3)
    
  }
  
  
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