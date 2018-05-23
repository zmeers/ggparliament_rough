## Functions to create parliaments for ggparliament
## Zoe Meers
##
##

#' A ggplot2 geom for parliament plots
#' @param type type of parliament (horseshow, semicircle, circle, classroom, opposing benches)
#' @param totalseats the total number of seats in parliament
#' @param parlrows number of rows in the parliament
#' @param seatspp seats per party
#' @param party_names names of political parties in parliament
#'
#' @example
#' df <- data.frame(Party = c("GUE/NGL", "S&D", "Greens/EFA", "ALDE", "EPP", "ECR", "EFD", "NA"),Number = c(35, 184, 55, 84, 265, 54, 32, 27))
#' df1 <- parliament_data(df)
#' ggplot() + geom_parliament_waffle(type="semicircle, seatspp=df1$Number, parlrows=6, totalseats=sum(df1$Number))
#'
#' @author
#' Zoe Meers
geom_parliament_waffle <- function(totalseats=NULL, parlrows=NULL, seatspp=NULL, party_names=NULL, type=c("horseshoe", "semicircle", "circle", "classroom", "opposing_benches")) {
    if (type == "horseshoe") {
        seats <- function(N, M, r0=5.5) {
            radii <- seq(r0, 7, len = M)
            
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
        
        geom_point(data = result, aes(x, y, colour = as.character(party)), shape=15, size=4)
    }
    else if (type == "circle") {
        circle <- expand.grid(
            y = 1:parlrows,
            x = seq_len(ceiling(sum(seatspp) / parlrows)),
            z = party_names
        )
        
        vec <- rep(party_names, seatspp)
        circle$party <- c(vec, rep(NA, nrow(circle) - length(vec)))
        
        # Plot it
        geom_point(data = circle, aes(x = x, y = y), colour = circle$party, shape=15, size=3)
        # + coord_polar() + scale_y_discrete(expand=c(0.7, 0))
    }
    else if (type == "classroom") {
        classroom <- expand.grid(
            y = 1:parlrows,
            x = seq_len(ceiling(sum(seatspp) / parlrows)),
            z = party_names
        )
        
        vec <- rep(party_names, seatspp)
        classroom$party <- c(vec, rep(NA, nrow(classroom) - length(vec)))
        
        # Plot it
        geom_point(data = classroom, aes(x = x, y = y), colour = classroom$party, shape=15, size=3)
    }
    else if (type == "opposing_benches") {
        westminster <- expand.grid(
            y = 1:parlrows,
            x = seq_len(ceiling(sum(seatspp) / parlrows)),
            z = party_names
        )
        
        vec <- rep(party_names, seatspp)
        westminster$party <- c(vec, rep(NA, nrow(westminster) - length(vec)))
        
        # Plot it
        geom_point(data = westminster, aes(x = x, y = y), colour = westminster$party, shape=15, size=3)
    }
    else {
        seats <- function(N, M, r0=1) {
            radii <- seq(r0, 2, len = M)
            
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
        
        
        geom_point(data = result, aes(x, y, colour = as.character(party)), size=3, shape=15)
    }
}


parliament_data <- function(data= NA, seats=NA, party_names = NA, type=c("semicircle", "horseshoe")) {
    if (type == "horseshoe") {
        data$Share <- seats / sum(seats)
        data$ymax <- cumsum(data$Share)
        data$ymin <- c(0, head(data$ymax, n = -1))
        return(data)
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
    left + plot_spacer() + right
}