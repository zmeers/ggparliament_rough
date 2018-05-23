scale_colour_party <- function(country = "USA", ...) {
    scale_color_manual(
    if (country=="USA"){

    }
    else if(country == "AUS"){
        values = c("red", "grey", "#009900", "blue"), 
        labels=c("ALP", "Ind.", "Nats.","LP")
    }
    else if(country == "GER"){
        values = c("red", "grey", "#009900", "blue"), 
        labels=c("CDU/CSU", "SPD", "LINKE","GRUENE")
    }
    else if(country == "UK"){

    }
    else (country == "FRA"){

    }
    )
}