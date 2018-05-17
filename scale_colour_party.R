scale_colour_party <- function(country=NA){
    scale_colour_manual(
        if (country=="Australia"){
            values=c("red", "grey","#009900","blue"),
            labels=c("ALP, Ind.", "Nats.","LNP")
        }
    )
}
