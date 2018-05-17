library(pacman)
p_load(tidyverse, ggforce, scales, pscl, svglite, htmltools)
# setwd

# u-shape parliament
geom_arc_parliament <- function(fill=fill, colour=colour) {
  x <- geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .6, r = 1, start = ymin, end = ymax, fill = fill, explode = .02), colour = colour)
  return(x)
}


# parliament lines
au_parliament_lines <- function() {
  list(geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .7, r = .7, start = ymin, end = ymax), colour = "white", fill = "transparent"), geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .8, r = .8, start = ymin, end = ymax), colour = "white", fill = "transparent"), geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .9, r = .9, start = ymin, end = ymax), colour = "white", fill = "transparent"))
}


nz_parliament_lines <- function() {
 list(geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .75, r = .75, start = ymin, end = ymax), colour = "white", fill = "transparent"), geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .9, r = .9, start = ymin, end = ymax), colour = "white", fill = "transparent"))
}



# Example:
# grab Australian election results from pscl
data(AustralianElections)

# filter to 2016, slice percent, total seats
AustralianElections <- AustralianElections %>%
  filter(date == "2016-07-02") %>%
  gather() %>%
  slice(-c(1:3, 8:19))

# reorder rows into parliament layout (i.e. independents in the middle of the circle btwn the two major parties)
AustralianElections <- AustralianElections[c(1, 4, 3, 2), ]
AustralianElections$key <- factor(AustralianElections$key)
# percent breakdown
AustralianElections$Share <- AustralianElections$value / sum(AustralianElections$value)
AustralianElections$ymax <- cumsum(AustralianElections$Share)
AustralianElections$ymin <- c(0, head(AustralianElections$ymax, n = -1))


# reorder key levels.. again
AustralianElections$key
AustralianElections$key <- factor(AustralianElections$key, levels(AustralianElections$key)[c(1, 4, 3, 2)])
AustralianElections$key
# clean labels
AustralianElections$key <- gsub("Seats", "", AustralianElections$key)




x <- AustralianElections %>%
  mutate_at(vars(starts_with("y")), rescale, to = pi * c(-.5, .5), from = 0:1) %>%
  ggplot() + geom_arc_parliament(fill = AustralianElections$key, colour = "white") + au_parliament_lines() + labs(title = "U-shaped Parliament", subtitle = "2016 AU election", fill = "Party") + scale_fill_manual(values = c("red", "blue", "#009900", "grey")) + theme_void()
x
ggsave(x, file = "au_u-shaped_parli_lines.png")


x <- AustralianElections %>%
  mutate_at(vars(starts_with("y")), rescale, to = pi * c(-.5, .5), from = 0:1) %>%
  ggplot() + geom_arc_parliament(fill = AustralianElections$key, colour = "white") + labs(title = "U-shaped Parliament", subtitle = "2016 AU election", fill = "Party") + scale_fill_manual(values = c("red", "blue", "#009900", "grey")) + theme_void()
x
ggsave(x, file = "au_u-shaped_parli_nolines.png")



# 
# 
# # get the svg as tag
# pso_svg <- htmlSVG(print(x),height=10,width = 14)
# 
# browsable(
#     attachDependencies(
#         tagList(
#             pso_svg,
#             tags$script(
#                 sprintf(
#                     "var data = %s
#                     
#                     var svg = d3.select('svg');
#                     
#                     svg.select('style').remove();
#                     
#                     var bars = svg.selectAll('rect:not(:last-of-type):not(:first-of-type)')
#                     .data(d3.merge(d3.values(d3.nest().key(function(d){return d.Sample}).map(data))))
#                     
#                     bars.style('fill',function(d){
#                     var t = textures
#                     .circle()
#                     .background(d3.rgb(d3.select(this).style('fill')).toString());
#                     svg.call(t);
#                     return t.url();
#                     });
#                     "    
#                     ,
#                     jsonlite::toJSON(AustralianElections)
#                 )
#             )
#         ),
#         list(
#             htmlDependency(
#                 name = "d3",
#                 version = "3.5",
#                 src = c(href = "http://d3js.org"),
#                 script = "d3.v3.min.js"
#             ),
#             htmlDependency(
#                 name = "textures",
#                 version = "1.0.3",
#                 src = c(href = "https://rawgit.com/riccardoscalco/textures/master/"),
#                 script = "textures.min.js"
#             )
#         )
#         )
#         )

