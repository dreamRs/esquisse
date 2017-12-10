

#  ------------------------------------------------------------------------
#
# Title : Icons for geoms
#    By : VP
#  Date : vendredi 21 juillet 2017
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( dplyr )
library( ggplot2 )
library( grid )
library( lubridate )





# Fun ---------------------------------------------------------------------

coord_circle <- function(centre = c(0, 0), r = 1, n = 1000) {
  data_frame(
    x = seq(from = 0 - r, to = 0 + r, length.out = n %/% 2),
    y = sqrt(r^2 - x^2)
  ) %>% bind_rows(., -.) %>%
    mutate(x = x + centre[1], y = y + centre[2])
}
coord_circle(centre = c(0, 0), r = 1)




#  ------------------------------------------------------------------------








# Geom bar ----------------------------------------------------------------




png(filename = "inst/geomIcon/www/gg-bar.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#EF562D") + coord_fixed() + theme_void()
p <- ggplot(mpg) + geom_bar(mapping = aes(x = class), fill = "white") + theme_void()
p0 <- ggplot() +
  geom_segment(aes(x=0, xend=2, y=0, yend=0), size = 2,
               arrow = arrow(length = unit(0.5, "cm")), color = "white") +
  geom_segment(aes(x=0, xend=0, y=0, yend=2), size = 2,
               arrow = arrow(length = unit(0.5, "cm")), color = "white") +
  theme_void()
print(p, vp = viewport(width = unit(0.5, "npc"), height = unit(0.5, "npc")))
dev.off()
# print(p0, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
# ggsave(filename = "www/gg-bar.png")





# Geom line ---------------------------------------------------------------


png(filename = "inst/geomIcon/www/gg-line.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#5586A4") + coord_fixed() + theme_void()
economics2 <- economics %>% mutate(quarter = lubridate::quarter(date), year = lubridate::year(date)) %>% group_by(year) %>% summarise(date = min(date), psavert = mean(psavert))
p <- ggplot(data = economics2) + geom_line(mapping = aes(x = date, y = psavert), color = "white", size = 1.4) + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()







# Geom density ------------------------------------------------------------

ggplot(data.frame(x = rnorm(1000))) +
  aes(x) + theme_void() +
  geom_density(fill = "#5587A2", color = "white")


png(filename = "inst/geomIcon/www/gg-density.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#05668D") + coord_fixed() + theme_void()
p <- ggplot(data = data.frame(x = rnorm(10000, sd = 20))) + geom_density(mapping = aes(x = x), color = "white", fill = "white") + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()




# Geom histogram ----------------------------------------------------------


png(filename = "inst/geomIcon/www/gg-histo.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#0C4C8A") + coord_fixed() + theme_void()
# p <- ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), fill = "white") + theme_void()
# p <- ggplot(data = iris) + geom_histogram(mapping = aes(x = Sepal.Length), fill = "white", bins = 20) + theme_void()
p <- ggplot(data = data.frame(x = rnorm(10000, sd = 20))) + geom_histogram(mapping = aes(x = x), fill = "white", bins = 20) + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()

ggplot(data = iris) + geom_histogram(mapping = aes(x = Sepal.Length), bins = 20)






# Geom point --------------------------------------------------------------



png(filename = "inst/geomIcon/www/gg-point.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#02C39A") + coord_fixed() + theme_void()
p <- ggplot(data = filter(iris, Species == "virginica", Sepal.Width < 3.5, Sepal.Width > 2.5)) + geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width), size = 7, color = "white") + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()


ggplot(data = filter(iris, Species == "virginica", Sepal.Width < 3.5, Sepal.Width > 2.5)) + geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width))







# Geom boxplot ------------------------------------------------------------


png(filename = "inst/geomIcon/www/gg-boxplot.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#97D5E0") + coord_fixed() + theme_void()
p <- ggplot(data = iris) + geom_boxplot(mapping = aes(x = Species, y = Sepal.Width), color = "black", fill = "white", size = 2) + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()






# Geom auto ---------------------------------------------------------------



png(filename = "inst/geomIcon/www/gg-auto.png", bg = "transparent")
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#F6D258") + coord_fixed() + theme_void()
p <- ggplot() + geom_text(mapping = aes(x = 0, y = 0, label = "auto"), color = "white", size = 50) + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()





# Geom tile ---------------------------------------------------------------

png(filename = "inst/geomIcon/www/gg-auto.png", bg = "transparent")
df <- expand.grid(x = 0:5, y = 0:5)
df$z <- runif(nrow(df))
ggplot(data = coord_circle(centre = c(0, 0), r = 1)) + geom_polygon(aes(x = x, y = y), fill = "#F6D258") + coord_fixed() + theme_void()
p <- ggplot(data = df) + geom_tile(aes(x, y, fill = z)) + scale_fill_distiller(palette = "Greys", guide = FALSE) + theme_void()
print(p, vp = viewport(width = unit(0.6, "npc"), height = unit(0.6, "npc")))
dev.off()



