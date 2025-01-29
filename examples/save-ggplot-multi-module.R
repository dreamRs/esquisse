library(shiny)
library(ggplot2)
library(esquisse)
library(bslib)

ui <- page_fluid(
  theme = bs_theme_esquisse(),
  save_multi_ggplot_ui("mod")
)

server <- function(...) {

  p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
  p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
  p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
  p4 <- ggplot(mtcars) + geom_bar(aes(carb))
  p5 <- ggplot(presidential) +
    geom_segment(aes(y = name, x = start, xend = end)) +
    geom_point(aes(y = name, x = start)) +
    geom_point(aes(y = name, x = end))

  save_multi_ggplot_server(
    id = "mod",
    plot_list_r = reactive(list(
      list(
        ggobj = p1,
        code = "ggplot(mtcars) + geom_point(aes(mpg, disp))",
        label = "Plot 1"
      ),
      list(
        ggobj = p2,
        code = "ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))",
        label = "Plot 2"
      ),
      list(
        ggobj = p3,
        code = "ggplot(mtcars) + geom_smooth(aes(disp, qsec))",
        label = "Plot 3"
      ),
      list(
        ggobj = p4,
        code = "ggplot(mtcars) + geom_bar(aes(carb))",
        label = "Plot 4"
      ),
      list(
        ggobj = p5,
        code = "ggplot(presidential) +
  geom_segment(aes(y = name, x = start, xend = end)) +
  geom_point(aes(y = name, x = start)) +
  geom_point(aes(y = name, x = end))",
        label = "Plot 5"
      )
    ))
  )
}

if (interactive())
  shinyApp(ui, server)


