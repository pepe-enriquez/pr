library("dplyr")
library("tidyr", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")
library("plotly", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")
library("GGally")

# devtools::install_github("ropensci/crosstalk")
library(crosstalk)


sd <- SharedData$new(txhousing, ~year)

p <- ggplot(sd, aes(month, median)) +
  geom_line(aes(group = year)) + 
  geom_smooth(data = txhousing, method = "gam") + 
  facet_wrap(~ city)

ggplotly(p, tooltip = "year") %>%
  highlight(on = "plotly_click", defaultValues = 2015, color = "red")

#########################

d <- SharedData$new(iris)
p <- GGally::ggpairs(d, aes(color = Species), columns = 1:4)
layout(ggplotly(p), dragmode = "select")

#########################

sd <- SharedData$new(txhousing, ~city)
p <- ggplot(sd, aes(date, median)) + geom_line()
gg <- ggplotly(p, tooltip = "city")

highlight(gg, on = "plotly_hover", dynamic = TRUE)
highlight(gg, on = "plotly_hover", dynamic = TRUE, persistent = TRUE)

#########################

library(leaflet)
library(htmltools)

sd <- SharedData$new(quakes)

p <- plot_ly(sd, x = ~depth, y = ~mag) %>% 
  add_markers(alpha = 0.5) %>%
  layout(dragmode = "select") %>%
  highlight(dynamic = TRUE, persistent = TRUE)

map <- leaflet(sd) %>% 
  addTiles() %>% 
  addCircles()

htmltools::tagList(p, map)

############## mi versión ###############

sd <- SharedData$new(quakes)

p <- plot_ly(sd, x = ~depth, y = ~mag) %>% 
  add_markers(alpha = 0.5) %>%
  layout(dragmode = "select") %>%
  highlight(dynamic = TRUE, persistent = TRUE,off="plotly_doubleclick")

map <- leaflet(sd) %>% 
  addTiles() %>% 
  addCircles()

browsable(htmltools::tagList(p, map))


########################################

# Group name is used to populate a title for the dropdown
sd <- SharedData$new(txhousing, ~city, group = "Choose a city")
plot_ly(sd, x = ~date, y = ~median) %>%
  add_lines(text = ~city, hoverinfo = "text") %>%
  highlight(on = "plotly_hover", persistent = TRUE, selectize = TRUE)

########################################

sd <- SharedData$new(txhousing, ~city)

base <- plot_ly(sd, color = I("black")) %>%
  group_by(city)

p1 <- base %>%
  summarise(has = sum(is.na(median))) %>%
  filter(has > 0) %>%
  arrange(has) %>%
  add_bars(x = ~has, y = ~factor(city, levels = city), 
           hoverinfo = "none") %>%
  layout(
    barmode = "overlay",
    xaxis = list(title = "Number of months missing"),
    yaxis = list(title = "")
  ) 

p2 <- base %>%
  add_lines(x = ~date, y = ~median, alpha = 0.3) %>%
  layout(xaxis = list(title = ""))

subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>% 
  layout(margin = list(l = 120)) %>%
  highlight(on = "plotly_click", off = "plotly_doubleclick", color = "red")

########################################

d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black")) %>%
  layout(dragmode = "select")

subplot(
  plot_ly(d, y = ~disp, color = I("black")) %>% 
    add_boxplot(name = "overall"),
  scatterplot, shareY = TRUE
) %>% layout(dragmode = "select")


########################################

p <- subplot(
  plot_ly(d, x = ~factor(vs)) %>% add_histogram(color = I("black")),
  scatterplot
) 

# Selections are actually additional traces, and, by default, 
# plotly.js will try to dodge bars placed under the same category
layout(p, barmode = "overlay")

########################################

# if you don't want to highlight individual points, you could specify
# `class` as the key variable here, instead of the default (rownames)
m <- SharedData$new(mpg)
p <- ggplot(m, aes(displ, hwy, colour = class)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")
ggplotly(p) %>% highlight("plotly_hover")

########################################

# for better tick labels
mtcars$am <- dplyr::recode(mtcars$am, `0` = "automatic", `1` = "manual")
# choose a model by AIC stepping backwards 
mod <- step(lm(mpg ~ ., data = mtcars), trace = FALSE)
# produce diagnostic plots, coloring by automatic/manual
pm <- GGally::ggnostic(mod, mapping = aes(color = am))
# ggplotly() automatically adds rownames as a key if none is provided
ggplotly(pm) %>% highlight("plotly_click")

########################################

m <- SharedData$new(mpg)
p1 <- ggplot(m, aes(displ, fill = class)) + geom_density()
p2 <- ggplot(m, aes(displ, hwy, fill = class)) + geom_point()
subplot(p1, p2) %>% highlight("plotly_click") %>% hide_legend()

########################################

# data frames do support list columns,
# but tibble::tibble() provides a nicer API for this...
d <- data.frame(x = 1:4, y = 1:4)
d$key <- lapply(1:4, function(x) letters[seq_len(x)])
d
#>   x y        key
#> 1 1 1          a
#> 2 2 2       a, b
#> 3 3 3    a, b, c
#> 4 4 4 a, b, c, d

SharedData$new(d, ~key) %>%
  plot_ly(x = ~x, y = ~y)

########################################


