install.packages(tidyverse)
install.packages(MASS)
library(tidyverse)
library(MASS)


###Simulation###
set.seed(853)
mu <-c(10,10)
correlation <-0.9
sigma <-matrix(c(1,correlation, correlation, 1), nrow = 2)
sim_data <- mvrnorm(n = 100, mu, sigma) %>% floor()
mat_page <- sim_data[,1]
rol_page <- sim_data[,2]

set.seed(853)
c = (0:17)
d = (0:20)

mat <- tibble(day = seq(1:100), page = mat_page,
              reader = "Matt")

rol <- tibble(day = seq(1:100), page = rol_page,
              reader = "Rol")

ash <- tibble(day = seq(1:100), page = sample(c, size = 100, replace = TRUE),
              reader = "Ash")

jac <- tibble(day = seq(1:100), page = sample(c, size = 100, replace = TRUE),
              reader = "Jacki")

mik <- tibble(day = seq(1:100), page = sample(d, size = 100, replace = TRUE),
              reader = "Mike")

read_data <-rbind(mat, rol, ash, jac, mik)


### Testing ###
read_data$reader |> unique() == c("Matt", "Jacki", "Ash", "Rol", "Mike") 
read_data$day |> min() > 0
read_data$day |> max() == 100
read_data$page |> class() == "integer"
sum(is.na(read_data)) == 0

### Graphing ###

read_data %>%
  group_by(reader) %>% 
  arrange(day,page) %>%
  mutate(total = cumsum(page)) %>% 
  ggplot(read_data, mapping = aes(x = day, y = total, colour = reader)) +
    geom_line(alpha = 0.5) +
    scale_x_continuous(expand = c(0, 1))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    labs(x = "Day",
         y = "Cumulative number of pages read", fill = "Reader",
         caption = "Figure 1. Cumulative number of pages read by each undergraduate student over 100 days"
    )




