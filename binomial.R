library(tidyverse)
theme_set(theme_minimal())

X <- 0:20
prob_coin <- dbinom(X, 20, 0.5)

tibble(x=X,y=prob_coin) |> 
  ggplot(aes(x=x,y=y)) +
  geom_col()

tibble(x=0:15,y=dbinom(0:15, 15, 0.23)) |> 
  ggplot(aes(x=x,y=y)) +
  geom_col()
