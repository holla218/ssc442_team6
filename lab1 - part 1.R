library(tidyverse)
library(gapminder)
library(ggplot2)
data = mpg

p1 <- ggplot(data) 
p1 + geom_point(mapping = aes(x=displ,y=hwy,color="red"))

p2 <- ggplot(data, aes(x=class,y=drv)) 
p2 + geom_point(mapping = aes(x=class,y=drv),size = 2, color = "navy")

p1_2 <- ggplot(data) 
p1_2 + geom_point(mapping = aes(x=displ,y=hwy,color=class))

p1
p2
p1_2

data2 = gapminder
p1b <- ggplot(data2)
p1b + geom_point(mapping = aes(x = gdpPercap, y = lifeExp))
p1b
p1b_2 <- p1b + geom_smooth(mapping = aes(x = gdpPercap, y=lifeExp))
p1b_2
p1b_3 <- p1b + geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) + geom_smooth(mapping = aes(x = gdpPercap, y=lifeExp), method = "lm") + scale_x_log10()
p1b_3


library(scales)
p4b <- p1b + geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_smooth((mapping = aes(x = gdpPercap, y = lifeExp)), method = "lm") +
  scale_x_log10(labels = scales::dollar)
p4b