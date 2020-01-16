library(tidyverse)
data = mpg

p1 <- ggplot(data) 
p1 + geom_point(mapping = aes(x=displ,y=hwy,color="red"))

p2 <- ggplot(data, aes(x=class,y=drv)) 
p2 + geom_point(mapping = aes(x=class,y=drv),size = 2, color = "navy")

p1_2 <- ggplot(data) 
p1_2 + geom_point(mapping = aes(x=displ,y=hwy,color=class))
