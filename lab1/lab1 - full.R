###EXERCISE 1#####

library(tidyverse)
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
p1b_4 <- p1b + geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_smooth((mapping = aes(x = gdpPercap, y = lifeExp)), method = "lm") +
  scale_x_log10(labels = scales::dollar)
p1b_4

p1b_5 <- ggplot(data = gapminder,
                mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p1b_5 + geom_point() + scale_x_log10()

p1b_6 <- ggplot(data = gapminder,
                mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p1b_6 + geom_point(aes(x = gdpPercap, y = lifeExp, color = 'yellow')) + scale_x_log10()


p1_7 <- ggplot(data = gapminder,
               mapping = aes(x = gdpPercap, y = lifeExp))
p1_7 + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()

p1_8 <- p1_7 + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")

library(scales)
p1_9 <- ggplot(data = gapminder,
               mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p1_9 + geom_point()
p1_9 + geom_point() + scale_x_log10(labels = dollar)
p1_9 + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

p1_10 <- ggplot(data = gapminder,
                mapping = aes(x = gdpPercap, y = lifeExp))
p1_10 + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()

p1_11 <- ggplot(data = gapminder,
                mapping = aes(x = gdpPercap, y = lifeExp))
p1_11 + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")

####EXERCISE 2#####
library(tidyverse)
library(ggplot2)
library(ggthemr)

setwd("C:/Users/tykra/OneDrive/Classes/Spring 2020/SSC 442/ssc442_team6")
ggthemr("fresh")
Bank_Data <- read.csv("bank.csv")

Bank_Data_agg <- count(Bank_Data, job, y)
Bank_Data_agg_2 <- count(Bank_Data, job)
Bank_Data_agg <- Bank_Data_agg[which(Bank_Data_agg$y=="yes"),]
Bank_Data_agg$totals <- Bank_Data_agg_2$n
Bank_Data_agg$percentage <- Bank_Data_agg$n/Bank_Data_agg$totals
Bank_Data_agg$job <- with(Bank_Data_agg, reorder(job, percentage))

Job_vs_Success <- ggplot(Bank_Data_agg, aes(x = job, y = percentage))+
  geom_bar(stat="identity")+
  labs(title="Job vs Success", x="Job", y = "Success Rate")+
  scale_y_continuous(limits=c(0,.25),
                     breaks=c(0,0.05,0.10,0.15,0.2,0.25),
                     labels=c("0%", "5%", "10%", "15%", "20%", "25%"))+
  coord_flip()+
  theme(plot.title = element_text(size=35, hjust=0.5), text=element_text(size=16))

ggsave("Job_vs_Success.png",
       plot = Job_vs_Success,
       device = "png",
       width = 10, height = 7,
       units = c("in"),
       dpi = 600)

Day_vs_Success <- ggplot(Bank_Data, aes(x=day, fill = y))+
  geom_histogram()

Day_vs_Success
ggsave("Day_vs_Success.png",
       plot = Day_vs_Success,
       device = "png",
       width = 10, height = 7,
       units = c("in"),
       dpi = 600)