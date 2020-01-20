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