library(tidyverse)
library(ggplot2)
library(ggthemr)

setwd("C:/Users/tykra/OneDrive/Classes/Spring 2020/SSC 442/ssc442_team6")
ggthemr("lilac")
Bank_Data <- read.csv("bank.csv")

Bank_Data$log_balance <- log(Bank_Data$balance)

Education_vs_LogBalance <- ggplot(Bank_Data, aes(x=education, y = log_balance))+
  geom_boxplot()+
  labs(title="Education vs Natural Log of Balance", x="Education", y= "Log Balance")+
  theme(plot.title = element_text(size=35, hjust = 0.5), text=element_text(size=20))
  
ggsave("Education_vs_Log_Balance.png",
       plot = Education_vs_LogBalance,
       device = "png",
       width = 10, height = 7,               
       units = c("in"),
       dpi = 600)

Bank_Data_agg <- aggregate(Bank_Data$balance, list(Bank_Data$job), mean)
Bank_Data_agg$Group.1 <- factor(Bank_Data_agg$Group.1, levels = Bank_Data_agg$Group.1[order(Bank_Data_agg$x)])
Job_vs_Balance <- ggplot(Bank_Data_agg, aes(x = Group.1, y = x))+
  geom_bar(stat="identity")+
  labs(title="Job vs Balance", x="Job", y = "Balance")+
  coord_flip()+
  theme(plot.title = element_text(size=35, hjust=0.5), text=element_text(size=16))
  
ggsave("Job_vs_Balance.png",
         plot = Job_vs_Balance,
         device = "png",
         width = 10, height = 7,               
         units = c("in"),
         dpi = 600)
  
  
