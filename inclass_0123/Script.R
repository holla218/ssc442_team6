library(tidyverse)

Bank_Data <- read.csv('ssc442_team6/inclass_0123/bank.csv')

null_bank_model = lm(balance ~ age + housing, data = Bank_Data)
full_bank_model = lm(balance ~ ., data = Bank_Data)
anova(null_bank_model, full_bank_model)

summary(full_bank_model)
summary(full_bank_model)$coef

summary(lm(balance ~ age, data=Bank_Data))

bank_model_null <- lm(balance ~ age + job + marital + education + default +
                        housing + loan + contact + day + month + duration +
                        campaign + previous + y, data = Bank_Data)
bank_model_full <- lm(balance ~ age + job + marital + education + default +
                        housing + loan + contact + day + month + duration +
                        campaign + previous + y, data = Bank_Data)
anova(bank_model_null, bank_model_full)
# F-Statistic > 1 for : housing, day, contact, duration, campaign, previous, y

new_bank_model <- lm(balance ~ age + job + marital + education + default +
                       loan + month, data = Bank_Data)
summary(new_bank_model)
summary(bank_model_full)
anova(bank_model_full, new_bank_model)

# By removing those variables listed above from the regression, we arrive at a
# model cotnaining fewer variables that is a better predictor of an individual's
# bank balance. We know this because the F-Statistic of our anova model is 7.206
# when the null hypothesis is the regression containing all variables present in
# the data set and the alternative hypthesis is our final model with those non-
# predicitive variables taken out. This means that our alternative hypothesis is
# better than our null hypothesis. With that being said, there is still a relatively
# small r-squared value and the model is not a great predictor of bank balance.