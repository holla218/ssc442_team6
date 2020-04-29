library(tidyverse)
library(forcats)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(MASS)
library(boot)

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

setwd('C:/Users/Jonathon/Desktop/')

data <- read.csv('1916_Present_Data_2.csv', fileEncoding = "UTF-8-BOM", header = TRUE)

set.seed(22)
data_index = sample(nrow(data), size = trunc(0.80 * nrow(data)))
data_trn = data[data_index, ]
data_tst = data[-data_index, ]

##### Testing that everything is working right
fit_test = glm(Won_Election ~ Won_Pop_Vote,
               data = data_trn, family = binomial)
fit_test
fit_tst_pred1 = ifelse(predict(fit_test, data_tst) < 0,
                        "N",
                        "Y")

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

(conf_mat_50 = make_conf_mat(predicted = fit_tst_pred1, actual = data_tst$Won_Election))

names(data_trn)
fit_model = glm(Won_Election ~ 
                  Incumbent_Party
                 +Incumbent_Person
                 +Incumbent_Person*War
                 +Incumbent_Party*GDP.Growth
                 +Incumbent_Party*Rise_In_Inflation
                 +Incumbent_Person*domestic8
                 +Incumbent_Party*Dow_Returns,
                data = data_trn, family = binomial)
summary(fit_model)
model_tst_pred1 = ifelse(predict(fit_model, data_tst) < 0,
                                                  "N",
                                                  "Y")
(conf_mat_50 = make_conf_mat(predicted = model_tst_pred1, actual = data_tst$Won_Election))
                       
cv.glm(data_trn, fit_model, K = 5)$delta[1]

#### Looking at handpicked regressions
names(data_trn)
fit_model = glm(Won_Election ~ 
                 Incumbent_Person
                +Incumbent_Party*Voter.Turnout
                +Incumbent_Party*War
                +Incumbent_Party*Dow_Returns,
                data = data_trn, family = "binomial")
summary(fit_model)
model_tst_pred1 = ifelse(predict(fit_model, data_tst) < 0,
                       "N",
                       "Y")

(conf_mat_50 = make_conf_mat(predicted = model_tst_pred1, actual = data_tst$Won_Election))

cv.glm(data_trn, fit_model, K = 5)$delta[1]


#### Run regression on all columns with Incumbent Person
storage <- list()

for(i in names(data_trn)[c(-2,-3,-4, -5, -6, -7)]){
  reg <- glm(Won_Election ~ Incumbent_Party*get(i), data_trn, family='binomial')
  fit_quality <- cv.glm(data_trn, reg, K = 3)$delta[1]
  storage[[i]] <- fit_quality
}
storage

storage_data <- as.data.frame(storage)

sort.default(storage_data)

#the most influential  varialbles, as identified by the model are pop2, domestic2, voter.turnout, dow_returns, rise_in_inflation

names(data_trn)

fit_model = glm(Won_Election ~ 
              Incumbent_Party*pop2
              +Incumbent_Party*domestic2
              +Incumbent_Party*Voter.Turnout
              +Incumbent_Party*Dow_Returns
              +Incumbent_Party*Rise_In_Inflation,
              data = data_trn, family = "binomial")
summary(fit_model)
model_tst_pred1 = ifelse(predict(fit_model, data_tst) < 0,
                         "N",
                         "Y")

(conf_mat_50 = make_conf_mat(predicted = model_tst_pred1, actual = data_tst$Won_Election))

cv.glm(data_trn, fit_model, K = 5)$delta[1]

Party_Regressions <- list()
for (i in names(data_trn)[c(-2,-3,-4,-5,-6,-7)]){
  reg <- glm(Won_Election ~ Year*Candidate.Party*get(i), data_trn, family='binomial')
  fit_quality <- cv.glm(data_trn, reg, K = 3)$delta[1]
  Party_Regressions[[i]] <- fit_quality
}
Party_Regressions

Party_Regressions_data <- as.data.frame(Party_Regressions)

sort.default(Party_Regressions_data)

# just say the lowest five variables


#think about running lowest five variables here
fit_model = summary(glm(Won_Election ~ 
              Candidate.Party*Voter.Turnout
            +Candidate.Party*American.Indian..Eskimo..Aleut
            +Candidate.Party*domestic2
            +Candidate.Party*Pct_1yr_Change_Producer_Price_Index_Commodities
            +Candidate.Party*GDP.Growth, 
            data=data_trn, family="binomial"))
summary(fit_model)
model_tst_pred1 = ifelse(predict(fit_model, data_tst) < 0,
                         "N",
                         "Y")

(conf_mat_50 = make_conf_mat(predicted = model_tst_pred1, actual = data_tst$Won_Election))

cv.glm(data_trn, fit_model, K = 5)$delta[1]


names(data_trn)

fit_model = glm(Won_Election ~ 
                Candidate.Party*Voter.Turnout
                +Candidate.Party*American.Indian..Eskimo..Aleut
                +Candidate.Party*domestic2
                +Candidate.Party*Pct_1yr_Change_Producer_Price_Index_Commodities
                +Candidate.Party*GDP.Growth,
                data = data_trn, family = "binomial")
summary(fit_model)
model_tst_pred1 = ifelse(predict(fit_model, data_tst) < 0,
                         "N",
                         "Y")

(conf_mat_50 = make_conf_mat(predicted = model_tst_pred1, actual = data_tst$Won_Election))

cv.glm(data_trn, fit_model, K = 5)$delta[1]

