### Exercise 2
library(tidyverse)
library(kernlab)
library(boot)


make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

bank <- read_csv("https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv")

set.seed(123)
bank$y = factor(bank$y)

smp_size <- floor(0.8 * nrow(bank))
train_ind <- sample(seq_len(nrow(bank)), size = smp_size)

bank_trn <- bank[train_ind, ]
bank_tst <- bank[-train_ind, ]

fit_education = glm(y ~ education,
                    data = bank_trn, family = binomial)
fit_selected = glm(y ~ education + balance + housing + month,
                   data = bank_trn, family = binomial)
fit_additive = glm(y ~ .,
                   data = bank_trn, family = binomial)

fit_over = glm(y ~ education * (.),
               data = bank_trn, family = binomial, maxit = 50)
cv.glm(bank_trn, fit_education, K = 10)$delta[1]
cv.glm(bank_trn, fit_selected, K = 10)$delta[1]
cv.glm(bank_trn, fit_additive, K = 10)$delta[1]
cv.glm(bank_trn, fit_over, K = 10)$delta[1]


bank_tst_pred = ifelse(predict(fit_additive, bank_tst) > 0,"yes","no")
bank_tst_pred = ifelse(predict(fit_additive, bank_tst, type = "response") > 0.5,"yes","no")
(conf_mat_50 = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$y))

table(bank_tst$y) / nrow(bank_tst)

