### Exercise 2
library(tidyverse)
bank <- read_csv("https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv")

set.seed(123)
bank$y <- str_replace_all(bank$y, c('no'= '0', 'yes'='1'))
bank$Y <- as.numeric(bank$y)

smp_size <- floor(0.8 * nrow(bank))
train_ind <- sample(seq_len(nrow(bank)), size = smp_size)

bank_trn <- bank[train_ind, ]
bank_tst <- bank[-train_ind, ]

fit_education = glm(Y ~ education,
                    data = bank_trn, family = binomial)
fit_selected = glm(Y ~ education + balance + housing + month,
                   data = bank_trn, family = binomial)
fit_additive = glm(Y ~ .,
                   data = bank_trn, family = binomial)

fit_over = glm(Y ~ education * (.),
               data = bank_trn, family = binomial, maxit = 50)
cv.glm(bank_trn, fit_education, K = 10)$delta[1]
cv.glm(bank_trn, fit_selected, K = 10)$delta[1]
cv.glm(bank_trn, fit_additive, K = 10)$delta[1]
cv.glm(bank_trn, fit_over, K = 10)$delta[1]


bank_tst_pred = ifelse(predict(fit_additive, bank_tst) > 0,1,0)
bank_tst_pred = ifelse(predict(fit_additive, bank_tst, type = "response") > 0.5,1,0)

(conf_mat_50 = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$Y))

table(bank_tst$Y) / nrow(bank_tst)
