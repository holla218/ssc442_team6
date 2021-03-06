install.packages("kernlab")
library(kernlab)
data("spam")
tibble::as.tibble(spam)

is.factor(spam$type)
levels(spam$type)

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)

# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

### EXERCISE 1 ###

#1

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]
#The most overfit was fit_caps, followed by fit+selected, followed by fit_ober, followed by fit_additive

#2

set.seed(21)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]
#The most overfit was fit_caps, followed by fit_selected, followed by fit_over, followed by fit_additive
#The conclusion does not change



make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))

table(spam_tst$type) / nrow(spam_tst)

#3

# fit_caps
spam_tst_pred1 = ifelse(predict(fit_caps, spam_tst) > 0,
                        "spam",
                        "nonspam")
spam_tst_pred1 = ifelse(predict(fit_caps, spam_tst, type = "response") > 0.5,
                        "spam",
                        "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred1, actual = spam_tst$type))

# fit_selected
spam_tst_pred2 = ifelse(predict(fit_selected, spam_tst) > 0,
                        "spam",
                        "nonspam")
spam_tst_pred2 = ifelse(predict(fit_selected, spam_tst, type = "response") > 0.5,
                        "spam",
                        "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred2, actual = spam_tst$type))


# fit_additive
spam_tst_pred3 = ifelse(predict(fit_additive, spam_tst) > 0,
                        "spam",
                        "nonspam")
spam_tst_pred3 = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                        "spam",
                        "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred3, actual = spam_tst$type))

table(spam_tst$type) / nrow(spam_tst)
# fit_over
spam_tst_pred4 = ifelse(predict(fit_over, spam_tst) > 0,
                        "spam",
                        "nonspam")
spam_tst_pred4 = ifelse(predict(fit_over, spam_tst, type = "response") > 0.5,
                        "spam",
                        "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred4, actual = spam_tst$type))

#4


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


