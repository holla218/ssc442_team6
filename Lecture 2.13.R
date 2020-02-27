install.packages("caret")
install.packages("rpart")
library(caret)
library(rpart)
install.packages("ISLR")
library(ISLR)
install.packages("tibble")
library(tibble)

plot_tree_split = function(cut = 0, main = "main") {
  plot(sim_slr_data, pch = 20, col = "grey", cex = 2, main = main)
  grid()
  abline(v = cut, col = "black", lwd = 2)
  left_pred = mean(sim_slr_data$y[sim_slr_data$x < cut])
  right_pred = mean(sim_slr_data$y[sim_slr_data$x > cut])
  segments(x0 = -2, y0 = left_pred, x1 = cut, y1 = left_pred, col = "limegreen", lwd = 2)
  segments(x0 = cut, y0 = right_pred, x1 = 2, y1 = right_pred, col = "firebrick", lwd = 2)
}
par(mfrow = c(1, 3))
plot_tree_split(cut = -0.5, main = "cut @ x = -0.5")
plot_tree_split(cut = 0, main = "cut @ x = 0.0")
plot_tree_split(cut = 0.75, main = "cut @ x = 0.75")

calc_split_mse = function(cut = 0) {
  l_pred = mean(sim_slr_data$y[sim_slr_data$x < cut])
  r_pred = mean(sim_slr_data$y[sim_slr_data$x > cut])
  
  l_mse = round(sum((sim_slr_data$y[sim_slr_data$x < cut] - l_pred) ^ 2), 2)
  r_mse = round(sum((sim_slr_data$y[sim_slr_data$x > cut] - r_pred) ^ 2), 2)
  t_mse = round(l_mse + r_mse, 2)
  c("Total MSE" = t_mse, "Left MSE" = l_mse, "Right MSE" = r_mse)
}
cbind("Cutoff" = c(-0.5, 0.0, 0.75),
  rbind(calc_split_mse(cut = -0.5),
    calc_split_mse(cut = 0),
    calc_split_mse(cut = 0.75))) %>%
  kable() %>%
  kable_styling(full_width = FALSE)

tree_slr = rpart(y ~ x, data = sim_slr_data)
plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr, tibble(x = x)),
      col = "darkorange", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()
rpart.plot::rpart.plot(tree_slr)



tree_slr = rpart(y ~ x, data = sim_slr_data, cp = 0, minsplit = 5)
plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr, tibble(x = x)),
      col = "darkorange", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()
rpart.plot::rpart.plot(tree_slr)


tree_slr_cp_10 = rpart(y ~ x, data = sim_slr_data, cp = 0.10, minsplit = 2)
tree_slr_cp_05 = rpart(y ~ x, data = sim_slr_data, cp = 0.05, minsplit = 2)
tree_slr_cp_00 = rpart(y ~ x, data = sim_slr_data, cp = 0.00, minsplit = 2)


par(mfrow = c(1, 3))

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.10, minsplit = 2")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_cp_10, tibble(x = x)),
      col = "firebrick", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.05, minsplit = 2")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_cp_05, tibble(x = x)),
      col = "dodgerblue", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.00, minsplit = 2")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_cp_00, tibble(x = x)),
      col = "limegreen", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

tree_slr_ms_25 = rpart(y ~ x, data = sim_slr_data, cp = 0.01, minsplit = 25)
tree_slr_ms_10 = rpart(y ~ x, data = sim_slr_data, cp = 0.01, minsplit = 10)
tree_slr_ms_02 = rpart(y ~ x, data = sim_slr_data, cp = 0.01, minsplit = 02)
par(mfrow = c(1, 3))

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.01, minsplit = 25")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_ms_25, tibble(x = x)),
      col = "firebrick", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.01, minsplit = 10")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_ms_10, tibble(x = x)),
      col = "dodgerblue", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.01, minsplit = 2")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_ms_02, tibble(x = x)),
      col = "limegreen", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

tree_slr_ms_25 = rpart(y ~ x, data = sim_slr_data, cp = 0.01, minsplit = 25)
tree_slr_ms_10 = rpart(y ~ x, data = sim_slr_data, cp = 0.01, minsplit = 10)
tree_slr_ms_02 = rpart(y ~ x, data = sim_slr_data, cp = 0.01, minsplit = 02)


par(mfrow = c(1, 3))

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.01, minsplit = 25")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_ms_25, tibble(x = x)),
      col = "firebrick", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.01, minsplit = 10")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_ms_10, tibble(x = x)),
      col = "dodgerblue", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "cp = 0.01, minsplit = 2")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(tree_slr_ms_02, tibble(x = x)),
      col = "limegreen", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()


# load data, coerce to tibble
crdt = as_tibble(ISLR::Credit)

library(dplyr)
# data prep
crdt = crdt %>%
  select(-ID) %>%
  select(-Rating, everything())

# test-train split
set.seed(1)
crdt_trn_idx = sample(nrow(crdt), size = 0.8 * nrow(crdt))
crdt_trn = crdt[crdt_trn_idx, ]
crdt_tst = crdt[-crdt_trn_idx, ]

# estimation-validation split
crdt_est_idx = sample(nrow(crdt_trn), size = 0.8 * nrow(crdt_trn))
crdt_est = crdt_trn[crdt_est_idx, ]
crdt_val = crdt_trn[-crdt_est_idx, ]
# check data
head(crdt_trn, n = 10)

crdt_knn_01 = knnreg(Rating ~ Age + Gender + Student, data = crdt_est, k = 1)
crdt_knn_10 = knnreg(Rating ~ Age + Gender + Student, data = crdt_est, k = 10)
crdt_knn_25 = knnreg(Rating ~ Age + Gender + Student, data = crdt_est, k = 25)
head(crdt_knn_10$learn$X)
dist(head(crdt_knn_10$learn$X))
sqrt(sum((crdt_knn_10$learn$X[4, ] - crdt_knn_10$learn$X[5, ]) ^ 2))

predict(crdt_knn_10, crdt_val[1:5, ])


library(purrr)
knn_mod_list = list(
  crdt_knn_01 = knnreg(Rating ~ Age + Gender + Student, data = crdt_est, k = 1),
  crdt_knn_10 = knnreg(Rating ~ Age + Gender + Student, data = crdt_est, k = 10),
  crdt_knn_25 = knnreg(Rating ~ Age + Gender + Student, data = crdt_est, k = 25)
)
knn_val_pred = map(knn_mod_list, predict, crdt_val)
calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
map_dbl(knn_val_pred, calc_rmse, crdt_val$Rating)

crdt_tree = rpart(Rating ~ Age + Gender + Student, data = crdt_est)
crdt_tree
library(rpart.plot)
rpart.plot(crdt_tree)
crdt_tree_big = rpart(Rating ~ Age + Gender + Student, data = crdt_est,
                      cp = 0.0, minsplit = 20)
rpart.plot(crdt_tree_big)
tree_mod_list = list(
  crdt_tree_0000 = rpart(Rating ~ Age + Gender + Student, data = crdt_est, cp = 0.000),
  crdt_tree_0001 = rpart(Rating ~ Age + Gender + Student, data = crdt_est, cp = 0.001),
  crdt_tree_0010 = rpart(Rating ~ Age + Gender + Student, data = crdt_est, cp = 0.010),
  crdt_tree_0100 = rpart(Rating ~ Age + Gender + Student, data = crdt_est, cp = 0.100)
)
tree_val_pred = map(tree_mod_list, predict, crdt_val)
map_dbl(tree_val_pred, calc_rmse, crdt_val$Rating)
crdt_tree_all = rpart(Rating ~ ., data = crdt_est)
rpart.plot(crdt_tree_all)
calc_rmse(
  actual = crdt_val$Rating,
  predicted = predict(crdt_tree_all, crdt_val)
)
predict(crdt_tree_all, crdt_val[1:5, ])
