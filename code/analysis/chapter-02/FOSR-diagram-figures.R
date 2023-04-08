plots_path <- here::here("figures")
set.seed(1)
t = seq(0, 2 * pi, length.out = 101)
beta_0_t <-  - sin(t)
beta_1_t <- t^2/40

plot(beta_0_t + beta_1_t)
plot(beta_0_t)


K <- outer(t, t, FUN = function(s, t) {0.25 * dnorm(abs(s-t))})
eps_i <- t(mvtnorm::rmvnorm(n = 2, sigma = K))

matplot(eps_i, type = "l")

y_1 <- beta_0_t + beta_1_t + eps_i[, 1]
y_2 <- beta_0_t + eps_i[, 2]

pdf(file = file.path(plots_path, "fosr-y1.pdf"), width = 3, height = 3)
plot(y_1, type = "l", ylim = range(y_1, y_2), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fosr-y2.pdf"), width = 3, height = 3)
plot(y_2, type = "l", ylim = range(y_1, y_2), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "blue4")
dev.off()

pdf(file = file.path(plots_path, "fosr-b0.pdf"), width = 3, height = 3)
plot(beta_0_t, type = "l", xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "black")
dev.off()

pdf(file = file.path(plots_path, "fosr-b1.pdf"), width = 3, height = 3)
plot(beta_1_t, type = "l", xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "red4")
dev.off()


pdf(file = file.path(plots_path, "fosr-e1.pdf"), width = 3, height = 3)
plot(eps_i[, 1], type = "l", ylim = range(eps_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fosr-e2.pdf"), width = 3, height = 3)
plot(eps_i[, 2], type = "l", ylim = range(eps_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "blue4")
dev.off()


plot(y_1, type = "l", lwd = 2)
plot(y_2, type = "l", lwd = 2)
