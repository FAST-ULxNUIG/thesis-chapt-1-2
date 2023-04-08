plots_path <- here::here("figures")
set.seed(1)
t = seq(0, 2 * pi, length.out = 101)
beta_0_t <-  - sin(t)
beta_1_t <- t^2/40

plot(beta_0_t + beta_1_t)
plot(beta_0_t)

K <- outer(t, t, FUN = function(s, t) {0.25 * dnorm(abs(s-t))})
eps_i <- t(mvtnorm::rmvnorm(n = 4, sigma = K))
u_i <- t(mvtnorm::rmvnorm(n = 2, sigma = K))

matplot(eps_i, type = "l")
matplot(u_i, type = "l")

y_11 <- beta_0_t + beta_1_t + u_i[, 1] +  eps_i[, 1]
y_12 <- beta_0_t + u_i[, 1] +  eps_i[, 2]
y_21 <- beta_0_t + beta_1_t +  u_i[, 2] +  eps_i[, 3]
y_22 <- beta_0_t +  u_i[, 2] +  eps_i[, 4]

plot(y_)

ylims_y <- range(y_11, y_12, y_21, y_22)

pdf(file = file.path(plots_path, "fmm-y11.pdf"), width = 3, height = 3)
plot(y_11, type = "l", ylim = ylims_y, xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fmm-y12.pdf"), width = 3, height = 3)
plot(y_12, type = "l", ylim = ylims_y, xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fmm-y21.pdf"), width = 3, height = 3)
plot(y_21, type = "l", ylim = ylims_y, xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "blue4")
dev.off()

pdf(file = file.path(plots_path, "fmm-y22.pdf"), width = 3, height = 3)
plot(y_22, type = "l", ylim = ylims_y, xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "blue4")
dev.off()

pdf(file = file.path(plots_path, "fmm-b0.pdf"), width = 3, height = 3)
plot(beta_0_t, type = "l", xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "black")
dev.off()

pdf(file = file.path(plots_path, "fmm-b1.pdf"), width = 3, height = 3)
plot(beta_1_t, type = "l", xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "red4")
dev.off()



pdf(file = file.path(plots_path, "fmm-e11.pdf"), width = 3, height = 3)
plot(eps_i[, 1], type = "l", ylim = range(eps_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fmm-e12.pdf"), width = 3, height = 3)
plot(eps_i[, 2], type = "l", ylim = range(eps_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fmm-e21.pdf"), width = 3, height = 3)
plot(eps_i[, 3], type = "l", ylim = range(eps_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "blue4")
dev.off()

pdf(file = file.path(plots_path, "fmm-e22.pdf"), width = 3, height = 3)
plot(eps_i[, 4], type = "l", ylim = range(eps_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "blue4")
dev.off()


#####
pdf(file = file.path(plots_path, "fmm-u1.pdf"), width = 3, height = 3)
plot(u_i[, 1], type = "l", ylim = range(u_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

pdf(file = file.path(plots_path, "fmm-u2.pdf"), width = 3, height = 3)
plot(u_i[, 2], type = "l", ylim = range(u_i), xaxt = "n", lwd = 5, yaxt = "n",ylab = NA, xlab = NA, axes = FALSE,  col = "blue4")
dev.off()





