library(funData)
set.seed(1996)
simImages <- simFunData(argvals = list(seq(0,1,0.01), seq(-pi/2, pi/2, 0.02)), 
                        M = c(5,4), eFunType = c("Wiener","Fourier"),
                        eValType = "linear", N = 2)


plots_path <- here::here("figures")
pdf(file = file.path(plots_path, "image-01.pdf"), width = 3.5, height = 3)
plot(simImages$simData, obs = 1)
dev.off()
pdf(file = file.path(plots_path, "image-02.pdf"), width = 3.5, height = 3)
plot(simImages$simData, obs = 2)
dev.off()


# -------------------------------------------------------------------------
set.seed(1996)
simCurves1 <- simFunData(seq(0,1,0.01), M = 4, eFunType = "Fourier", eValType = "linear", N = 2)
pdf(file = file.path(plots_path, "curve-01.pdf"), width = 3, height = 3)
plot(simCurves1$simData, obs = 1, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE)
dev.off()

pdf(file = file.path(plots_path, "curve-02.pdf"), width = 3, height = 3)
plot(simCurves1$simData, obs = 2, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE)
dev.off()



# -------------------------------------------------------------------------

set.seed(1996)
simCurves2 <- simFunData(seq(0,1,0.01), M = 6, eFunType = "Wiener", eValType = "linear", N = 2)
pdf(file = file.path(plots_path, "curve-03.pdf"), width = 3, height = 3)
plot(simCurves2$simData, obs = 1, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE, col = "orange")
dev.off()


pdf(file = file.path(plots_path, "curve-04.pdf"), width = 3, height = 3)
plot(simCurves2$simData, obs = 2, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE, col = "orange")
dev.off()



# -------------------------------------------------------------------------

set.seed(1996)
simCurves3 <- simFunData(seq(0,1,0.01), M = 10, eFunType = "Poly", eValType = "linear", N = 2)
pdf(file = file.path(plots_path, "curve-05.pdf"), width = 3, height = 3)
plot(simCurves3$simData, obs = 1, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE, col = "blue")
dev.off()


pdf(file = file.path(plots_path, "curve-06.pdf"), width = 3, height = 3)
plot(simCurves2$simData, obs = 2, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE, col = "blue")
dev.off()


# -------------------------------------------------------------------------

set.seed(1996)
simCurves4 <- simFunData(seq(0,1,0.01), M = 8, eFunType = "Wiener", eValType = "linear", N = 2)
pdf(file = file.path(plots_path, "curve-06.pdf"), width = 3, height = 3)
plot(simCurves4$simData, obs = 1, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()


pdf(file = file.path(plots_path, "curve-07.pdf"), width = 3, height = 3)
plot(simCurves4$simData, obs = 2, xaxt = "n", lwd = 5, yaxt = "n", xlab = NA, axes = FALSE, col = "darkgreen")
dev.off()

