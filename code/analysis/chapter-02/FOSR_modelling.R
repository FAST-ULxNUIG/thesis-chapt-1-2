# ======================================================================= #
# Fit Function-on-sclara regression model to the GaitRec data set
# based on the data extracted in FLM_Data_Extraction_Script.R
# ======================================================================= #


# 1) Load Packages ----------------------------------------------------------------
# Already Loaded:
#library(readr)
#library(tidyverse)   # CRAN v1.3.0
#library(fda)
# Load other packages, for plotting.
library(RColorBrewer) # CRAN v1.1-2
library(ggtext)       # CRAN v0.1.2       
library(ggpubr)       # CRAN v0.4.0
library(scales)       # CRAN v1.1.1
library(refund)       # CRAN v0.1-23
library(tidyverse)    # CRAN v1.3.0
library(tikzDevice)   # CRAN v0.12.3.1

doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937
plots_path <- here::here("figures")

select <- dplyr::select

# Read in data from the extraction script.
# This should take a long time.
source("code/analysis/chapter-02/FOSR_data_extraction.R") # be prepared to wait.

# 2) Settings for Plotting ------------------------------------------------
# ggplot settings (some extras here)
theme_set(new = theme_bw())
theme_update(axis.text = element_text(size = 11),
             axis.title = element_text(size = 11),
             strip.text = element_text(size = 11),
             panel.grid.minor = element_blank(),
             plot.title = element_text(size = 11, hjust = 0.5),
             plot.subtitle = element_text(size = 11, hjust = 0.5),
             axis.ticks = element_blank(),
             strip.background = element_rect(size = 0.5))



# 3) Exploratory Analysis -------------------------------------------------
# Evaluate the FD object of the data for plotting.
combined_vgrf_df <- data.frame(Sessions_df, t(eval.fd(evalarg = 0:100, fdobj = combined_average_profiles)))
colnames(combined_vgrf_df)[-c(1, 2)] <- 0:100

# Plot all the curves on the same plot:
combined_vgrf_df %>%
  gather(- SESSION_ID, - CLASS_LABEL, key = "time_seq", value = "fun_val") %>%
  mutate(time_seq = as.numeric(time_seq)) %>%
  ggplot() +
  aes(x = time_seq, y = fun_val, group = SESSION_ID, color = CLASS_LABEL) +
  geom_line(alpha = 0.5)

# Plot them in sub-plots next to each other:
# Using facets.
# make a data set to do this
combined_vgrf_lng <- combined_vgrf_df %>%
  gather(- SESSION_ID, - CLASS_LABEL, key = "time_seq", value = "fun_val") %>%
  mutate(time_seq = as.numeric(time_seq)) %>%
  mutate(facet_factor = CLASS_LABEL)
# plot.
combined_vgrf_lng %>%
  ggplot() +
  aes(x = time_seq, y = fun_val, group = SESSION_ID, color = CLASS_LABEL) +
  facet_wrap(~ CLASS_LABEL) +
  geom_line(alpha = 0.5)

# ....nice plot.
# For the figures, it would be good to add
# facet with the  Group Mean Functions.

## 3.1) Exploratory Plot for Figures in Paper -----------------------------------------------------------------------

# This will involve reshaping the data
mean_df <- combined_vgrf_lng %>% 
  group_by(time_seq, CLASS_LABEL) %>%
  select(-facet_factor) %>%
  summarise(fun_val = mean(fun_val))

# test plot:
mean_df %>%
  ggplot(aes(x = time_seq, y = fun_val, group = CLASS_LABEL, color = CLASS_LABEL)) +
  geom_line()
mean_df$facet_factor <- "mean"

# Figure Plot:
# involves wrangling with different factors 
# One for splitting into facetsn(facet_factor) and one for colors (CLASS_LABEL)
factor_plot <- bind_rows(combined_vgrf_lng, mean_df) %>% # combine data and mean function data
  mutate(facet_factor = factor(facet_factor, # re-label facets for strip texts.
                               levels = c("HC", "A", "K", "H", "C", "mean"),
                               labels = c("Healthy Control", "Ankle", "Knee", "Hip", "Calcaneous", "Group Mean Functions")
  )) %>%
  mutate(CLASS_LABEL = factor(CLASS_LABEL, # re-label facets for strip texts.
                              levels = c("HC", "A", "K", "H", "C"),
                              labels = c("Healthy Control", "Ankle", "Knee", "Hip", "Calcaneous"))) %>%
  ggplot() + # plot
  facet_wrap( ~ facet_factor) +
  aes(x = time_seq, y = fun_val, color = CLASS_LABEL) +
  geom_line(aes(group = SESSION_ID), data =  . %>% filter(facet_factor != "Group Mean Functions"), alpha = 0.8) +
  geom_line(data =  . %>% filter(facet_factor == "Group Mean Functions"), size = 0.7) + # have to do separately because of variable names
  theme(legend.position = "none") +
  labs(x = "Normalised Time ($\\%$ of Gait Cycle)",
       y = "vGRF (Normalised to Body Weight)")
# Show plot:
factor_plot

# # looks good, can save.
# ggsave(plot = factor_plot, filename = "Figures/FLM_factors.png",
#        device = "png", units = "cm", width = 20, height = 14)



tikz(file.path(plots_path, "fosr-dataset.tex"),
     width = 1 * doc_width_inches, 
     height = (2.1/3) *  doc_width_inches)
factor_plot
dev.off()




#par(mfrow = c(1, 1))
#plot(mean.fd(combined_average_profiles[which(Sessions_df$CLASS_LABEL == "HC")]), lwd = 1.5)
#lines(mean.fd(combined_average_profiles[which(Sessions_df$CLASS_LABEL == "A")]), col = "red", lwd = 1.5)
#lines(mean.fd(combined_average_profiles[which(Sessions_df$CLASS_LABEL == "K")]), col = "blue", lwd = 1.5)
#lines(mean.fd(combined_average_profiles[which(Sessions_df$CLASS_LABEL == "H")]), col = "green", lwd = 1.5)
#lines(mean.fd(combined_average_profiles[which(Sessions_df$CLASS_LABEL == "C")]), col = "orange", lwd = 1.5)
#legend("bottom", col = c("black", "red", "blue", "green", "orange"), legend =  c("HC", "A", "K", "H", "C"), lwd =3)



# 4) Modelling ------------------------------------------------------------

# Re-order the factor levels in the matrix:
Sessions_df <- Sessions_df %>%
  mutate(CLASS_LABEL = factor(CLASS_LABEL, # re-label facets for strip texts.
                              levels = c("HC", "A", "K", "H", "C"),
                              labels = c("Healthy Control", "Ankle", "Knee", "Hip", "Calcaneus")))
# Get ready for Functional Linear Model:
# Set up the design matrix:
# Column of 1's and an indicator variable for each level of the factor class_label
modmat = cbind(intercept = 1, model.matrix(~ factor(Sessions_df$CLASS_LABEL) - 1))
modmat
stopifnot(nrow(modmat) == nrow(Sessions_df))
dim(modmat)
colnames(modmat)
# Set up a constraint.
# the 1's mark the variables we want to sum to zero for indetifiability
(constraints = matrix(c(0,1, 1, 1, 1, 1), 1))
# the factor effects will sum to 0, not the intercept.

# we are going to use the refund software

# Use Function on Scalar Regression:
# ?fosr - for help.
# Arguments
# fdobj = the functional responses, a functional data object (class "fd") as in the fda package
response_obj <- combined_average_profiles
# X = the model matrix, whose columns represent scalar predictors.
# ... Should ordinarily include a column of 1s.
head(modmat)
# con = a row vector or matrix of linear contrasts of the coefficient functions,
# ... to be constrained to equal zero.

# argvals = the d argument values at which the coefficient functions will be evaluated.
arg_vals <- 0:100
# method = estimation method:
# - "OLS" for penalized ordinary least squares,
# - "GLS" for penalized generalized least squares,
# - "mix" for mixed effect models.

# gam.method = 	smoothing parameter selection method, to be passed to gam:
#- "REML" for restricted maximum likelihood,
#- "GCV.Cp" for generalized cross-validation.

# cov.method = covariance estimation method:
# the current options are naive or modified Cholesky. See Details.

# lambda = smoothing parameter value. If NULL, the smoothing parameter(s) will be estimated.
#... See Details.

# nbasis, norder = number of basis functions,
#... and order of splines (the default, 4, gives cubic splines),
#...for the B-spline basis used to represent the coefficient functions.
#... when the functional responses are supplied using fdobj,
# these arguments are ignored in favor of the values pertaining to the supplied object.

# pen.order = order of derivative penalty.

# multi.sp a logical value indicating whether separate smoothing parameters
# should be estimated for each coefficient function. Currently must be FALSE if method = "OLS"


# pve	= if method = 'mix', the percentage of variance explained 
# ...by the principal components; defaults to 0.99

# max.iter = maximum number of iterations if method = "GLS".

# maxlam = maximum smoothing parameter value to consider (when lamvec=NULL; see lofocv).


# a logical value indicating whether a cross-validation score
#... should be computed even if a single fixed lambda is specified (when method = "OLS").

# scale =	logical value or vector determining scaling of the matrix X 


# 1) Ordinary least squares.
# perform on subset of data
# randomy sample 100 obs
set.seed(1996) # set seed to randomly sampe from obs:
olsmod_sample = fosr(fdobj = response_obj[sample(1:nrow(Sessions_df), size = 100, replace = F)],
                     X = modmat[sample(1:nrow(Sessions_df), size = 100, replace = F), ],
                     con = constraints, # sum-to-zero's
                     method = "OLS", # ordinary least squares.
                     argvals = arg_vals)

# works perfectly. for larger data, it will throw warnings.
# issue explained here
# https://github.com/refunders/refund/issues/86
# but seems to work ok, so not to worry. 
# I think we'll still use GLS. # This should give correct confidence intervals!
# run anyway:
olsmod = fosr(fdobj = response_obj,
              X = modmat,
              con = constraints,
              method="OLS",
              argvals = arg_vals)


glsmod <- fosr(fdobj = response_obj,
               X = modmat,
               con = constraints,
               method = "GLS",
               argvals = arg_vals)

# we will work with GLS model.
plot(glsmod, split = 1)
par(mfrow = c(1, 1))
plot(glsmod$resid)


# 5) Model Output/ Estimates ----------------------------------------------
# prepare nice plot of outputs

# get point estimates:
parameter_ests <- glsmod$est.func 
colnames(parameter_ests) <- c("Overall Mean", "Healthy Control", "Ankle", "Knee", "Hip", "Calcaneus")
params_long <- data.frame(time_seq = 0:100, parameter_ests) %>%
  gather(-time_seq, key = "condition", value = "point_est")

# get SE's for pointwise CIs
parameter_se <- glsmod$se.func
colnames(parameter_se) <- c("Overall Mean", "Healthy Control", "Ankle", "Knee", "Hip", "Calcaneus")

# cosntruct pointwise CIs in data frames
# upper curve:
upper_long <- data.frame(time_seq = 0:100, parameter_ests + 2*parameter_se) %>%
  gather(-time_seq, key = "condition", value = "upper")
# lower curve:
lower_long <- data.frame(time_seq = 0:100, parameter_ests - 2*parameter_se) %>%
  gather(-time_seq, key = "condition", value = "lower")

# Plot:
# have to do a little hacky thing by adding 'fake point'
# to replicate the refund plot.fosr outcome where the scales
# are fixed for all BUT the mean function.
# with ggplot facet, you can either fix all or none 
# or do this little trick
coefficients_plot <- inner_join(params_long, lower_long, by = c("time_seq", "condition")) %>%
  inner_join(upper_long, by = c("time_seq", "condition")) %>%
  mutate(condition = factor(condition,
                            levels =  c("Overall.Mean", "Healthy.Control", "Ankle", "Knee", "Hip", "Calcaneus"),
                            labels = c("$\\beta_0 (t)$ Overall Mean",
                                       "$\\beta_1 (t)$ Healthy Control",
                                       "$\\beta_2 (t)$ Ankle",
                                       "$\\beta_3 (t)$ Knee",
                                       "$\\beta_4 (t)$ Hip",
                                       "$\\beta_5 (t)$ Calcaneus"))) %>%
  ggplot() +
  aes(x = time_seq, color = condition) +
  facet_wrap(~ condition, scales = "free") +
  geom_hline(yintercept = 0, col = "grey") +
  geom_line(aes(y = point_est)) +
  geom_line(aes(y = upper), lty = "dashed", lwd = 0.7)+
  geom_line(aes(y = lower), lty = "dashed", lwd = 0.7) +
  geom_point(data = . %>% filter(as.numeric(condition) != 1), # TRICK :-)
             inherit.aes = F, aes(x = 50, y = -0.15), color = NA) +
  geom_point(data = . %>% filter(as.numeric(condition) != 1),
             inherit.aes = F, aes(x = 50, y = 0.15), color = NA) +
  scale_color_manual(values = c("black", hue_pal()(5))) +
  theme(legend.position = "none") +
  labs(x ="Normalised Time ($\\%$ of Gait Cycle)", y = "$\\widehat{\\beta} (t)$")
#Look at plot
coefficients_plot


tikz(file.path(plots_path, "fosr-coefs.tex"),
     width = 1 * doc_width_inches, 
     height = (2.1/3) *  doc_width_inches)
coefficients_plot
dev.off()

# ggsave(plot = coefficients_plot, filename = "Figures/FLM_coefficients.png",
#        device = "png", units = "cm", width = 20, height = 14)

# ignore warnings.




# 6) model Assesement ----------------------------------------------------
# Do a permutation test
#
#combined_warping_df <- data.frame(Sessions_df, t(eval.fd(evalarg = 0:100, fdobj = combined_average_profiles_reg$warpfd)))
#colnames(combined_warping_df)[-c(1, 2)] <- 0:100
# Don't RUN, can load after, skip line.
permutation_test <- fosr.perm.fit(fdobj = response_obj,
                                  X = modmat,
                                  con = constraints,
                                  method = "GLS",
                                  argvals = arg_vals,
                                  nperm = 400, 
                                  prelim = 20)
saveRDS(permutation_test, file = "Outputs/Permutation_Results.RDS") # Save.
# SKIP TO HERE:
permutation_test <- readRDS(file = "Outputs/Permutation_Results.RDS")
# permutation_test
par(mfrow = c(1, 1))
plot(permutation_test)
# Formal test at 5% level
alpha_0.05_test <- fosr.perm.test(x = permutation_test, level = 0.05)
# Highly significant.

colnames(permutation_test$F.perm ) <- 0:100

perm_plot <- permutation_test$F.perm %>%
  as.data.frame %>%
  rownames_to_column(var = "perm_num") %>%
  gather(-perm_num, key = "time", value = "F_null") %>%
  mutate(time = as.numeric(time)) %>%
  ggplot() +
  aes(x = time, y = F_null, group = perm_num) +
  geom_line(col = "grey") +
  geom_hline(yintercept = alpha_0.05_test$critval, col = "red", lty = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(plot.margin = margin(r = 10, l = 10, t = 10)) +
  labs(title = "F Test Permutation Results",
       y = "F Statistic",
       x = "Normalised Time (%) of Gait Cycle") +
  geom_line(data = data.frame(time = 0:100, F_true = permutation_test$F), inherit.aes = F,
            aes(x = time, y = F_true), col = "cornflowerblue") +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 8),
        axis.title  = element_text(size = 8))

# View
perm_plot

ggsave(plot = perm_plot, filename = "Figures/FLM_permutation.png",
       device = "png", units = "cm", width = 8, height = 5)


