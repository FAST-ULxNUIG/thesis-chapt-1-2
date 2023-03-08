library(readr)        # CRAN v1.3.1
library(tidyverse)    # CRAN v1.3.0
library(fda)          # CRAN v5.1.9
library(RColorBrewer) # CRAN v1.1-2
library(ggtext)       # [github::wilkelab/ggtext] v0.1.0
library(ggpubr)       # CRAN v0.4.0
library(Polychrome)   # CRAN v1.2.6
library(tikzDevice)   # CRAN v0.12.3.1


data_path <- here::here("data")
functions_path <- here::here("code", "functions")
plots_path <- here::here("figures")

options(tikzLatexPackages = c(getOption( "tikzLatexPackages" ),"\\usepackage{amsfonts}"))
options(tikzLatexPackages = c(getOption( "tikzLatexPackages" ),"\\usepackage{amsmath}"))
options(tikzLatexPackages = c(getOption( "tikzLatexPackages" ),"\\usepackage{amssymb}"))


# -------------------------------------------------------------------------
average_profiles <- readRDS(file = file.path(data_path, "gaitrec-healthy.rds"))
selected_sessions <- readRDS(file = file.path(data_path, "gaitrec-selected-sessions.rds"))

# -------------------------------------------------------------------------
source(file.path(functions_path, "theme_gunning.R"))
theme_gunning()
# theme_update(axis.text = element_text(size = 10),
#              axis.title = element_text(size = 12),
#              strip.text = element_text(size = 12))
# -------------------------------------------------------------------------

# 7) Plots of the mean and Covariance Structure ---------------------------


# 7.1 Covariance ----------------------------------------------------------

co_var_unreg <- eval.bifd(var.fd(fdobj1 = average_profiles), sevalarg = 0:100, tevalarg = 0:100)

# plot covariance surfaces
filled.contour(x = 0:100, y = 0:100, z = co_var_unreg)

co_var <- co_var_unreg
# we can see registration has 'focused' the variance 
# we'll use the registered data for our plots

co_var <- as.data.frame(co_var)
colnames(co_var) <- 0:100
co_var$s <- 0:100


# set up function to inerpolate a color palette:
getPalette = colorRampPalette(brewer.pal(n = 9, name = "YlOrRd"))

# Plot of Covariance Function:
# function to label breaks for the contour plot.
breaks_labeller <- function(string_break){
  cleaned_string <- str_remove(string = string_break, pattern =  "]") %>%
    str_remove(pattern = "\\(") %>%
    str_remove(pattern = fixed(" "))
  
  inf_string <- str_extract(string = cleaned_string,
                            pattern = "[^,]*") %>%
    as.numeric %>%
    round(digits = 2)
  
  sup_string <- str_extract(string = cleaned_string,
                            pattern =  "[^,]*$") %>%
    as.numeric %>%
    round(digits = 2)
  
  rounded_lab = paste0("(", inf_string, ", ", sup_string, "]")
  return(rounded_lab)
}
# test
breaks_labeller(c("(-0.4263, -0.3654]", "(-0.4255, -0.4554]"))
# plot contour
co_var_plot <- co_var %>%
  gather(-s, key = "t", value = "var") %>%
  mutate(t = as.numeric(t)) %>%
  ggplot() +
  geom_contour_filled(mapping = aes(x = s, y = t, z = 100*var), bins = 15, alpha = 0.9) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = getPalette(n = 15), labels = breaks_labeller) +
  theme(axis.ticks = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 10)) +
  labs(title = "Covariance Function $\\widehat{C}(t_1, t_2)$",
       fill = "$C(t_1, t_2)$")




# 7.2 Mean Function -------------------------------------------------------

# Evaluated the registered data:
# We will plot in the background
averaged_eval <- eval.fd(evalarg = 0:100, fdobj = average_profiles)
# shape it as a data frame to plot:
rownames(averaged_eval) <- 0:100
colnames(averaged_eval) <- selected_sessions
# we are going to plot the meamn, store this in df:
averaged_eval_mean <- apply(X = averaged_eval, MARGIN = 1, mean)
av_mean_df <- data.frame("t_val" = 0:100, "mean_val" = averaged_eval_mean)

# make plot:
mean_plot <- averaged_eval %>%
  as.data.frame %>%
  rownames_to_column(var = "t_val") %>%
  mutate(t_val = as.numeric(as.character(t_val))) %>%
  gather(-t_val, key = "subject", value = "grf_val") %>%
  ggplot() +
  scale_y_continuous(expand = c(0, 0)) +
  aes(x = t_val, y = grf_val) +
  geom_line(mapping = aes(group = subject), color = "grey", alpha = 0.5) +
  geom_line(data = av_mean_df, aes(y = mean_val), lwd = 1) +
  labs(y = "$\\widehat{\\mu}(t)$ (Mean vGRF, Normalised to BW)",
       x = "Normalised Time ($\\%$ of Gait Cycle)",
       title = "Sample Mean Function $\\widehat{\\mu} (t)$") + 
  theme(legend.position = "none")




# 7.3 Arrange and Save Plots ----------------------------------------------

# Arrange plots side by side:
mean_covar_plot <- ggarrange(mean_plot, co_var_plot, ncol = 2, widths = c(0.45, 0.55), align = "h")       

doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937

tikz(file.path(plots_path, "gaitrec-mean-covar.tex"),
     width = 1.5 * doc_width_inches, 
     height = 0.5 *  doc_width_inches)
mean_covar_plot
dev.off()
