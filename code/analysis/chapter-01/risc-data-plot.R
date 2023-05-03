library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.4.0
library(tikzDevice) # CRAN v0.12.3.1
source("code/functions/theme_gunning.R")
theme_gunning()
risc1_plot_dt <- readRDS("data/risc1-intro-plot.rds")
plots_path <- here::here("figures")
doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937
risc1_plot_dt_lng <- melt.data.table(risc1_plot_dt,
                                     measure.vars = paste0("data_", 0:197),
                                     variable.factor = FALSE,
                                     variable.name = "time",
                                     na.rm = TRUE)

risc1_plot_dt_lng[, time := as.numeric(stringr::str_remove(time, "data_"))]

risc1_plot_dt_lng[,
  plane_of_motion := factor(plane_of_motion,
                            levels = c("fle", "abd", "rot"),
                            labels = c("\\emph{flexion}", "\\emph{abduction}", "\\emph{rotation}"))
]
risc1_plot_dt_lng[, side := factor(side, levels = c("left", "right"),
                                   labels = c("Left", "Right"))]

p <- ggplot(data = risc1_plot_dt_lng) +
  aes(x = 5 * time, y = value,
      group = interaction(stride_num, side, subject_id),
      color = side) +
  labs(x = "Time (milliseconds)",
       y ="Angle ($^{\\circ}$)") +
  geom_line(alpha = 0) +
  geom_line() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("red3", "#619CFF"), name = "\\textbf{Side:}") +
  facet_wrap( ~ location + plane_of_motion, scales = "free") +
  theme(legend.position = "top")




tikz(file.path(plots_path, "risc-data-plot.tex"),
     width = 1.5 * doc_width_inches, 
     height = 0.7 *  doc_width_inches)
p
dev.off()
