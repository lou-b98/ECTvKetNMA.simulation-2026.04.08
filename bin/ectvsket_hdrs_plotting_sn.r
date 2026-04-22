##### HDRS-17 KETAMINE & ECT ##### 
hdrs17.dat <- read_excel(win.file,2)
hdrs17.dat <- as.data.frame(hdrs17.dat)

hdrs17.dat$study <- reorder(hdrs17.dat$study, hdrs17.dat$year)

# Ketamine HDRS Plot - 800 x 400
plot.hdrs17.ket.dur <- ggplot(data = hdrs17.dat,
                              aes(x = time.dur, y = ket.hdrs17.mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(colour = study),
             stroke = 1.5,
             size = 3) +
  geom_line(aes(colour = study), size = 1) + 
  geom_errorbar(aes(ymin = ket.hdrs17.mean - (ket.hdrs17.sd/sqrt(ket.hdrs17.n)), 
                    ymax = ket.hdrs17.mean + (ket.hdrs17.sd/sqrt(ket.hdrs17.n)), 
                    colour = study), 
                size = 1,
                width = 0.1) +
  labs(x = "Time (weeks)", y = "KETAMINE - HDRS-17") +
  theme_bw() +
  ylim(c(0, 27)) +
  guides(colour=guide_legend("Study")) +
  theme(text = element_text(size = TEXT.SIZE), 
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

# ECT HDRS Plot - 800 x 400
plot.hdrs17.ect.dur <- ggplot(data = hdrs17.dat,
                              aes(x = time.dur, y = ect.hdrs17.mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(colour = study),
             stroke = 1.5,
             size = 3) +
  geom_line(aes(colour = study), size = 1) + 
  geom_errorbar(aes(ymin = ect.hdrs17.mean - (ect.hdrs17.sd/sqrt(ect.hdrs17.n)), 
                    ymax = ect.hdrs17.mean + (ect.hdrs17.sd/sqrt(ect.hdrs17.n)), 
                    colour = study), 
                size = 1,
                width = 0.1) +
  labs(x = "Time (weeks)", y = "ECT - HDRS-17") +
  theme_bw() +
  ylim(c(0, 27)) +
  guides(colour=guide_legend("Study")) +
  theme(text = element_text(size = TEXT.SIZE), 
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

###### PLOT ECT SPLINE ######
res.hdrs17.spline.ect <- rma.mv(ect.hdrs17.mean, ect.hdrs17.sd^2,
                                mods = ~ ns(time.dur, df = 4), 
                                random = ~ 1 | study, 
                                method = "REML",
                                data = data[complete.cases(data$time.dur),])

pred.hdrs17.ect.dur <- predict(res.hdrs17.spline.ect)
pred.hdrs17.ect.dur$time.dur <- res.hdrs17.spline.ect$data$time.dur
pred.hdrs17.ect.dur <- as.data.frame(pred.hdrs17.ect.dur)
pred.hdrs17.ect.dur <- pred.hdrs17.ect.dur %>%
  arrange(time.dur)

plot.hdrs17.ect.dur <- ggplot(data = data,
                              aes(x = time.dur, y = ect.hdrs17.mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(colour = study),
             stroke = 1.5,
             size = 3) +
  geom_line(aes(colour = study), size = 1) + 
  geom_errorbar(aes(ymin = ect.hdrs17.mean - (ect.hdrs17.sd/sqrt(ect.hdrs17.n)), 
                    ymax = ect.hdrs17.mean + (ect.hdrs17.sd/sqrt(ect.hdrs17.n)), 
                    colour = study), 
                size = 1,
                width = 0.1) +
  # Add prediction line
  geom_line(data = pred.hdrs17.ect.dur, 
            aes(x = time.dur, 
                y = pred), 
            size = 2,
            colour = "black",
            linetype = "solid",
            alpha = 0.75) +
  geom_ribbon(data = pred.hdrs17.ect.dur, 
              aes(y = pred, ymin = ci.lb, ymax = ci.ub),
              colour = "black",
              alpha = 0.2) +
  labs(x = "Time (days)", y = "ECT - HDRS-17") +
  theme_bw() +
  ylim(c(0, 30)) +
  guides(colour = guide_legend("Study")) +
  theme(text = element_text(size = TEXT.SIZE), 
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

tiff(file = paste("hdrs17_ectspline.tiff"), width = 16, height = 8, units='cm', res = 300)
plot.hdrs17.ect.dur
dev.off()


###### PLOT KET SPLINE ######
res.hdrs17.spline.ket <- rma.mv(ket.hdrs17.mean, ket.hdrs17.sd^2,
                                mods = ~ ns(time.dur, df = 4), 
                                random = ~ 1 | study, 
                                method = "REML",
                                data = data[complete.cases(data$time.dur),])

pred.hdrs17.ket.dur <- predict(res.hdrs17.spline.ket)
pred.hdrs17.ket.dur$time.dur <- res.hdrs17.spline.ket$data$time.dur
pred.hdrs17.ket.dur <- as.data.frame(pred.hdrs17.ket.dur)
pred.hdrs17.ket.dur <- pred.hdrs17.ket.dur %>%
  arrange(time.dur)

plot.hdrs17.ket.dur <- ggplot(data = data,
                              aes(x = time.dur, y = ket.hdrs17.mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(colour = study),
             stroke = 1.5,
             size = 3) +
  geom_line(aes(colour = study), size = 1) + 
  geom_errorbar(aes(ymin = ket.hdrs17.mean - (ket.hdrs17.sd/sqrt(ket.hdrs17.n)), 
                    ymax = ket.hdrs17.mean + (ket.hdrs17.sd/sqrt(ket.hdrs17.n)), 
                    colour = study), 
                size = 1,
                width = 0.1) +
  # Add prediction line
  geom_line(data = pred.hdrs17.ket.dur, 
            aes(x = time.dur, 
                y = pred), 
            size = 2,
            colour = "black",
            linetype = "solid",
            alpha = 0.75) +
  geom_ribbon(data = pred.hdrs17.ket.dur, 
              aes(y = pred, ymin = ci.lb, ymax = ci.ub),
              colour = "black",
              alpha = 0.2) +
  labs(x = "Time (days)", y = "KET - HDRS-17") +
  theme_bw() +
  ylim(c(0, 30)) +
  guides(colour = guide_legend("Study")) +
  theme(text = element_text(size = TEXT.SIZE), 
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
plot.hdrs17.ket.dur

tiff(file = paste("hdrs17_ketspline.tiff"), width = 16, height = 8, units='cm', res = 300)
plot.hdrs17.ket.dur
dev.off()

###### PLOT ECT/KET SPLINE ######
# Difference of 2-points, on average, between the arms at baseline
# From t=0 meta-analysis
#   Ket HDRS-17: 23.0237
#   ECT HDRS-17: 25.0365  
# This is equivalent to size of 'benefit' of ketamine over ECT for week 1

# Combine the prediction data frames with a new column for condition
pred.hdrs17.ket.dur$condition <- "Ketamine"
pred.hdrs17.ect.dur$condition <- "ECT"

# Combine both data frames into one
pred_combined <- rbind(pred.hdrs17.ket.dur, pred.hdrs17.ect.dur)

# Plot ( #7570b3 and #7570b3)
plot.hdrs17.dur <- ggplot(data = pred_combined, aes(x = time.dur, y = pred, colour = condition)) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub, fill = condition), 
              alpha = 0.2) +  # Add confidence intervals
  geom_line(size = 2, alpha = 1.0) +  # Use a line to show the predictions
  labs(x = "Time (days)", y = "Predicted HDRS-17") +
  theme_bw() +
  theme(text = element_text(size = TEXT.SIZE), 
        legend.background = element_blank(),
        legend.title = element_blank(),  # Remove the legend title
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values = c("Ketamine" = "#1b9e77", "ECT" = "#7570b3")) +
  scale_fill_manual(values = c("Ketamine" = "#1b9e77", "ECT" = "#7570b3"))

tiff(file = paste("ectvsket_spline.tiff"), width = 16, height = 8, units='cm', res = 300)
plot.hdrs17.dur
dev.off()
