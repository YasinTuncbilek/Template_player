# Load relevant packages
library(beeswarm)
library(ggplot2)
library(ggbeeswarm)
library(readxl)
library(ggpubr)
library(ggrepel)
library(dplyr)
library(magick)
library(magrittr)

# Load data
data <- read_excel("Deep_completed_passes.xlsx", col_names = TRUE)

# Change NA to zero
data[is.na(data)] <- 0

# Get value of Víctor Gómez on deepl completed passes per 90 minutes
victor_gomez <- subset(data$`Deep completed passes per 90`, data$Player == "Víctor Gómez")

# Create plot: deep completed passes
plot <- data %>%
  filter(Player != "Víctor Gómez") %>%
  ggplot(aes(x = factor(0), y = `Deep completed passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Deep completed passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "XXX Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "black", size = 2)) +
  stat_summary(fun.y = median,
               fun.ymin = median,
               fun.ymax = median,
               geom = "crossbar", 
               colour = "black",
               width = 1.2,
               size = 1.3) +
  scale_y_continuous(limits = c(0, 2.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +
  geom_point(aes(y = victor_gomez), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot

+
  




# Load logo

