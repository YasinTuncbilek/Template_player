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
library(here)

# Load data
data <- read_excel("Deep_completed_passes.xlsx", col_names = TRUE)

# Change NA to zero
data[is.na(data)] <- 0

# Create labels for the plot
data$Label <- paste0(data$Player, ", ", data$Age, " jr., ", data$Team)

# Filter players on > 200 minutes played
data2 <- data %>%
  filter(`Minutes played` > 200)

# Get value of Víctor Gómez on deep completed passes per 90 minutes
victor_gomez <- subset(data2$`Deep completed passes per 90`, data$Player == "Víctor Gómez")

# Get players with minimum and maximum value on deep completed passes per 90 minuten
min <- subset(data2, `Deep completed passes per 90` == min(`Deep completed passes per 90`))
max <- subset(data2, `Deep completed passes per 90` == max(`Deep completed passes per 90`)) 
min_max <- rbind(min, max)

# Create plot: deep completed passes
plot <- data2 %>%
  filter(Player != "Víctor Gómez") %>%
  ggplot(aes(x = factor(0), y = `Deep completed passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#AA151B", colour = "#F1BF00", size = 12, shape = 21, stroke = 1) +
  ggtitle("Deep completed passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie (RB & RWB) en met meer dan 200 gespeelde minuten.") +
  labs(caption = "Data zijn tot en met de dertiende speelronde in La Liga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, size = 10, face = "italic"),
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
  geom_point(aes(y = victor_gomez), shape = 21, colour = "white", fill = "#2069aa", size = 16, shape = 21, stroke = 1) +
  coord_flip() +
  geom_label_repel(data = data2 %>% filter(Player == "Víctor Gómez"), 
                   aes(y = `Deep completed passes per 90`, label = Label),
                   label.size = 1,
                   box.padding   = 0.35,
                   point.padding = 0,
                   vjust = 4,
                   size = 3.5,
                   segment.color = "Black",
                   segment.size = 1,
                   nudge_x = 0.25,
                   nudge_y = 0.25) +
  geom_label_repel(data = max, 
                   aes(y = `Deep completed passes per 90`, label = Label),
                   label.size = 1,
                   box.padding = 0.35,
                   point.padding = 0,
                   vjust = 4,
                   size = 3.5,
                   segment.color = "Black",
                   segment.size = 1) +
  geom_label_repel(data = min %>% filter(Player == "Ximo Navarro"), 
                   aes(y = `Deep completed passes per 90`, label = Label),
                   label.size = 1,
                   box.padding = 0.35,
                   point.padding = 0,
                   hjust = 0.8,
                   vjust = 4,
                   size = 3.5,
                   segment.color = "Black",
                   segment.size = 1,
                   nudge_x = 0.25,
                   nudge_y = 0.25)

plot

+
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

# Call back the plot
plot <- image_read(paste0(here("/"), "Deep completed passes per 90 minuten.png"))

# Upload FoForTho logo
logo_raw <- image_read("Logo FoForTho.png")
logo <- logo_raw %>%
  image_scale("50") %>%
  image_background("grey", flatten = TRUE) %>%
  image_border("grey", "600x10") %>%
  image_annotate("FoForTho | @fofortho", color = "white", size = 20, 
                 location = "+10+25", gravity = "northeast") 

# Stack them on top of each other
final_plot <- image_append(image_scale(c(plot, logo), "2000"), stack = TRUE)

final_plot

## Save Plot
magick::image_write(
  image = final_plot, 
  path = here::here("Deep completed passes per 90 minuten_met logo.png"))







mtcars







add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.001 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}

plot_logo <- add_logo(
  plot_path = here::here("Deep completed passes per 90 minuten.png"),
  logo_path = here::here("Logo FoForTho.png"),
  logo_position = "top right",
  logo_scale = 20)

plot_logo

## Save Plot
magick::image_write(
  image = plot_logo, 
  path = here::here("Deep completed passes per 90 minuten_met logo.png"))





