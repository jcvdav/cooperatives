# figures

library(ggbiplot)
library(startR)
library(corrplot)
library(rnaturalearth)
library(sf)
library(cowplot)
library(here)
library(tidyverse)

# Load the data
coop_clean <- read.csv(file = here("data", "clean_cooperatives_data.csv"),
                       stringsAsFactors = F)


# Create a data.frame for components data
components_data <- coop_clean %>% 
  select(lon,
         lat,
         social_adaptive_capacity,
         social_sensitivity,
         ecological_sensitivity,
         ecological_exposure,
         ecological_recovery_potential) %>% 
  gather(variable, value, -c(lon, lat)) %>% 
  drop_na() %>% 
  mutate(variable_name = case_when(variable == "ecological_exposure" ~ "Ecological Exposure",
                                   variable == "ecological_recovery_potential" ~ "Ecological Recovery Potential",
                                   variable == "ecological_sensitivity" ~ "Ecological Sensitivity",
                                   variable == "social_adaptive_capacity" ~ "Social Adaptive Capacity",
                                   T ~ "Socias Sensitivity"))

# Map of components data
components_map <- ggplot(data = components_data) +
  geom_sf(data = coast) +
  geom_point(aes(x = lon, y = lat, fill = value), shape = 21, size = 2) +
  ggtheme_plot() +
  scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
  facet_wrap(~variable_name, ncol = 2) +
  guides(fill = guide_colorbar(title = "Score",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.55, 0.1)) +
  labs(x = "", y = "")

ggsave(plot = components_map,
       filename = here("docs", "img", "components_map.pdf"),
       width = 7,
       height = 7)

# Histogram of components
components_histogram <- ggplot(data = components_data,
                               aes(x = value, y = variable_name)) +
  geom_density_ridges(fill = "steelblue",
                      alpha = 0.5) +
  ggtheme_plot() +
  labs(x = "Score", y = "Component")

ggsave(plot = components_histogram,
       filename = here("docs", "img", "components_histogram.pdf"),
       width = 5,
       height = 5)

# Corrplot
pdf(file = here("docs", "img","components_corrplot.pdf"),
    height = 6,
    width = 6)

coop_clean %>% 
  select(social_adaptive_capacity,
         social_sensitivity,
         ecological_sensitivity,
         ecological_exposure,
         ecological_recovery_potential) %>% 
  magrittr::set_colnames(value = c("SAC", "SS", "ES", "EE", "ERP")) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  cor() %>% 
  corrplot(corr = .,
           type = "lower",
           method = "ellipse",
           diag = F,
           outline = T,
           tl.col = "black",
           tl.srt = 0,
           cl.pos = "r",
           cl.cex = 1,
           cl.ratio = 0.5)

dev.off()
















