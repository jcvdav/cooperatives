# figures
library(here)
library(ggbiplot)
library(startR)
library(corrplot)
library(rnaturalearth)
library(sf)
library(cowplot)
library(ggridges)
library(tidyverse)

update_geom_defaults(geom = "sf", new = list(fill = "gray90",
                                             color = "black",
                                             size = 1))

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
                                   T ~ "Social Sensitivity"))

# Map of components data
components_map <- ggplot(data = components_data) +
  geom_sf(data = coast) +
  geom_point(aes(x = lon, y = lat, fill = value), shape = 21, size = 2) +
  ggtheme_map() +
  scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
  facet_wrap(~variable_name, ncol = 2) +
  guides(fill = guide_colorbar(title = "Score",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.55, 0.1)) +
  labs(x = "", y = "")

ggsave(plot = components_map,
       filename = here("docs", "img", "components_map.png"),
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
       filename = here("docs", "img", "components_histogram.png"),
       width = 5,
       height = 5)

## Final map

score_map <- coop_clean %>% 
  dplyr::select(lon, lat, score) %>% 
  drop_na() %>% 
  ggplot() +
  geom_sf(data = coast) +
  geom_point(aes(x = lon, y = lat, fill = score), shape = 21, size = 2) +
  ggtheme_map() +
  scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
  guides(fill = guide_colorbar(title = "Score",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.05, 0.11)) +
  labs(x = "", y = "")

ggsave(plot = score_map,
       filename = here("docs", "img", "score_map.png"),
       width = 5,
       height = 5)

# Corrplot
png(filename = here("docs", "img","components_corrplot.png"),
    height = 6,
    width = 6,
    units = "in",
    res = 200)

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





## PCA

pca_data <- coop_clean %>%
  magrittr::set_rownames(value = paste(.$original_order,
                                       .$fishery_id,
                                       .$host_country,
                                       sep = "-")) %>%
  dplyr::select(social_capital,
                diversification,
                change_anticipation_adaptation,
                governmental_support,
                material_style_of_life,
                economic_dependence,
                food_dependence,
                sea_temp_vulnerability,
                overfishing,
                species_suceptibility,
                temperature_change,
                recovery_potential,
                mpa,
                managed_fishery)
pca_data %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot::corrplot(type = "lower", method = "ellipse", diag = F)

pca_data %>%
  dplyr::select(social_capital,
                diversification,
                change_anticipation_adaptation,
                governmental_support,
                material_style_of_life,
                economic_dependence,
                food_dependence) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  prcomp() %>% 
  ggbiplot(obs.scale = 1, var.scale = 1, circle = TRUE)


pca_data %>% 
  dplyr::select(sea_temp_vulnerability,
                overfishing,
                species_suceptibility,
                temperature_change,
                recovery_potential,
                mpa,
                managed_fishery) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  prcomp() %>% 
  ggbiplot::ggbiplot(obs.scale = 1, var.scale = 1, circle = TRUE)







