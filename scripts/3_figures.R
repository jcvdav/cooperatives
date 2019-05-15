# figures
library(startR)
library(ggrepel)
library(magrittr)
library(corrplot)
library(rnaturalearth)
library(sf)
library(cowplot)
library(ggridges)
library(here)
library(tidyverse)

source(here("scripts", "ggbiplot.R"))

update_geom_defaults(geom = "sf", new = list(fill = "gray90",
                                             color = "black",
                                             size = 0.1))

proj2 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

coast <- ne_countries(scale = "small",
                      type = "countries",
                      returnclass = "sf") %>% 
  st_transform(proj2) %>% 
  st_set_precision(1e6) %>% 
  st_union()

# Load the data
coop_clean <- read.csv(file = here("data", "clean_cooperatives_data.csv"),
                       stringsAsFactors = F) %>% 
  mutate(region = countrycode(sourcevar = host_country,
                              origin = "country.name",
                              destination = "continent"),
         region = ifelse(host_country == "Scotland", "Europe", region))


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
       aes(x = value, y = ..scaled..,  fill = variable_name)) +
  geom_density(alpha = 0.5) +
  ggtheme_plot() +
  labs(x = "Score", y = "Density") +
  guides(fill = guide_legend(title = "Component")) +
  scale_fill_brewer(palette = "Set1")

ggsave(plot = components_histogram,
       filename = here("docs", "img", "components_histogram.png"),
       width = 6,
       height = 3)

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
       width = 6,
       height = 3)

# Distribution of final scores

score_dist <- coop_clean %>% 
  dplyr::select(lon, lat, score) %>% 
  drop_na() %>% 
  ggplot(aes(x = score)) +
  geom_density(color = "black",
               size = 1,
               fill = "steelblue",
               alpha = 0.8) +
  labs(x = "Score", y = "Density")

ggsave(plot = score_dist,
       filename = here("docs", "img", "score_dist.png"),
       width = 5,
       height = 3)

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

## Socioeconomic PCA

pca_data <- coop_clean %>%
  magrittr::set_rownames(value = paste(.$original_order,
                                       .$fishery_id,
                                       .$host_country,
                                       sep = "-")) %>%
  dplyr::select(region,
                social_capital,
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

png(filename = here("docs", "img","dimensions_corrplot.png"),
    height = 6.5,
    width = 6,
    units = "in",
    res = 200)

pca_data %>% 
  select(-region) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot::corrplot(corr = .,
                     type = "lower",
                     method = "ellipse",
                     diag = F,
                     outline = T,
                     tl.col = "black",
                     # tl.srt = 0,
                     cl.pos = "r",
                     cl.cex = 1,
                     cl.ratio = 0.5)

dev.off()

regions_soc_pca <- pca_data %>%
  dplyr::select(region,
                social_capital,
                diversification,
                change_anticipation_adaptation,
                governmental_support,
                material_style_of_life,
                economic_dependence,
                food_dependence) %>% 
  magrittr::set_colnames(., value = str_replace_all(colnames(.), "_", " ")) %>% 
  drop_na() %$%
  region

soc_biplot <- pca_data %>%
  dplyr::select(social_capital,
                diversification,
                change_anticipation_adaptation,
                governmental_support,
                material_style_of_life,
                economic_dependence,
                food_dependence) %>% 
  magrittr::set_colnames(., value = str_replace_all(colnames(.), "_", " ")) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  prcomp() %>% 
  ggbiplot(obs.scale = 1, var.scale = 1,
           circle = TRUE,
           varname.size = 3,
           groups = regions_soc_pca) +
  scale_x_continuous(limits = c(-0.7, 0.7)) +
  scale_y_continuous(limits = c(-0.7, 0.7)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = "Continent"))

ggsave(plot = soc_biplot,
       filename = here("docs", "img", "soc_biplot.png"),
       width = 5,
       height = 5)

## Ecological PCA

regions_eco_pca <- pca_data %>%
  dplyr::select(region,
                sea_temp_vulnerability,
                overfishing,
                species_suceptibility,
                temperature_change,
                recovery_potential,
                mpa,
                managed_fishery) %>% 
  magrittr::set_colnames(., value = str_replace_all(colnames(.), "_", " ")) %>% 
  drop_na() %$%
  region

ecol_biplot <- pca_data %>% 
  dplyr::select(sea_temp_vulnerability,
                overfishing,
                species_suceptibility,
                temperature_change,
                recovery_potential,
                mpa,
                managed_fishery) %>% 
  magrittr::set_colnames(., value = str_replace_all(colnames(.), "_", " ")) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  prcomp() %>% 
  ggbiplot(obs.scale = 1, var.scale = 1,
           circle = TRUE,
           varname.size = 3,
           groups = regions_eco_pca) +
  scale_x_continuous(limits = c(-0.8, 0.8)) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = "Continent"))

ggsave(plot = ecol_biplot,
       filename = here("docs", "img", "ecol_biplot.png"),
       width = 5,
       height = 5)





