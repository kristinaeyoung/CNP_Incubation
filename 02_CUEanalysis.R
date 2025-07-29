---
  #  title: CUE data analysis
  # output:
  #  html_document:
  #  df_print: paged
  # pdf_document: default
  # date: "2024-01-28"
  ---
  
  # Reading in packages
  
library(ggplot2)
library(tidyverse)
library(readr)


# Looking at carbon use efficiency

selected_columns <- CO2_data %>%
  select(Trtmt, ug_CO2_C_gsoil, ubialC, Timepoint, Replicate)

summary <- selected_columns %>%
  group_by(Trtmt, Replicate) %>%
  summarise(avg_ug_CO2_C_gsoil = mean(ug_CO2_C_gsoil, na.rm = TRUE))

# Extract ubialC values at Timepoint "T6"
ubialC_T6 <- selected_columns %>%
  filter(Timepoint == "T6") %>%
  select(Trtmt, Replicate, ubialC)

summary <- summary %>%
  left_join(ubialC_T6, by = c("Trtmt", "Replicate"))

# Calculate the average of "ubial C" for rows where "Trtmt" is "dry"
control_avg_ubial <- summary %>%
  filter(Trtmt == "no_C_no_N_no_P") %>%
  summarise(mean_ubial_C = mean(ubialC, na.rm = TRUE)) %>%
  pull(mean_ubial_C)

control_avg_ug_CO2_C_gsoil <- summary %>%
  filter(Trtmt == "no_C_no_N_no_P") %>%
  summarise(mean_ug_CO2_C_gsoil = mean(avg_ug_CO2_C_gsoil, na.rm = TRUE)) %>%
  pull(mean_ug_CO2_C_gsoil)

# Create new columns with the average values for "dry" in "Trtmt"
summary <- summary %>%
  mutate(control_avg_ubial = control_avg_ubial,
         control_avg_mg_CO2_C_gsoil = control_avg_mg_CO2_C_gsoil)

# Remove rows where "Trtmt" is "dry"

summary <- summary %>%
  filter(Trtmt != "dry")

summary <- summary %>%
  filter(Trtmt != "no_C_low_N_no_P")

summary <- summary %>%
  filter(!str_detect(Trtmt, "^no_C"))

# Create a new column with the specified calculation
summary <- summary %>%
  mutate(new_column = (avg_ug_CO2_C_gsoil - control_avg_ug_CO2_C_gsoil) / (ubialC - control_avg_ubial))


summary <- summary %>%
  mutate(new_column = (ubialC - control_avg_ubial) / (avg_ug_CO2_C_gsoil - control_avg_ug_CO2_C_gsoil))

summary <- summary %>%
  mutate(new_column = ifelse(new_column > 100, NA, new_column))


ggplot(summary, aes(x = Trtmt, 
                    y = new_column)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))


ggplot(CUE_real, aes(x = Trtmt, 
                     y = CUE)) + 
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))

model_CUE <- aov(CUE ~ Trtmt, data = CUE_real)
summary(model_CUE)

TukeyHSD(model_CUE)
