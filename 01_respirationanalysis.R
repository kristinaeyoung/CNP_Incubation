---
  #  title: "2024_01_28_CO2_data"
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

# Reading in the data

CO2_data <- X2024_01_28_CO2_data
View(CO2_data)

# create a heatmap
library(ggplot2)
library(dplyr)

# Summarize data if there are replicates
heatmap_data <- CO2_data %>%
  group_by(Camt, Namt, Pamt) %>%
  summarize(mean_CO2 = mean(mg_CO2_C_gsoil, na.rm = TRUE))

# Create heatmap
ggplot(heatmap_data, aes(x = factor(Namt), y = factor(Pamt), fill = mean_CO2)) +
  geom_tile(color = "white") +
  facet_wrap(~ Camt, labeller = label_both) +
  scale_fill_viridis_c(name = "Respiration (mg CO2-C/g soil)") +
  labs(
    x = "Carbon Level",
    y = "Phosphorus Level"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))














# Determining amounts of C in the soil
CO2_conversions <- CO2_data %>%
  mutate(
    extC_mg = extC / 1000,              # Convert from micrograms to milligrams
    extC_percent = (extC * 1e-6) * 100  # Convert from micrograms to percentage
  )

# View the updated data frame
head(CO2_conversions)



# View the summary statistics
summary_stats
```

Renaming variables for graphing

```{r}
CO2_data$Camt <- as.factor(CO2_data$Camt)
CO2_data$Namt <- as.factor(CO2_data$Namt)
CO2_data$Pamt <- as.factor(CO2_data$Pamt)
CO2_data$Trtmt <- as.factor(CO2_data$Trtmt)

```

# Graphing the data
# total ppm/min
```{r}
CO2_data$Trtmt <- as.factor(CO2_data$Trtmt)
CO2_data$total_ppm_min <- as.numeric(CO2_data$total_ppm_min)

ggplot(CO2_data, aes(x = Trtmt, 
                     y = total_ppm_min, 
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```
Graphing the data
24hr CO2 sum
```{r}
ggplot(CO2_data, aes(x = Trtmt, 
                     y = T24hr_CO2_sum,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```
Graphing the data
48hr CO2 sum
```{r}
ggplot(CO2_data, aes(x = Trtmt, 
                     y = T48hr_CO2_sum,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))

