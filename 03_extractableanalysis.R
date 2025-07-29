---
  #  title: extractable pools (C,N,P) analysis
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
library(dplyr)
library(viridis)

# Reading in the data

CO2_data <- X2024_01_28_CO2_data
View(CO2_data)


# Filter to just Timepoint T6
CO2_T6 <- CO2_data %>%
  filter(Timepoint == "T6")

# Ensure treatment columns are factors
CO2_T6$Camt <- factor(CO2_T6$Camt)
CO2_T6$Namt <- factor(CO2_T6$Namt)
CO2_T6$Pamt <- factor(CO2_T6$Pamt)

# Plot: ubialC across treatment levels (faceted by P level)
ggplot(CO2_T6, aes(x = Namt, y = ubialC, color = Camt)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  geom_boxplot(aes(group = interaction(Camt, Namt)), alpha = 0.3, outlier.shape = NA) +
  geom_point() +
  facet_wrap(~ Pamt, labeller = label_both) +
  labs(
    title = "Microbial Biomass C (ubialC) at T6",
    x = "Nitrogen Level",
    y = "Microbial Biomass C",
    color = "Carbon Level"
  ) +
  theme_minimal(base_size = 14)

# --- Heatmap: Microbial Biomass C at Timepoint T6 ---

# Summarize across replicates
heatmap_data_ubialC <- CO2_T6 %>%
  group_by(Camt, Namt, Pamt) %>%
  summarize(mean_ubialC = mean(ubialC, na.rm = TRUE))

# Create heatmap
ggplot(heatmap_data_ubialC, aes(x = factor(Namt), y = factor(Pamt), fill = mean_ubialC)) +
  geom_tile(color = "white") +
  facet_wrap(~ Camt, labeller = label_both) +
  scale_fill_viridis_c(name = "Microbial Biomass C") +
  labs(
    title = "Microbial Biomass C (T6)",
    x = "Nitrogen Level",
    y = "Phosphorus Level"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

# --- Three-Way ANOVA: Respiration at Timepoint T6 ---

# Run the ANOVA using mg_CO2_C_gsoil (respiration)

model <- aov(ubialC~ Camt * Namt * Pamt, data = CO2_T6)
summary(model)


# Optional: diagnostic plots
plot(model)
# Extract residuals from the ANOVA model
residuals_model <- residuals(model)

# Run Shapiro-Wilk test on residuals
shapiro.test(residuals_model)




# Calculate max, min, and mean for extC_mg and extC_percent
summary_stats <- CO2_conversions %>%
  summarise(
    max_extC_mg = max(extC_mg, na.rm = TRUE),
    min_extC_mg = min(extC_mg, na.rm = TRUE),
    mean_extC_mg = mean(extC_mg, na.rm = TRUE),
    max_extC_percent = max(extC_percent, na.rm = TRUE),
    min_extC_percent = min(extC_percent, na.rm = TRUE),
    mean_extC_percent = mean(extC_percent, na.rm = TRUE)
  )


microbial C
```{r}
CO2_data$ubialC <- as.numeric(CO2_data$ubialC)
ggplot(CO2_data, aes(x = Trtmt, 
                     y = ubialC,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```

extractable C
```{r}
ggplot(CO2_data, aes(x = Trtmt, 
                     y = extC, 
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```


microbial N
```{r}
CO2_data$ubialN <- as.numeric(CO2_data$ubialN)
ggplot(CO2_data, aes(x = Trtmt, 
                     y = ubialN,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```
extractable N
```{r}
ggplot(CO2_data, aes(x = Trtmt, 
                     y = extN,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```
microbial P
```{r}
CO2_data$ubialP <- as.numeric(CO2_data$ubialP)
ggplot(CO2_data, aes(x = Trtmt, 
                     y = ubialP,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```
extractable P
```{r}
CO2_data$extP <- as.numeric(CO2_data$extP)
ggplot(CO2_data, aes(x = Trtmt, 
                     y = extP,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))
```




