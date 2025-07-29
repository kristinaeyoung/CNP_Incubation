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


setwd("C:/Users/Kristina/OneDrive - New Mexico State University/Desktop/GIT REPOs/CPN_Incubation/CNP_Incubation")

CO2_data <- read.csv("2024_01_28_CO2_data.csv")
View(CO2_data)

# Determining amounts of C in the soil
CO2_conversions <- CO2_data %>%
  mutate(
    extC_mg = extC / 1000,              # Convert from micrograms to milligrams
    extC_percent = (extC * 1e-6) * 100  # Convert from micrograms to percentage
  )

# View the updated data frame
head(CO2_conversions)

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

Graphing the data
total ppm/min
```{r}
CO2_data$Trtmt <- as.factor(CO2_data$Trtmt)
CO2_data$total_ppm_min <- as.numeric(CO2_data$total_ppm_min)
CO2_data$ubialC <- as.numeric(CO2_data$ubialC)
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
```

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


Looking at average ug/CO2-C/g soil/min, averaged over the 6 timepoints

```{r}
graphing <- CO2_data %>%
  group_by(Trtmt) %>%
  summarise(
    average_ug_CO2_C_gsoil_min = average(ugCO2C_gsoil_min))


ggplot(CO2_data, aes(x = Trtmt, 
                     y = ugCO2C_gsoil_min,
                     color = Camt)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))

```{r}
selected_values <- c("no_C_no_N_no_P", "high_C_high_N_high_P", "no_C_high_N_high_P", "high_C_high_N_no_P", "high_C_no_N_high_P")

# Filter the data frame to create a new data frame with only the specified values
new_df <- dplyr::filter(CO2_data, Trtmt %in% selected_values)

# View the new data frame
print(new_df)


ggplot(new_df, aes(x = Trtmt, 
                   y = ubialC)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))

model <- aov(ubialC^2 ~ Trtmt, data = new_df)
summary(model)
plot(model)
TukeyHSD(model)

summarised_df <- new_df %>%
  group_by(across(-c(Timepoint, mg_CO2_C_gsoil))) %>%
  summarise(total_mg_CO2_C_gsoil = sum(mg_CO2_C_gsoil, na.rm = TRUE), .groups = 'drop')

# View the new summarised data frame
print(summarised_df)


ggplot(summarised_df, aes(x = Trtmt, 
                          y = total_mg_CO2_C_gsoil)) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.title.y = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.line = element_line(colour = "black"))

model2 <- aov(log(total_mg_CO2_C_gsoil) ~ Trtmt, data = summarised_df)
summary(model2)
plot(model2)
TukeyHSD(model2)


