# ==========================================
# Load Required Libraries
# ==========================================
library(glmmTMB)
library(ggplot2)
library(dplyr)
library(tidyr)
library(MuMIn)

# ==========================================
# Load the Dataset
# ==========================================
dat <- read.table("C:\\Users\\jyoth\\OneDrive\\Desktop\\PG\\BIOS14\\Assignment\\midterm\\exam2022_part2-1.txt", header = TRUE)
summary(dat)

# ==========================================
# Data Preprocessing
# ==========================================
dat$age <- as.factor(dat$age)
dat$sex <- as.factor(dat$sex)
dat$cohort <- as.factor(dat$cohort)
dat$season <- as.factor(dat$season)

# Calculate average horn length and asymmetry
dat$avg_horn <- (dat$hornL + dat$hornR) / 2
dat$asymmetry <- abs(dat$hornL - dat$hornR)

# ==========================================
# Run ANOVA Models (Simpler Fixed Effect Models)
# ==========================================
anova_mass <- aov(mass ~ age * sex, data = dat)
summary(anova_mass)

anova_horn <- aov(avg_horn ~ age * sex, data = dat)
summary(anova_horn)

# ==========================================
# Fit GLMM for Body Mass and Horn Length
# ==========================================
model_mass <- glmmTMB(mass ~ age * sex + (1 | cohort) + (1 | season), data = dat, family = gaussian(link = "identity"))
model_horn <- glmmTMB(avg_horn ~ age * sex + (1 | cohort) + (1 | season), data = dat, family = gaussian(link = "identity"))

# ==========================================
# Model Selection Using AIC
# ==========================================
aic_mass <- AIC(model_mass, anova_mass)
print("AIC for Body Mass Models:")
print(aic_mass)

aic_horn <- AIC(model_horn, anova_horn)
print("AIC for Horn Length Models:")
print(aic_horn)

# ==========================================
# Visualization: Boxplots and Interaction Plots
# ==========================================
ggplot(dat, aes(x = age, y = mass, fill = sex)) + 
  geom_boxplot() + 
  labs(title = "Body Mass by Age and Sex", x = "Age", y = "Body Mass (kg)") + 
  theme_minimal()

ggplot(dat, aes(x = age, y = avg_horn, fill = sex)) + 
  geom_boxplot() + 
  labs(title = "Average Horn Length by Age and Sex", x = "Age", y = "Average Horn Length (mm)") + 
  theme_minimal()

ggplot(dat, aes(x = age, y = mass, group = sex, color = sex)) + 
  stat_summary(fun = mean, geom = "line", size = 1) + 
  labs(title = "Interaction: Body Mass by Age and Sex", x = "Age", y = "Body Mass (kg)") + 
  theme_minimal()

ggplot(dat, aes(x = age, y = avg_horn, group = sex, color = sex)) + 
  stat_summary(fun = mean, geom = "line", size = 1) + 
  labs(title = "Interaction: Horn Length by Age and Sex", x = "Age", y = "Average Horn Length (mm)") + 
  theme_minimal()

# ==========================================
# Summary Table: Mean and Standard Deviation
# ==========================================
summary_table <- dat %>%
  group_by(age, sex) %>%
  summarise(
    Mean_Mass = mean(mass, na.rm = TRUE),
    SD_Mass = sd(mass, na.rm = TRUE),
    Mean_Horn = mean(avg_horn, na.rm = TRUE),
    SD_Horn = sd(avg_horn, na.rm = TRUE)
  )
print(summary_table)



