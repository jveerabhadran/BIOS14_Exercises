# ==========================================
# Load Required Libraries
# ==========================================
library(ggplot2)
library(dplyr)
library(glmmTMB)   
library(MuMIn)    

# ==========================================
# Load the Datasets
# ==========================================
male_CS <- read.csv("C:/Users/jyoth/OneDrive/Desktop/PG/BIOS14/Exam/Data/male_CS.csv")
male_CV <- read.csv("C:/Users/jyoth/OneDrive/Desktop/PG/BIOS14/Exam/Data/male_CV.csv")
female_CS <- read.csv("C:/Users/jyoth/OneDrive/Desktop/PG/BIOS14/Exam/Data/female_CS.csv")
female_CV <- read.csv("C:/Users/jyoth/OneDrive/Desktop/PG/BIOS14/Exam/Data/female_CV.csv")

# ==========================================
# Combine the Datasets into One Data Frame
# ==========================================
all_data <- bind_rows(male_CS, male_CV, female_CS, female_CV)

# Convert categorical variables to factors
all_data$sex <- as.factor(all_data$sex)
all_data$sp <- as.factor(all_data$sp)

# ==========================================
# Data Preprocessing
# ==========================================
summary(all_data)  # Summary of the data
anyNA(all_data)    # Check for missing values

# ==========================================
# Reference Group for Factors
# ==========================================
# Relevel sex to make "Male" the reference group
all_data$sex <- relevel(all_data$sex, ref = "Male")

# Relevel species to make CV the reference group
all_data$sp <- relevel(all_data$sp, ref = "CV")

# ==========================================
# ANOVA 
# ==========================================
# ANOVA for Body Size (Total Body Length)
anova_body_size <- aov(tbl ~ sex * sp, data = all_data)
anova_body_summary <- summary(anova_body_size)

# ANOVA for Wing Length (Forewing Length)
anova_wing_length <- aov(fwl ~ sex * sp, data = all_data)
anova_wing_summary <- summary(anova_wing_length)

# ==========================================
# Calculate R-squared for ANOVA Models
# ==========================================
# Body Size
sse_body <- anova_body_summary[[1]]["Residuals", "Sum Sq"]
sst_body <- sum(anova_body_summary[[1]][, "Sum Sq"])
r_squared_body <- 1 - (sse_body / sst_body)
cat("R-squared for Body Size ANOVA: ", r_squared_body, "\n")

# Wing Length
sse_wing <- anova_wing_summary[[1]]["Residuals", "Sum Sq"]
sst_wing <- sum(anova_wing_summary[[1]][, "Sum Sq"])
r_squared_wing <- 1 - (sse_wing / sst_wing)
cat("R-squared for Wing Length ANOVA: ", r_squared_wing, "\n")

# ==========================================
# Intercepts and Effect Sizes
# ==========================================
# Intercept (Mean for Reference Group: Male CV)
intercept_body <- coef(anova_body_size)["(Intercept)"]  # Mean Body Size
intercept_wing <- coef(anova_wing_length)["(Intercept)"]  # Mean Wing Length

# Contrasts (Differences in Means from Reference Group)
contrast_body_sex_sp <- coef(anova_body_size)["sexFemale:spCS"]
contrast_wing_sex_sp <- coef(anova_wing_length)["sexFemale:spCS"]

# Effect Sizes
effect_size_body <- (contrast_body_sex_sp / intercept_body) * 100
effect_size_wing <- (contrast_wing_sex_sp / intercept_wing) * 100

# Print Results
cat("Intercept (Body Size, Male CV): ", intercept_body, "\n")
cat("Intercept (Wing Length, Male CV): ", intercept_wing, "\n")
cat("Effect Size for Body Size (CS Females vs Reference): ", effect_size_body, "%\n")
cat("Effect Size for Wing Length (CS Females vs Reference): ", effect_size_wing, "%\n")

# ==========================================
# GLMM Models (Body Size and Wing Length)
# ==========================================
# GLMM for Body Size (tbl) with random effects
glmm_body_size <- glmmTMB(tbl ~ sex * sp + (1|year) + (1|id), data = all_data, family = gaussian(link = "identity"))

# GLMM for Wing Length (fwl) with random effects
glmm_wing_length <- glmmTMB(fwl ~ sex * sp + (1|year) + (1|id), data = all_data, family = gaussian(link = "identity"))

# ==========================================
# Compare Models Using AIC
# ==========================================
# AIC for Body Size Models
aic_body_size <- AIC(anova_body_size, glmm_body_size)
print("AIC for Body Size Models:")
print(aic_body_size)

# AIC for Wing Length Models
aic_wing_length <- AIC(anova_wing_length, glmm_wing_length)
print("AIC for Wing Length Models:")
print(aic_wing_length)

# ==========================================
# Visualization and Summary Tables
# ==========================================
# Boxplot for Body Size (tbl)
ggplot(all_data, aes(x = sex, y = tbl, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~sp) +
  labs(title = "Sexual Dimorphism in Body Size by Species", x = "Sex", y = "Total Body Length (mm)") +
  theme_minimal()

# Boxplot for Forewing Length (fwl)
ggplot(all_data, aes(x = sex, y = fwl, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~sp) +
  labs(title = "Sexual Dimorphism in Forewing Length by Species", x = "Sex", y = "Forewing Length (mm)") +
  theme_minimal()

# Interaction Plot for Body Size
ggplot(all_data, aes(x = sex, y = tbl, group = sp, color = sp)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Interaction: Body Size by Sex and Species", x = "Sex", y = "Total Body Length (mm)") +
  theme_minimal()

# Interaction Plot for Wing Length
ggplot(all_data, aes(x = sex, y = fwl, group = sp, color = sp)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Interaction: Forewing Length by Sex and Species", x = "Sex", y = "Forewing Length (mm)") +
  theme_minimal()

# ==========================================
# Summary Table: Mean, Standard Deviation, and Standard Error
# ==========================================
summary_table <- all_data %>%
  group_by(sp, sex) %>%
  summarise(
    Mean_Body_Size = mean(tbl, na.rm = TRUE),
    SD_Body_Size = sd(tbl, na.rm = TRUE),
    SE_Body_Size = SD_Body_Size / sqrt(n()),  
    Mean_Wing_Length = mean(fwl, na.rm = TRUE),
    SD_Wing_Length = sd(fwl, na.rm = TRUE),
    SE_Wing_Length = SD_Wing_Length / sqrt(n())  
  )

print(summary_table)




