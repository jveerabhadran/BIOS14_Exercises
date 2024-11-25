# Loading the dataset
dat <- read.csv("butterflies.csv")
names(dat)

# Prepare data columns by appending "M" for maternal host and "L" for larval host
dat$MaternalHost <- paste0(dat$MaternalHost, "M")
dat$LarvalHost <- paste0(dat$LarvalHost, "L")
means <- tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means

# Conduct two-way ANOVA on DevelopmentTime with factors MaternalHost and LarvalHost
anova_model <- aov(DevelopmentTime ~ MaternalHost * LarvalHost, data = dat)
summary(anova_model)

# Calculate means for the interaction plot
means <- tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
se <- function(x) sd(x) / sqrt(length(x))
std_errors <- with(dat, tapply(DevelopmentTime, list(MaternalHost, LarvalHost), se))

# Data for plotting
plot_data <- data.frame(
  MaternalHost = rep(c("BarbareaM", "BerteroaM"), each = 2),
  LarvalHost = rep(c("BarbareaL", "BerteroaL"), times = 2),
  MeanDevelopmentTime = as.vector(means),
  SE = as.vector(std_errors)
)

# Plot with ggplot2
ggplot(plot_data, aes(x = LarvalHost, y = MeanDevelopmentTime, group = MaternalHost, color = MaternalHost)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = MeanDevelopmentTime - SE, ymax = MeanDevelopmentTime + SE), width = 0.1) +
  labs(x = "Larval Host", y = "Development Time (days)", color = "Maternal Host") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle("Larval Development Time by Larval and Maternal Host Plant")
