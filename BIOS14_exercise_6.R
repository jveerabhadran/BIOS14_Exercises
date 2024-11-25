# Load necessary library
library(MASS)  # For negative binomial GLM

# Load the data
dat = read.csv("C:\\Users\\jyoth\\OneDrive\\Desktop\\PG\\BIOS14\\Data\\Eulaema.csv")

# Inspect the data
head(dat)
summary(dat)

# Step 1: Data preparation
# Create mean-centered predictor for MAP (Mean Annual Precipitation)
dat$mcMAP <- dat$MAP - mean(dat$MAP, na.rm = TRUE)

# Step 2: Fit the negative binomial GLM
m <- glm.nb(Eulaema_nigrita ~ mcMAP + forest., data = dat)

# Step 3: Summarize the model results
summary(m)

# Calculate pseudo R^2
pseudo_r2 <- 1 - m$deviance / m$null.deviance
print(pseudo_r2)

# Step 4: Visualize predictions
# Create a sequence for forest cover
newforest <- seq(min(dat$forest., na.rm = TRUE), max(dat$forest., na.rm = TRUE), length.out = 200)

# Predict abundance at three precipitation levels (mean, mean ± SD)
newMAP_mean <- rep(mean(dat$mcMAP, na.rm = TRUE), length(newforest))
newMAP_plus <- rep(mean(dat$mcMAP, na.rm = TRUE) + sd(dat$mcMAP, na.rm = TRUE), length(newforest))
newMAP_minus <- rep(mean(dat$mcMAP, na.rm = TRUE) - sd(dat$mcMAP, na.rm = TRUE), length(newforest))

# Predict responses
pred_mean <- predict(m, newdata = list(mcMAP = newMAP_mean, forest. = newforest), type = "response")
pred_plus <- predict(m, newdata = list(mcMAP = newMAP_plus, forest. = newforest), type = "response")
pred_minus <- predict(m, newdata = list(mcMAP = newMAP_minus, forest. = newforest), type = "response")

# Plot the results
plot(dat$forest., dat$Eulaema_nigrita, col = "grey", las = 1,
     xlab = "Forest cover",
     ylab = expression(paste(italic("El. nigrita"), " abundance")))
lines(newforest, pred_mean, lwd = 2, col = "black")
lines(newforest, pred_plus, lwd = 2, col = "blue")
lines(newforest, pred_minus, lwd = 2, col = "red")

# Add a legend
legend("topleft", lty = 1, lwd = 2, col = c("black", "blue", "red"), bty = "n",
       legend = c("MAP = Mean", "MAP = Mean + SD", "MAP = Mean - SD"))
