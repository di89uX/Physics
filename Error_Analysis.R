# Load required libraries
library(ggplot2)
library(errors)

# Load the Data
data <- read.csv("schrg.csv")
View(data)

# Define uncertainties for voltage and current (example values)
voltage_uncertainty <- 0.01  # Example: 10 mV uncertainty
current_uncertainty <- 0.001   # Example: 1 mA uncertainty

# Create errors objects for voltage and current
data$Voltage_with_error <- with_uncertainty(data$Voltage..V., voltage_uncertainty)
data$I_with_error <- with_uncertainty(sqrt(data$I.2), current_uncertainty)

# Scatter Plot with error bars
ggplot(data, aes(x = I.2, y = Voltage..V.)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = Voltage..V. - voltage_uncertainty, 
                    ymax = Voltage..V. + voltage_uncertainty), width = 0.01) +
  ggtitle("The graph of V vs I^2 with Error Bars") +
  xlab("I^2(A^2)") +
  ylab("V(V)")

# Fit a linear Model
model <- lm(Voltage..V. ~ I.2, data = data)

# Add Regression Line
ggplot(data, aes(x = I.2, y = Voltage..V.)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("The graph of V vs I^2 with Fitted Line") +
  xlab("I^2(A^2)") +
  ylab("V(V)")

# Print model summary
model_summary <- summary(model)
print(model_summary)

# Calculate the specific charge
slope <- coef(model)[2]  # Extract slope (coefficient of I.2)

# Given parameters
r <- 0.08  # radius in meters
n <- 130  # number of coils
R <- 0.15  # coil radius in meters
mu_0 <- 4 * pi * 10^(-7)  # permeability of vacuum in T*m/A

# Calculate k
k <- (mu_0 * (4/5)^(2/3) * n * R)

# Calculate e/m and its uncertainty
specific_charge <- (2 * slope) / (r^2 * k^2)

# Calculate uncertainty in specific charge using error propagation
# Assuming independent uncertainties
slope_uncertainty <- summary(model)$coefficients[2, 2]  # Standard error of the slope
k_uncertainty <- 0.01 * k  # Example: 1% uncertainty in k

# Propagation of uncertainty formula: 
# If z = f(x, y), then the uncertainty in z is given by:
# σ_z = sqrt((∂f/∂x * σ_x)^2 + (∂f/∂y * σ_y)^2)
specific_charge_uncertainty <- sqrt(
  (2 / (r^2 * k^2) * slope_uncertainty)^2 +
    (2 * slope / (r^2 * k^3) * k_uncertainty)^2
)

# Output the specific charge and its uncertainty
cat("The specific charge of the electron (e/m) is:", specific_charge, "C/kg\n")
cat("The uncertainty in the specific charge is:", specific_charge_uncertainty, "C/kg\n")