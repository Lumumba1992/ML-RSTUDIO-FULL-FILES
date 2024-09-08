```{r}
# Load necessary libraries
library(MASS)  # for mvrnorm function
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 1000

# Generate random continuous predictors with realistic ranges and distributions
continuous_predictors_df <- data.frame(
  Temperature = rnorm(n, mean = 27, sd = 5),  # Average temperature in tropical regions
  Rainfall = rnorm(n, mean = 150, sd = 50),   # Monthly rainfall in mm
  Humidity = rnorm(n, mean = 75, sd = 10),    # Percentage
  Elevation = rnorm(n, mean = 500, sd = 300), # Meters above sea level
  NDVI = runif(n, min = 0, max = 1),          # Normalized Difference Vegetation Index
  Population_Density = rpois(n, lambda = 300),# People per square km
  Distance_to_Water = rnorm(n, mean = 10, sd = 5), # Km
  Travel_Time = rnorm(n, mean = 30, sd = 15), # Minutes
  Healthcare_Access = rnorm(n, mean = 70, sd = 20),# Percentage of population
  Sanitation_Facilities = rnorm(n, mean = 60, sd = 25),# Percentage of population
  GDP_per_Capita = rnorm(n, mean = 1500, sd = 500),    # USD
  Literacy_Rate = rnorm(n, mean = 80, sd = 15),    # Percentage
  Unemployment_Rate = rnorm(n, mean = 10, sd = 5), # Percentage
  Malnutrition_Rate = rnorm(n, mean = 20, sd = 10),# Percentage
  Urbanization_Rate = rnorm(n, mean = 40, sd = 20),# Percentage
  Insecticide_Treated_Nets = rnorm(n, mean = 50, sd = 30),# Percentage of households
  Indoor_Residual_Spraying = rnorm(n, mean = 30, sd = 20),# Percentage of households
  Housing_Quality = rnorm(n, mean = 60, sd = 20),  # Quality index 0-100
  Immunization_Coverage = rnorm(n, mean = 70, sd = 20),# Percentage
  Access_to_Clinics = rnorm(n, mean = 60, sd = 25) # Percentage of population
)

# Ensure no negative values for variables that shouldn't be negative
continuous_predictors_df <- continuous_predictors_df %>%
  mutate(
    Rainfall = pmax(Rainfall, 0),
    Humidity = pmin(pmax(Humidity, 0), 100),
    Elevation = pmax(Elevation, 0),
    Population_Density = pmax(Population_Density, 0),
    Distance_to_Water = pmax(Distance_to_Water, 0),
    Travel_Time = pmax(Travel_Time, 0),
    Healthcare_Access = pmin(pmax(Healthcare_Access, 0), 100),
    Sanitation_Facilities = pmin(pmax(Sanitation_Facilities, 0), 100),
    GDP_per_Capita = pmax(GDP_per_Capita, 0),
    Literacy_Rate = pmin(pmax(Literacy_Rate, 0), 100),
    Unemployment_Rate = pmin(pmax(Unemployment_Rate, 0), 100),
    Malnutrition_Rate = pmin(pmax(Malnutrition_Rate, 0), 100),
    Urbanization_Rate = pmin(pmax(Urbanization_Rate, 0), 100),
    Insecticide_Treated_Nets = pmin(pmax(Insecticide_Treated_Nets, 0), 100),
    Indoor_Residual_Spraying = pmin(pmax(Indoor_Residual_Spraying, 0), 100),
    Housing_Quality = pmin(pmax(Housing_Quality, 0), 100),
    Immunization_Coverage = pmin(pmax(Immunization_Coverage, 0), 100),
    Access_to_Clinics = pmin(pmax(Access_to_Clinics, 0), 100)
  )

# Generate some categorical predictors with realistic proportions
set.seed(123)
categorical_predictors_df <- data.frame(
  Gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)),
  Age_Group = sample(c("Child", "Adult", "Senior"), n, replace = TRUE, prob = c(0.3, 0.6, 0.1)),
  Region = sample(c("Central Kenya", "Coastal Region", "Western Kenya", "Nyanza", "Rift valley"), n, replace = TRUE),
  Season = sample(c("Dry", "Wet"), n, replace = TRUE, prob = c(0.4, 0.6))
)

# Combine continuous and categorical predictors
predictors_df <- bind_cols(continuous_predictors_df, categorical_predictors_df)

# Create a linear predictor for each variant
# Encoding categorical variables into dummy variables
dummy_vars <- model.matrix(~ Gender + Age_Group + Region + Season - 1, data = predictors_df)

# Combining continuous and dummy variables
combined_predictors <- cbind(as.matrix(continuous_predictors_df), dummy_vars)

# Linear predictor for three classes (multinomial logit)
# Assuming a coefficient matrix for 3 classes
coefficients_matrix <- matrix(runif(ncol(combined_predictors) * 3, -1, 1), ncol = 3)

# Linear predictor
linear_predictor <- combined_predictors %*% coefficients_matrix

# Add some random noise
linear_predictor <- linear_predictor + matrix(rnorm(n * 3), ncol = 3)

# Convert linear predictor to probabilities using a more stable softmax
softmax <- function(x) {
  exp_x <- exp(x - apply(x, 1, max, na.rm = TRUE))  # Subtract max(x) for numerical stability
  exp_x / (rowSums(exp_x, na.rm = TRUE) + 1e-10)  # Add small value to the denominator
}

probabilities <- softmax(linear_predictor)

# Check for any invalid probabilities and handle them
probabilities[!is.finite(probabilities)] <- 1 / 3  # Assign equal probability if invalid

# Ensure probabilities are positive and sum to 1
probabilities <- pmax(probabilities, 0)
probabilities <- probabilities / rowSums(probabilities)

# Sample malaria variants based on the calculated probabilities
malaria_variants <- apply(probabilities, 1, function(x) sample(1:3, 1, prob = x))

# Add malaria variants to the data frame
data <- predictors_df %>%
  mutate(Malaria_Variant = factor(malaria_variants, labels = c("Plasmodium Falciparum", "Plasmodium Vivax", "Plasmodium Malariae")))

# Print the first few rows of the generated data
head(data)

# Optional: Inspect the structure of the data
str(data)
```