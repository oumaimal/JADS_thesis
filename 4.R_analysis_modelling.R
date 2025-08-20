### Install packages
install.packages("sandwich")
install.packages("lmtest")
install.packages("quantreg")
install.packages("Matrix")
install.packages("tidyr")


library(sandwich)
library(lmtest)
library(boot)
library(MASS)
library(Matrix)
library(quantreg)

getwd()
setwd("C:/Users/20193694/OneDrive - TU Eindhoven/Documenten/Thesis/YT_code")

yt_data <- read.csv("yt_preprocessed.csv", header = TRUE)


# converting columns to be categorical, that need to be categorical
yt_data$publish_year <- factor(yt_data$publish_year, 
                               levels = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024))
yt_data$publish_month <- factor(yt_data$publish_month, 
                                levels = c("January", "February", "March", "April", "May", "June", "July", 
                                           "August", "September", "October", "November", "December"))
yt_data$publish_day <- factor(yt_data$publish_day, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# log transform the dependent variable (view_count)
yt_data$log_view_count <- log(yt_data$view_count)
# Transform title length (parabolic relationship)
yt_data$title_length_sq <- yt_data$title_length^2
# Transform tags_count (parabolic relationship)
yt_data$tags_count_sq <- yt_data$tags_count^2





########################################### DESCRIPTIVES #######################################################
# --- Define key continuous variables ---
continuous_vars <- c("view_count", "tags_count", "title_length", "tags_originality", 
                     "title_originality", "duration_minutes", "subscriber_count", 
                     "days_since_last_upload")

# --- Get summary statistics in a loop ---
for (var in continuous_vars) {
  cat("Statistics for", var, ":\n")
  print(summary(yt_data[[var]]))
  cat("Standard Deviation:", sd(yt_data[[var]], na.rm = TRUE), "\n\n")
}




########################################### ASSUMPTION CHECKS ##########################################################################################################

### Check assumption for negative binomial: overdispersion

# --- Model Setup (ensure these variables/models are correctly defined from your previous steps) ---
# regression formula
full_formula <- " ~ tags_count + title_originality + tags_originality + duration_minutes + subscriber_count +
                  days_since_last_upload + title_length +
                  publish_year + publish_month + publish_day"

# Your fitted Negative Binomial model
negbin_model <- glm.nb(as.formula(paste("view_count", full_formula)), data = yt_data)

# --- Overdispersion Check ---

# 1. Fit a Poisson Model (for comparison)
poisson_model <- glm(as.formula(paste("view_count", full_formula)),
                     data = yt_data,
                     family = poisson(link = "log"))

# 2. Perform Likelihood Ratio Test (LRT)
# A significant p-value (< 0.05) means NegBin is better than Poisson,
# indicating significant overdispersion.
print("Likelihood Ratio Test (Poisson vs. Negative Binomial):")
lrtest(poisson_model, negbin_model)

# 3. Inspect the Negative Binomial's dispersion parameter (theta)
# Smaller theta indicates more overdispersion.
cat("\nEstimated Theta (Dispersion Parameter):", negbin_model$theta, "\n")

# 4. (Quick check) Compare mean vs. variance of your raw count data
cat("\nMean of view_count:", mean(yt_data$view_count), "\n")
cat("Variance of view_count:", var(yt_data$view_count), "\n")




############################################ Check for multicollinearity ##########################################################################################################
# Load necessary libraries (if not already loaded)
install.packages("dplyr")
install.packages("corrplot")
library(dplyr)    # For select() and pipe operator (optional but good practice)
library(corrplot) # For visualizing the correlation matrix (highly recommended)

# --- 1. Select Only Numeric Variables for the Correlation Matrix ---
# Identify all the continuous predictor variables you are using in your models
# Adjust this list if your variable names or selection criteria differ
numeric_vars <- yt_data %>%
  select(duration_minutes, subscriber_count, days_since_last_upload,
         title_originality, tags_originality,
         title_length, title_length_sq,
         tags_count, tags_count_sq) %>%
  as.matrix() # Convert to matrix for cor() function

# --- 2. Compute the Correlation Matrix ---
correlation_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# --- 3. Print the Correlation Matrix ---
print(correlation_matrix)

# --- 4. Visualize the Correlation Matrix (Recommended for easy interpretation) ---
# A common way to visualize correlations is with corrplot
corrplot(correlation_matrix,
         method = "color", # 'circle', 'square', 'ellipse', 'number', 'shade', 'color', 'pie'
         type = "upper",   # 'upper', 'lower', 'full'
         tl.col = "black", # Text label color
         tl.srt = 45,      # Text label rotation
         addCoef.col = "black", # Add coefficients to the plot
         number.cex = 0.7, # Size of the coefficients
         diag = FALSE)     # Do not show correlations on the diagonal (always 1)

# You can also use other methods like 'number' to see the exact values more clearly
# corrplot(correlation_matrix, method = "number", type = "upper", tl.col = "black", tl.srt = 45, diag = FALSE)



################################################ VIF Scores #######################################################################
# --- 1. Install and Load the 'car' Package ---
# The 'car' package (Companion to Applied Regression) contains the vif() function.
install.packages("car")
library(car)
library(dplyr) # Also loading dplyr for data manipulation

# --- 2. Create a Clean Dataset for the Model ---
# VIF calculation requires a dataset with no missing values (NA).

# Ensure your categorical variables are treated as factors
yt_data_for_vif <- yt_data %>%
  mutate(
    publish_year = as.factor(publish_year),
    publish_month = as.factor(publish_month),
    publish_day = as.factor(publish_day)
  )

# Select all variables for the model and remove rows with missing data
model_data <- yt_data_for_vif %>%
  select(
    view_count, # Your dependent variable
    duration_minutes, subscriber_count, days_since_last_upload,
    tags_originality, title_originality,
    tags_count, tags_count_sq,
    title_length, title_length_sq,
    publish_year, publish_month, publish_day # Include your categorical controls
  ) %>%
  na.omit() # Remove any rows with NA values in any of these columns

# --- 3. Fit a Linear Model (for diagnostic purposes) ---
# The vif() function needs a model object as input. We create a simple linear model (lm)
# because VIF assesses the relationships *between predictors*, not their relationship
# with the outcome variable. 

# The formula 'view_count ~ .' means "regress view_count on all other variables in the data frame".
vif_model <- lm(view_count ~ ., data = model_data)


# --- 4. Calculate and Print VIF Scores ---
# Now, run the vif() function on your fitted model
vif_scores <- vif(vif_model)

# Print the results
print(vif_scores)

# For categorical variables with more than two levels, the 'car' package computes a
# Generalized VIF (GVIF). The output will show you these values automatically. To make
# them comparable to regular VIFs, you should look at the GVIF^(1/(2*Df)) column,
# which is also provided in the output. The same rule of thumb (e.g., < 5) applies.






################################################# MODELS ################################################################################################################################################################################################

### Negative binomial regression - Model 1
negbin_model1 <- glm.nb(as.formula(paste("view_count", "~ tags_originality + tags_count + tags_count_sq + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day")), 
                        data = yt_data)
summary(negbin_model1)

# Get robust standard errors for Negative Binomial
# For glm.nb, vcovHC works on the fitted model
print("\n--- Negative Binomial Model with HC3 Robust Standard Errors ---")
# regel hieronder is origineel
#robust_negbin_summary <- coeftest(negbin_model1, vcov = vcovHC(negbin_model1, type = "HC3"))
robust_negbin_summary <- coeftest(negbin_model1, vcov = vcovCL(negbin_model1, cluster = ~channel_name))
print(robust_negbin_summary)


### Negative binomial regression - Model 2
negbin_model2 <- glm.nb(as.formula(paste("view_count", "~ title_originality + title_length + title_length_sq + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day")), 
                        data = yt_data)
summary(negbin_model2)

# Get robust standard errors for Negative Binomial
# For glm.nb, vcovHC works on the fitted model
print("\n--- Negative Binomial Model with HC3 Robust Standard Errors ---")
# regel hieronder is origineel
#robust_negbin_summary <- coeftest(negbin_model2, vcov = vcovHC(negbin_model2, type = "HC3"))
robust_negbin_summary <- coeftest(negbin_model2, vcov = vcovCL(negbin_model2, cluster = ~channel_name))
print(robust_negbin_summary)



### Negative binomial regression - Model 3
negbin_model3 <- glm.nb(as.formula(paste("view_count", "~ tags_originality + tags_count + tags_count_sq + title_originality + title_length + title_length_sq + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day")), 
                        data = yt_data)
summary(negbin_model3)

# Get robust standard errors for Negative Binomial
# For glm.nb, vcovHC works on the fitted model
print("\n--- Negative Binomial Model with HC3 Robust Standard Errors ---")
# regel hieronder is origineel
#robust_negbin_summary <- coeftest(negbin_model3, vcov = vcovHC(negbin_model3, type = "HC3"))
robust_negbin_summary <- coeftest(negbin_model3, vcov = vcovCL(negbin_model3, cluster = ~channel_name))
print(robust_negbin_summary)





#################################################### MODEL EVALUATION ####################################################################################################################################################################################

# Assuming you have already loaded libraries (MASS, lmtest, sandwich)
# and your yt_data is loaded with squared terms and factors defined.

# --- Example for Model 1 (Repeat for Model 2 and Model 3) ---

# Fit Model 1 (as per your definition)
negbin_model1 <- glm.nb(as.formula(paste("view_count", "~ tags_originality + tags_count + tags_count_sq + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day")),
                        data = yt_data)

# Get the summary to easily extract the dispersion parameter
summary_model1 <- summary(negbin_model1)

# 1. Dispersion Parameter (Theta)
dispersion_param1 <- summary_model1$theta
cat("Model 1 Dispersion Parameter (Theta):", round(dispersion_param1, 3), "\n")

# 2. AIC
aic_model1 <- AIC(negbin_model1)
cat("Model 1 AIC:", round(aic_model1, 2), "\n")

# 3. BIC
bic_model1 <- BIC(negbin_model1)
cat("Model 1 BIC:", round(bic_model1, 2), "\n")

# 4. Clustered Robust Standard Errors (already calculated via coeftest)
# You don't get a single value here, but you state that you used this method.
# The previous `print(robust_negbin_summary)` output demonstrates its application.

# --- Repeat for Model 2 and Model 3 ---

# For Model 2:
negbin_model2 <- glm.nb(as.formula(paste("view_count", "~ title_originality + title_length + title_length_sq + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day")),
                        data = yt_data)
summary_model2 <- summary(negbin_model2)
dispersion_param2 <- summary_model2$theta
aic_model2 <- AIC(negbin_model2)
bic_model2 <- BIC(negbin_model2)
cat("\nModel 2 Dispersion Parameter (Theta):", round(dispersion_param2, 3), "\n")
cat("Model 2 AIC:", round(aic_model2, 2), "\n")
cat("Model 2 BIC:", round(bic_model2, 2), "\n")


# For Model 3:
negbin_model3 <- glm.nb(as.formula(paste("view_count", "~ tags_originality + tags_count + tags_count_sq + title_originality + title_length + title_length_sq + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day")),
                        data = yt_data)
summary_model3 <- summary(negbin_model3)
dispersion_param3 <- summary_model3$theta
aic_model3 <- AIC(negbin_model3)
bic_model3 <- BIC(negbin_model3)
cat("\nModel 3 Dispersion Parameter (Theta):", round(dispersion_param3, 3), "\n")
cat("Model 3 AIC:", round(aic_model3, 2), "\n")
cat("Model 3 BIC:", round(bic_model3, 2), "\n")






############################################## TEST RELATIONSHIP BETWEEN TAGS_COUNT AND VIEW_COUNT #######################################################################
library(ggplot2)
library(MASS) # For glm.nb()

# --- 1. Load Data ---
# yt_data <- read.csv("yt_preprocessed.csv", header = TRUE)

# --- 2. Data Preparation (IMPORTANT: BEFORE MODEL FITTING) ---
# Create the squared term for tags_count
yt_data$tags_count_sq <- yt_data$tags_count^2
yt_data$title_length_sq <- yt_data$title_length^2

# Convert categorical variables to factors. This is CRUCIAL.
yt_data$publish_year <- as.factor(yt_data$publish_year)
yt_data$publish_month <- as.factor(yt_data$publish_month)
yt_data$publish_day <- as.factor(yt_data$publish_day)
yt_data$channel_name <- as.factor(yt_data$channel_name)

# --- 3. Define and Fit the Negative Binomial Model ---
full_formula <- "view_count ~ tags_count + tags_count_sq + tags_originality + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day + channel_name"
negbin_model <- glm.nb(as.formula(full_formula), data = yt_data)

# Print a summary of the model results
summary(negbin_model)

# --- 4. Prepare Data for Plotting the Fitted Curve (CORRECTED) ---

# Create a prediction data frame for the 'tags_count' range
plot_data <- data.frame(tags_count = seq(min(yt_data$tags_count, na.rm = TRUE), max(yt_data$tags_count, na.rm = TRUE), length.out = 100))
plot_data$tags_count_sq <- plot_data$tags_count^2

# Get the list of predictors to hold constant
constant_vars <- setdiff(all.vars(formula(negbin_model)), c("view_count", "tags_count", "tags_count_sq"))

# This corrected loop correctly assigns the mean/mode of each specific variable.
for (var in constant_vars) {
  if (var %in% names(yt_data)) {
    if (is.numeric(yt_data[[var]])) {
      plot_data[[var]] <- mean(yt_data[[var]], na.rm = TRUE)
    } else if (is.factor(yt_data[[var]])) {
      mode_val <- names(sort(table(yt_data[[var]]), decreasing = TRUE))[1]
      plot_data[[var]] <- factor(mode_val, levels = levels(yt_data[[var]]))
    }
  }
}

# --- 5. Predict view_count ---
plot_data$predicted_view_count <- predict(negbin_model, newdata = plot_data, type = "response")

# --- 6. Plotting ---
ggplot(yt_data, aes(x = tags_count, y = view_count)) +
  geom_point(alpha = 0.1) + # Use lower alpha for dense scatter plots
  geom_line(data = plot_data, aes(y = predicted_view_count), color = "blue", size = 1) +
  labs(
    title = "Fitted Quadratic Relationship of Tags Count on View Count",
    x = "Tags Count",
    y = "Predicted View Count"
  ) +
  # Option 1: Using ylim() - removes data points above the limit
  # ylim(0, 20000000)
  
  # Option 2: Using coord_cartesian() - zooms in without removing data, preferred for showing model fit
  coord_cartesian(ylim = c(0, 20000000)) +
  theme_minimal()



# Extract coefficients for tags_count and tags_count_sq
beta1_tags_count <- coef(negbin_model)["tags_count"]
beta2_tags_count_sq <- coef(negbin_model)["tags_count_sq"]

# Calculate the optimal tags_count
optimal_tags_count <- -beta1_tags_count / (2 * beta2_tags_count_sq)

# Print the result
cat("Optimal number of tags:", round(optimal_tags_count, 2), "\n")


########################################### TEST RELATIONSHIP BETWEEN TITLE_LENGTH AND VIEW_COUNT ###############################################################
library(ggplot2)
library(MASS) # For glm.nb()

# --- 1. Load Data ---
#yt_data <- read.csv("yt_preprocessed.csv", header = TRUE)

# --- 2. Data Preparation (IMPORTANT: BEFORE MODEL FITTING) ---
# Create the squared term for title_length (instead of tags_count)
yt_data$title_length_sq <- yt_data$title_length^2
yt_data$tags_count_sq <- yt_data$tags_count^2

# Convert categorical variables to factors. This is CRUCIAL.
yt_data$publish_year <- as.factor(yt_data$publish_year)
yt_data$publish_month <- as.factor(yt_data$publish_month)
yt_data$publish_day <- as.factor(yt_data$publish_day)
yt_data$channel_name <- as.factor(yt_data$channel_name)

# --- 3. Define and Fit the Negative Binomial Model ---
# UPDATED: full_formula now includes title_length and title_length_sq
full_formula <- "view_count ~ title_length + title_length_sq + title_originality + duration_minutes + subscriber_count + days_since_last_upload + publish_year + publish_month + publish_day + channel_name"
negbin_model <- glm.nb(as.formula(full_formula), data = yt_data)

# Print a summary of the model results
summary(negbin_model)

# --- 4. Prepare Data for Plotting the Fitted Curve (CORRECTED) ---

# Create a prediction data frame for the 'title_length' range
plot_data <- data.frame(title_length = seq(min(yt_data$title_length, na.rm = TRUE), max(yt_data$title_length, na.rm = TRUE), length.out = 100))
plot_data$title_length_sq <- plot_data$title_length^2

# Get the list of predictors to hold constant
constant_vars <- setdiff(all.vars(formula(negbin_model)), c("view_count", "title_length", "title_length_sq"))

# This corrected loop correctly assigns the mean/mode of each specific variable.
for (var in constant_vars) {
  if (var %in% names(yt_data)) {
    if (is.numeric(yt_data[[var]])) {
      plot_data[[var]] <- mean(yt_data[[var]], na.rm = TRUE)
    } else if (is.factor(yt_data[[var]])) {
      mode_val <- names(sort(table(yt_data[[var]]), decreasing = TRUE))[1]
      plot_data[[var]] <- factor(mode_val, levels = levels(yt_data[[var]]))
    }
  }
}

# --- 5. Predict view_count ---
plot_data$predicted_view_count <- predict(negbin_model, newdata = plot_data, type = "response")

# --- 6. Plotting ---
# UPDATED: aes(x = title_length, y = view_count)
ggplot(yt_data, aes(x = title_length, y = view_count)) +
  geom_point(alpha = 0.1) + # Use lower alpha for dense scatter plots
  # UPDATED: aes(y = predicted_view_count) within geom_line
  geom_line(data = plot_data, aes(y = predicted_view_count), color = "blue", size = 1) +
  labs(
    title = "Fitted Quadratic Relationship of Title Length on View Count", # UPDATED Title
    x = "Title Length", # UPDATED X-axis label
    y = "Predicted View Count"
  ) +
  coord_cartesian(ylim = c(0, 20000000)) + # Adjust Y-axis limit as needed
  theme_minimal()


# --- Calculate Optimal Title Length ---
# UPDATED: Extract coefficients for title_length and title_length_sq
beta1_title_length <- coef(negbin_model)["title_length"]
beta2_title_length_sq <- coef(negbin_model)["title_length_sq"]

# Calculate the optimal title_length
optimal_title_length <- -beta1_title_length / (2 * beta2_title_length_sq)

# Print the result
cat("Optimal title length:", round(optimal_title_length, 2), "\n")











