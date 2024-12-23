# Load libraries
library(glmnet)
library(MASS)

# Load data with binary output
MyData <- read.csv("Class_Data_3.csv")
MyData2 <- MyData[c(2:15)]  # Remove ID column
MyDataScale <- as.data.frame(scale(MyData2))  # Scale data by columns
MyDataScale$Enhancement <- MyData$Enhancement  # Keep Enhancement column as binary output

# Prepare data for glmnet (model matrix and response variable)
x <- model.matrix(Enhancement ~ IgG1 + IgG2 + IgG3 + IgG4 + ADCD + Total_IgG + 
                    `X229E_S` + `Cov.1_S` + `Cov.2_RBD` + `Cov.2_N` + `HKU1_S` + `NL63_S` + `OC43_S`, 
                  data = MyDataScale)[, -1]  # Remove intercept
y <- MyDataScale$Enhancement

# Bootstrap ridge regression
bootstrap_ridge <- function(data, x, y, n_bootstrap = 1000, alpha = 0) {
  n <- nrow(data)
  coef_matrix <- matrix(NA, nrow = n_bootstrap, ncol = ncol(x))
  lambda_values <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    # Sample with replacement to create a bootstrap sample
    bootstrap_idx <- sample(1:n, replace = TRUE)
    x_boot <- x[bootstrap_idx, ]
    y_boot <- y[bootstrap_idx]
    
    # Fit ridge model to the bootstrap sample
    ridge_model_boot <- cv.glmnet(x_boot, y_boot, family = "binomial", alpha = alpha)
    
    # Store the coefficients and lambda for the best model
    coef_matrix[i, ] <- as.matrix(coef(ridge_model_boot, s = "lambda.min"))[-1]  # Exclude intercept
    lambda_values[i] <- ridge_model_boot$lambda.min
  }

# Convert coefficient matrix to a data frame
coef_df <- as.data.frame(coef_matrix)
colnames(coef_df) <- colnames(x)

# Calculate summary statistics for coefficients (mean, CI)
summary_stats <- coef_df %>%
  summarize(across(everything(), list(mean = mean, sd = sd, 
                                      lower_ci = ~ quantile(., 0.025),
                                      upper_ci = ~ quantile(., 0.975))))

return(list(coef_matrix = coef_df, lambda_values = lambda_values, summary_stats = summary_stats))
}

# Perform bootstrap ridge regression
set.seed(123)  # For reproducibility
bootstrap_results <- bootstrap_ridge(MyDataScale, x, y, n_bootstrap = 1000)

# View summary statistics for the coefficients
print(bootstrap_results$summary_stats)

bootstrap_summary <- bootstrap_results$summary_stats
