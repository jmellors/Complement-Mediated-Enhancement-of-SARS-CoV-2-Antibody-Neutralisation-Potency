# Load libraries
library(glmnet)
library(MASS)

# Load data file with binary output
MyData <- read.csv("Class_Data_3.csv")
MyData2 <- MyData[c(2:15)] # Remove ID column
MyDataScale <- as.data.frame(scale(MyData2)) # Scale data by columns
MyDataScale$Enhancement <- MyData$Enhancement # Keep Enhancement column as a binary output

# Prepare data for glmnet (model matrix and response variable)
x <- model.matrix(Enhancement ~ IgG1 + IgG2 + IgG3 + IgG4 + ADCD + Total_IgG + 
                    `X229E_S` + `Cov.1_S` + `Cov.2_RBD` + `Cov.2_N` + `HKU1_S` + `NL63_S` + `OC43_S`, 
                  data = MyDataScale)[, -1]  # Remove intercept
y <- MyDataScale$Enhancement

# Lasso model (alpha = 1)
set.seed(100)
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# View the best lambda (penalty parameter)
best_lambda <- lasso_model$lambda.min
print(best_lambda)

# Coefficients for the best lambda
coef(lasso_model, s = "lambda.min")

# Get the coefficients at the optimal lambda
lasso_coefficients <- coef(lasso_model, s = "lambda.min")
print(lasso_coefficients)
