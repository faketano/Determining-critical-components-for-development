#Libraries
library(ggplot2)
library(tidyr)
library(gtools)
library(factoextra)
library(plyr)
library(dplyr)
library(ClusterR)
library(cluster)
library(gridExtra)
library(purrr)
library(tidyverse)
library(cluster)    
library(factoextra) 
library(dendextend) 
library(BBmisc)
library(pscl)
library(caret)
library(car)
library(glmnet)
library(MASS)
library(mda)
library(klaR)
library(boot)
library(bcaboot)
library(corrplot)
library(psych)
library(reshape2)

#Import Dataset
df <-  read.csv("SUPERVISED_DF.csv", row.names = 'Country', header= TRUE)
view(df)
dim(df)
#NORMALIZATION
df <- normalize(df[,1:16], method = "range", range = c(0, 1))
view(df)
#Checking NA VALUES
colSums(is.na(df)) #No missing values
str(df)

# I want to double check what I did on unsupervised. So I'll do clasification and see what I get.
# The OPTIMUM would be to get the same countries classified as Developed and UNDEVELOPED. 

#Checking for Correlation Within the Variables
corr_mat <- round(cor(df),2)
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
        )

# We see that we have some correlations (positive and negative) and that might affect our analysis.
# We might be including redundant variables. 
# We are going to be doing regularization in order to fix it.
# Since I have WAY too many variables for less than 200 observations a model wouldn't get proper estimates. 

#Now I add the CLUSTER result (1, 0) of the UNSUPERVISED.
result_supervised <- read.csv("RESULT_SUPERVISED_ML.csv", row.names = 'Country', header= TRUE)
df$Cluster <- result_supervised$Cluster

#I'll do regulariztion. I'll try different methods and see which one fits the best and has the MOST SENSE.
#Split the dataset into  and Test 
train_size <- floor(0.75 * nrow(df))
#Set seed
set.seed(433)
train_ind <- sample(seq_len(nrow(df)), size = train_size)
train <- df[train_ind,]
test <- df[-train_ind,]
view(train)
# Dumy code categorical predictor variables
x <- model.matrix(Cluster~., train)[,-1] #Variables WITHOUT cluster result.
y <- train$Cluster # Result of clustering. 1 Developed and 0 not developed.


#PENALIZED LOGISTIC REGRESSION. ALPHA=1 LASSO . ALPHA=0 RIDGE
set.seed(433)
glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

#Let's find the optimum lamda with Cross-Validation
#FIND OPTIMUM LAMBDA
# Find the best lambda using cross-validation
set.seed(433)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
#Find minimum LAMDA
cv.lasso$lambda.min#0.01907837 is minimum lamda

# Fit the final model on the training data using minimum lambda
model_lg <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

model_lg$beta # LASSO KEPT 8 VARIABLES.
# ALL VARIABLES HAVE THE RIGHT SIGN ACCORDING TO ECONOMIC THEORY
# LASSO ELIMINATED VARIABLES ALSO BECAUSE THEY WERE IRRELEVANT OR HAD MULTICOLINEARITY BETWEEN THEM

# Make predictions on the test data
x.test <- model.matrix(Cluster ~., test)[,-1]
probabilities_lg <- model_lg %>% predict(newx = x.test)
predicted.classes_lg <- ifelse(probabilities_lg > 0.9999999, 1, 0) #Threeshold logistic at 0.5
# Model accuracy 
observed.classes <- test$Cluster
mean(predicted.classes_lg == observed.classes)#MODEL HAS ACCURACY OF 0,95
# Overfitting?
# 1)Well, we have regularized the model and we have and using
# 2) a LOGISTIC model that works very good for a binomial classification such as this.

#see predicted countries
predicted.classes_lg # 1 IS DEVELOPED, 0 is not developed
# When we observe the results they seem to be OK according to empirical knowledge.

#lasso regression - PLOT
plot(cv.lasso)  # The dashed vertical lines inidicate the value of lamdbda that minimizes the error. 
cv.lasso$lambda.min #0.01907837

#finds also the value of lambda that gives the simplest model but also lies within one standard error of the optimal value of lambda.
cv.lasso$lambda.1se #0.0530867
#Using results
# CV with MINIMUM LAMBDA 
coef(cv.lasso, cv.lasso$lambda.min) #8 coefficients + intercept
# CV with LAMBDA SIMPLEST MODEL
coef(cv.lasso, cv.lasso$lambda.1se) #5 coefficients + intercept

# Final model with lambda.1se - SIMPLEST MODEL
lasso.model_1se <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.1se)
lasso.model_1se$beta

# Make prediction on test data
x.test_1se <- model.matrix(Cluster ~., test)[,-1]
probabilities_1se <- lasso.model_1se %>% predict(newx = x.test_1se)
predicted.classes_1se <- ifelse(probabilities_1se > 0.5, 1, 0)
# Model accuracy rate SIMPLES MODEL
mean(predicted.classes_1se == observed.classes) #0.9555556
coef(cv.lasso, cv.lasso$lambda.1se)# COEFFICIENTS ARE 5
# BETTER WITH LESS VARIABLES. BEST MODEL IS THE ONE WITH ONLY 5 VARIABLES


# Compare to LOGISTIC REGRESSION with NO LASSO
# Fit the model
full.model <- glm(Cluster ~., data = train, family = binomial)
# Make predictions
probabilities_full <- full.model %>% predict(test, type = "response")
predicted.classes_full <- ifelse(probabilities_full > 0.5, 1, 0)
# Model accuracy
observed.classes <- test$Cluster
mean(predicted.classes_full == observed.classes) #0.9111111 
#Model improves when not overloaded with information.
#Better to do LASSO and regularization. 
#Always better to eliminate redundant variables


#-----------------------------------------------------------------------------
#Testing Assumptions logistic regression 

# 1) The outcome is a binary
# 2) There is a linear relationship between the logit of the outcome and each predictor variables. 
# 3) There are no extreme values or outliers in the continuous predictors
# 4) There is no high intercorrelations -multicollinearity among the predictors.


# 1) Outcome is binary. YES 1 AND 0

# 2) There is a linear relationship between the logit of the outcome and each predictor variables.
# This can be done by visually inspecting the scatter plot between each predictor and the logit values.
# It will be done with the variables that LASSO decided to leave in and highest accuracy so 
lasso.model_1se$beta #LIFE_EXP, MEAN_YEARS_SCHOOL, EASE_BUSINESS, GDP_PER_CAPITA, BROAD_BAND
df_reduced <- df[,c(1,2,9,14,16)]
df_reduced$Cluster <- result_supervised$Cluster

# TRAIN AND TEST PARTITION
set.seed(433)
train_df_reduced <- df_reduced[train_ind,]
test_df_reduced <- df_reduced[-train_ind,]
# FINAL MODEL TO CHECK
#MODEL
FINAL_MODEL <- glm(Cluster ~., data = train_df_reduced, family = binomial)
# Predict the probability (p) of diabete positivity
final_probabilities <- predict(FINAL_MODEL, type = "response")
final_predicted.classes <- ifelse(final_probabilities > 0.5, "pos", "neg")
view(final_predicted.classes)

# Select only numeric predictors
mydata <- train_df_reduced %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(final_probabilities/(1-final_probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#PLOT
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#VARIABLES ARE LINEAR, WE ALSO SEE THAT THE CLUSTER VARIABLE IS CLEARLY LOGISTIC

# 3) NO EXTREME VALUES BECAUSE NORMALIZED

# 4) Multicolinearity. 
vif(FINAL_MODEL)
max(vif(FINAL_MODEL))
# No collinearity. MAX VIF is 1.54, WAY below the threshold of 5. 

# MODEL IS VALID

#-------------
#Linear Discriminant Analysis - FULL MODEL
# LDA algorithm starts by finding directions that maximize the separation between classes.

#LDA assumes that predictors are normally distributed (Gaussian distribution) 
#and that the different classes have class-specific means and equal variance/covariance.
# Fit the model all variables
model_lda <- lda(Cluster~., data = train)
# Make predictions
predictions_lda <- model_lda %>% predict(test)
# Model accuracy
mean(predictions_lda$class==test$Cluster) #0.8888889 LDA is WORSE than logistic regression

#COEFFICIENTS
model_lda #Already divided between 1 (developed) and 0 (undeveloped)
#0.4545 being UNDEVELOPED and 0.5454 of being developed

#Making predictions
names(predictions_lda)
# Predicted classes
head(predictions_lda$class, 6)
# Predicted probabilities of class memebership according to the MODEL
head(predictions_lda$posterior, 1000)

# REDUCED MODEL
model_lda_reduced <- lda(Cluster~., data = train_df_reduced)
predictions_lda_reduced <- model_lda_reduced %>% predict(test)
# Model accuracy
mean(predictions_lda_reduced$class==test$Cluster)
#0.9111 LDA is WORSE than logistic regression, but smaller model better than greater model


#----------------------------------------------------
#QUADRATIC DISCRIMINANT ANALYSIS
#ALSO QDA but LDA tends to be a better than QDA when you have a small training set.
# Fit full QDA MODEL
model_qda <- qda(Cluster~., data = train)
model_qda #coefficients, with the 2 groups
# Make predictions
predictions_qda <- model_qda %>% predict(test)
# Model accuracy
mean(predictions_qda$class == test$Cluster) #0.9111111 accuracy

#REDUCED QDA model
model_qda_reduced <- qda(Cluster~., data = train_df_reduced)
model_qda_reduced #coefficients, with the 2 groups
# Make predictions
predictions_qda_reduced <- model_qda_reduced %>% predict(test)
# Model accuracy
mean(predictions_qda_reduced$class == test$Cluster) #.8888889 accuracy
# QDA better with all variables. 

#--------------------------------------------
# MIXTURE DISCRIMINANT ANALYSIS 
  #Used when assumtion of gaussian distribution does not hold

model_mda <- mda(Cluster~., data = train)
model_mda

# Make predictions
predicted.classes_mda <- model_mda %>% predict(test)
# Model accuracy
mean(predicted.classes_mda == test$Cluster) #0.9555556 accuracy
model_mda$values

#REDUCED MODEL
model_mda_reduced <- mda(Cluster~., data = train_df_reduced)
model_mda_reduced

# Make predictions
predicted.classes_mda_reduced <- model_mda_reduced %>% predict(test)
# Model accuracy
mean(predicted.classes_mda_reduced == test$Cluster) #0.9333333 accuracy
model_mda$values

#-----------------
#FLEXIBLE DISCRIMINANY ANALYSIS 
  # Fit the model
model_fda <- fda(Cluster~., data = train)
# Make predictions
predicted.classes_fda <- model_fda %>% predict(test)
# Model accuracy
mean(predicted.classes_fda == test$Cluster) #0.8888889 accuracy

#REDUCED MODEL
model_fda_reduced <- fda(Cluster~., data = train_df_reduced)
# Make predictions
predicted.classes_fda_reduced <- model_fda_reduced %>% predict(test)
# Model accuracy
mean(predicted.classes_fda_reduced == test$Cluster) #0.9111111 accuracy


#--------------------
#REGULARIZED DISCRIMINANT ANALYSIS
#more robust against multicollinearity in the data. Very USEFUL for LARGE MULTIVARIATE DATASET CONTAINING HIGHLY CORRELATION

# Fit the model
model_rda <- rda(Cluster~., data = train)
# Make predictions
predictions_rda <- model_rda %>% predict(test)
# Model accuracy
mean(predictions_rda$class == test$Cluster) #0.9111111 accuracy


#------
#LINEAR REGRESSION USING HDI as Y
df_linear_regression <- df[,1:16]
df_linear_regression$HDI <- result_supervised[,1]
#LINEAR MODEL

linear_model <- lm(HDI~. , data = df_linear_regression)
summary(linear_model)

# It's good to know that the model takes as SIGNIFICATIVE the variables that we use in our final LOGISTIC MODEL 
# significative LIFE_EXP, MEAN_YEARS_SCHOOL, BIRTHS_PROFESSIONALS, EASE_BUSINESS, OUT_OF_SCHOOL_PRIMARY, GDP PER CAPITA, BROAD_BAND

vif(linear_model)
# BUT WE HAVE TOO MUCH COLINEARITY. SO LET'S TAKE THE VARIABLES OUT AND RE RUN.

#WE ELIMINATE NON SIGNIFICATIVE VARIABLES - REGULARIZATION FOR LINEAR REGRESSION



linear_model1 <- lm(HDI~Life_Exp+Mean_Years_School+Births_by_Professionals+Ease_Business+Out_Of_School_Primary+GDP_Per_Capita+Broad_Band , data = df_linear_regression)
vif(linear_model1) #ELIMINATING LIFE_EXP and MEAN_YEARS_SCHOOL, BROAD_BAND

linear_model2 <- lm(HDI~Births_by_Professionals+Ease_Business+Out_Of_School_Primary+GDP_Per_Capita , data = df_linear_regression)
vif(linear_model2)
coef(linear_model2)

par(mfrow = c(2, 2))
plot(linear_model) #Good, no clear pattern
plot(linear_model, 2)

# CROSS VALIDATION AND LASSO FOR LINEAR REGRESSION
# Find the best lambda using cross-validation
 #Gotta do this because the original dataset has binary outcome as Y, this one has HDI.
#Whole dataset
set.seed(500)
train_lr = df_linear_regression %>%
  sample_frac(0.7)
test_lr= df_linear_regression %>%
  setdiff(train_lr)
#FULL DATASET AS MODEL
FULL_X = model.matrix(HDI~., df_linear_regression)[,-1]
FULL_Y = df_linear_regression %>%
  dplyr::select(HDI) %>%
  unlist() %>%
  as.numeric()
# ONLY FEATURES
x_train = model.matrix(HDI~., train_lr)[,-1]
x_test = model.matrix(HDI~., test_lr)[,-1]
# ONLY RESPONSE
y_train = train_lr %>%
  dplyr::select(HDI) %>%
  unlist() %>%
  as.numeric()
y_test = test_lr %>%
  dplyr::select(HDI) %>%
  unlist() %>%
  as.numeric()

# LASSO LINEAR
linear_lasso = glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(linear_lasso) #PLOT

# LASSO for LAMBDA FINDING
set.seed(230)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
MIN_LAMBDA_LR <-  cv.out$lambda.min # Select lamda that minimizes training MSE #0.003619595
SIM_LAMBDA_LR <- cv.out$lambda.1se # Select lambda SIMPLEST model 0.01105372


# PREDICTIONS FOR MIN LAMBDA
lasso_pred_MIN_LAMBDA = predict(linear_lasso, s = MIN_LAMBDA_LR, newx = x_test) # Use min lambda to predict test data
mean((lasso_pred_MIN_LAMBDA - y_test)^2) # Calculate test MSE = 0.0005761049

#PREDICTION FOR SIMPLEST MODEL
lasso_pred_SIMPLEST_MODEL = predict(linear_lasso, s = SIM_LAMBDA_LR, newx = x_test) # Use lambda simplest model to predict test data
mean((lasso_pred_SIMPLEST_MODEL - y_test)^2) # Calculate test MSE = 0.0007168832

# USING THE ONE WITH LOWER MSE, so SIMPLEST MODEL
FINAL_OUTPUT = glmnet(FULL_X, FULL_Y, alpha = 1, lambda = SIM_LAMBDA_LR)# Fit lasso model on full dataset
FINAL_OUTPUT$beta #8 coefficients + INTERCEPT
#LIFE EXP, MEAN YEARS SCHOOL, GDP PER CAPITA MOST IMPORTANT COMPONENTS
summary(FINAL_OUTPUT)
coef(FINAL_OUTPUT)

  