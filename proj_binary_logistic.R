table_race <- table(df$race, df$readmitted)
print(table_race)
prop.table(table_race, margin = 2)
prop.table(table_race, margin = 1)
chisq.test(table_race)
table_gender <- table(df$gender, df$readmitted)
print(table_gender)
prop.table(table_gender, margin = 1)
chisq.test(table_gender)
table_age <- table(df$age, df$readmitted)
prop.table(table_age, margin = 1)
chisq.test(table_age)
table_weight <- table(df$weight, df$readmitted)
prop.table(table_weight, margin = 1)
chisq.test(table_weight)

met <- table(df$metformin, df$readmitted)
prop.table(met, margin = 1)
chisq.test(met)

rep <- table(df$repaglinide, df$readmitted)
prop.table(rep, margin = 1)
chisq.test(rep)

nat <- table(df$nateglinide, df$readmitted)
prop.table(nat, margin = 1)
chisq.test(nat)

chl <- table(df$chlorpropamide, df$readmitted)
prop.table(chl, margin = 1)
chisq.test(chl)

gli <- table(df$glimepiride, df$readmitted)
prop.table(gli, margin = 1)
chisq.test(gli)

ace <- table(df$acetohexamide, df$readmitted)
prop.table(ace, margin = 1)
chisq.test(ace)

glip <- table(df$glipizide, df$readmitted)
prop.table(glip, margin = 1)
chisq.test(glip)

gly <- table(df$glyburide, df$readmitted)
prop.table(gly, margin = 1)
chisq.test(gly)

tol <- table(df$tolbutamide, df$readmitted)
prop.table(tol, margin = 1)
chisq.test(tol)

pio <- table(df$pioglitazone, df$readmitted)
prop.table(pio, margin = 1)
chisq.test(pio)

ros <- table(df$rosiglitazone, df$readmitted)
prop.table(ros, margin = 1)
chisq.test(ros)

aca <- table(df$acarbose, df$readmitted)
prop.table(aca, margin = 1)
chisq.test(aca)

mig <- table(df$miglitol, df$readmitted)
prop.table(mig, margin = 1)
chisq.test(mig)

trog <- table(df$troglitazone, df$readmitted)
prop.table(trog, margin = 1)
chisq.test(trog)

tola <- table(df$tolazamide, df$readmitted)
prop.table(tola, margin = 1)
chisq.test(tola)

exam <- table(df$examide, df$readmitted)
prop.table(exam, margin = 1)
chisq.test(exam)

cit <- table(df$citoglipton, df$readmitted)
prop.table(cit, margin = 1)
chisq.test(cit)

ins <- table(df$insulin, df$readmitted)
prop.table(ins, margin = 1)
chisq.test(ins)

boxplot(num_medications ~ readmitted, data = df,
                  col = c("pink", "skyblue", "steelblue"),
                  main = "Boxplot of Number of Medications by Readmission Status",
                  xlab = "Readmitted",
                  ylab = "Number of Medications")

ggplot(df, aes(x = re_admitted, y = time_in_hospital, fill = re_admitted)) +
  geom_boxplot() +
  labs(title = "Time in Hospital by Readmission Status", x = "Readmitted", y = "Time in Hospital") +
  theme_minimal()

table_readmit <- table(df$readmitted)
print(table_readmit)

pie(table_readmit, 
    labels = paste(names(table_readmit), "(", table_readmit, ")", sep=""), 
    main = "Readmission Distribution", 
    col = rainbow(length(table_readmit)))

boxplot(time_in_hospital ~ readmitted, data = df,
        col = c("pink", "skyblue", "steelblue"),
        main = "Boxplot of Time in Hospital by Readmission Status",
        xlab = "Readmitted",
        ylab = "Time in Hospital")

boxplot(num_lab_procedures ~ readmitted, data = df,
        col = c("pink", "skyblue", "steelblue"),
        main = "Boxplot of Number of Lab Procedures by Readmission Status",
        xlab = "Readmitted",
        ylab = "Number of Lab Procedures")

boxplot(num_procedures ~ readmitted, data = df,
        col = c("pink", "skyblue", "steelblue"),
        main = "Boxplot of Number of Procedures by Readmission Status",
        xlab = "Readmitted",
        ylab = "Number of Procedures")

ggplot(df2, aes(x = weight)) + geom_histogram(bins = 15, fill = "purple")
ggplot(df, aes(x = weight)) +
  geom_density(fill = "steelblue") +
  labs(title = "Density Plot of Weights", x = "Weights Classes", y = "Density") +
  theme_minimal()

discretize_ifelse <- function(x) {
       if (x >= 140 & x < 240) {
             return(2)
         } else if (x >= 250 & x < 251) {
               return(6)
           } else if (x >= 390 & x < 460) {
                 return(9)
             } else if (x >= 460 & x < 520) {
                   return(8)
               } else if (x >= 520 & x < 580) {
                     return(7)
                 } else if (x >= 580 & x < 630) {
                       return(3)
                   } else if (x >= 710 & x < 740) {
                         return(4)
                     } else if (x >= 800 & x < 1000) {
                           return(5)
                       } else if (x >= 785 & x < 786) {
                             return(9)
                         } else if (x >= 786 & x <= 787) {
                               return(8)
                           } else if (x >= 787 & x <= 788) {
                                 return(7)
                             } else if (x >= 788 & x <= 789) {
                                   return(3)
                               } else {
                                     return(1)
                                 }
   }
discretize_vectorized <- function(x) {
       sapply(x, discretize_ifelse)
   }
df2$diag_3 <- discretize_vectorized(df2$diag_3)
df$diag_3 <- df2$diag_3
diag <- table(df$diag_3, df$readmitted)
print(diag)
prop.table(diag, margin = 1)
chisq.test(diag)

aic <- table(df$A1Cresult, df$readmitted)
prop.table(aic, margin = 1)
chisq.test(aic)
table(df$A1Cresult)

prop.table(table(df$time_in_hospital, df$readmitted), margin = 1)
table(df$time_in_hospital)
chisq.test(table(df$time_in_hospital, df$readmitted))

numlab <- function(x) {if (x >= 0 & x < 22) {
                    return(1)
                } else if (x >= 22 & x < 43) {
                          return(2)
                      } else if (x >= 43 & x < 64) {
                                return(3)
                            } else if (x >= 64 & x < 85) {
                                      return(4)
                                  } else if (x >= 85 & x < 106) {
                                            return(5)
                                    }
   }
numlab_vect <- function(x) {
            sapply(x, numlab)
        }
df$numlabproc_class <- numlab_vect(df$num_lab_procedures)
chisq.test(table(df$numlabproc_class, df$readmitted))
table(df$numlabproc_class, df$readmitted)

prop.table(table(df$num_procedures, df$readmitted), margin = 1)
table(df$num_procedures)
chisq.test(table(df$num_procedures, df$readmitted))

nummedics <- function(x) {if (x >= 0 & x < 15) {
                         return(1)
                     } else if (x >= 15 & x < 30) {
                                   return(2)
                               } else if (x >= 30 & x < 45) {
                                             return(3)
                                         } else if (x >= 45 & x < 60) {
                                                       return(4)
                                           }
}
nummed_vect <- function(x) {
                 sapply(x, nummedics)
             }
df$num_medics_class <- nummed_vect(df$num_medications)
chisq.test(df$num_medics_class, df$readmitted)
prop.table(table(df$num_medics_class, df$readmitted), margin = 1)
table(df$num_medics_class)

df_bin <- data.frame(df$age)
df_bin$weight <- df$weight
df_bin$admission_type_id <- df$admission_type_id
df_bin$discharge_disposition_id <- df$discharge_disposition_id
df_bin$admission_source_id <- df$admission_source_id
df_bin$time_in_hospital <- df$time_in_hospital
df_bin$medical_speciality <- df$medical_specialty
df_bin$num_lab_procedures <- df$num_lab_procedures
df_bin$num_medications <- df$num_medications
df_bin$num_outpatient <- df$number_outpatient
df_bin$num_emergency <- df$number_emergency
df_bin$num_inpatient <- df$number_inpatient
df_bin$addn_sec_diag <- df$diag_3
df_bin$metformin <- df$metformin
df_bin$pioglitazone <- df$pioglitazone
df_bin$insulin <- df$insulin
df_bin$change <- df$change
df_bin$diabetes_med <- df$diabetesMed
df_bin$readmitted <- df$re_admitted

categorical_vars <- c("age", "weight","admission_type_id", 
                                            "discharge_disposition_id", "admission_source_id", 
                                           "num_outpatient","num_emergency", "num_inpatient", "addn_sec_diag","change", "diabetes_med", "medical_speciality", "metformin", "pioglitazone", "insulin", "readmitted")

df_bin[categorical_vars] <- lapply(df_bin[categorical_vars], as.factor)
str(df_bin)

set.seed(123)
trainIndex <- createDataPartition(df_bin$readmitted, p = 0.7, list = FALSE)
train <- df_bin[trainIndex, ]
test <- df_bin[-trainIndex, ]

bin_model <- glm(readmitted ~ ., data = train, family = binomial)
library(car)
vif(bin_model)
library(ResourceSelection)
hoslem.test(train$readmitted, fitted(bin_model))

x <- model.matrix(readmitted ~ . -1, data = train)
y <- as.numeric(train$readmitted) - 1
y

x_test  <- model.matrix(readmitted ~ . -1, data = test)
y_test  <- as.numeric(test$readmitted) - 1

set.seed(123)
model_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(model_lasso)
best_lambda <- model_lasso$lambda.min
cat("Best Lambda:", best_lambda, "\n")
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)
coef(lasso_model)

lasso_prob <- predict(lasso_model, s = best_lambda, newx = x_test, type = "response")
lasso_pred <- ifelse(lasso_prob > 0.5, 1, 0)
conf_matrix <- confusionMatrix(factor(lasso_pred), factor(y_test), positive = "1")
print(conf_matrix)

library(pROC)
roc_obj <- roc(y_test, as.numeric(lasso_prob))
plot(roc_obj, main = "ROC Curve for LASSO Logistic Regression", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

auc_val <- auc(roc_obj)
cat("AUC:", auc_val, "\n")

library(randomForest)
set.seed(12)
rf_model <- randomForest(readmitted ~ ., data = train, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, test)
confusionMatrix(rf_pred, test$readmitted)
varImpPlot(rf_model)

#PCA
numeric_data <- model.matrix(readmitted ~ . -1, data = df_bin)
pca_result <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
summary(pca_result)
plot(pca_result, type = "l", main = "Scree Plot of PCA")
biplot(pca_result, scale = 0)
var_explained <- summary(pca_result)$importance[2, ]  # Proportion of Variance
cum_var_explained <- summary(pca_result)$importance[3, ]
which(cum_var_explained >= 0.70)[1]
cat("Selected Components: PC1 to PC", which(cum_var_explained >= 0.70)[1], "\n")
loadings <- pca_result$rotation
loadings[, 1] 
sort(abs(loadings[, 1]), decreasing = TRUE)
print(names(sort(abs(loadings[, 1]), decreasing = TRUE)))[1:15]

#Gradient Boosting
pc_data <- as.data.frame(pca_result$x[, 1:44])
pc_data$readmitted <- df_bin$readmitted
library(xgboost)
library(gbm)
set.seed(123)
gbm_model <- gbm(readmitted ~ ., 
                                     data = pc_data,
                                     distribution = "bernoulli",  # for binary outcome
                                     n.trees = 200,
                                     interaction.depth = 3,
                                     shrinkage = 0.1,
                                     cv.folds = 5,
                                     n.minobsinnode = 10,
                                     verbose = FALSE)
best_iter <- gbm.perf(gbm_model, method = "cv")
pred_probs <- predict(gbm_model, newdata = pc_data, n.trees = best_iter, type = "response")
library(pROC)
roc_curve <- roc(pc_data$readmitted, pred_probs)
auc(roc_curve)
plot(roc_curve, col = "blue")
xgb_pred <- ifelse(pred_probs > 0.5, 1, 0)
confusionMatrix(factor(xgb_pred), factor(df_bin$readmitted))

# Install if not already installed
install.packages("nnet")
install.packages("dplyr")

library(nnet)
library(dplyr)

df_multi <- data.frame(df$age)
df_multi$weight <- df$weight
df_multi$admission_type_id <- df$admission_type_id
df_multi$discharge_disposition_id <- df$discharge_disposition_id
df_multi$admission_source_id <- df$admission_source_id
df_multi$time_in_hospital <- df$time_in_hospital
df_multi$medical_speciality <- df$medical_specialty
df_multi$num_lab_procedures <- df$num_lab_procedures
df_multi$num_medications <- df$num_medications
df_multi$num_outpatient <- df$number_outpatient
df_multi$num_emergency <- df$number_emergency
df_multi$num_inpatient <- df$number_inpatient
df_multi$addn_sec_diag <- df$diag_3
df_multi$metformin <- df$metformin
df_multi$pioglitazone <- df$pioglitazone
df_multi$insulin <- df$insulin
df_multi$change <- df$change
df_multi$diabetes_med <- df$diabetesMed
df_multi$readmitted <- df$readmitted

str(df_multi)
df_multi[categorical_vars] <- lapply(df_multi[categorical_vars], as.factor)
str(df_multi)

multi <- multinom(readmitted ~ ., data = df_multi)
summary(multi)

linear_model <- lm(as.numeric(readmitted) ~ ., data = df_multi)
vif(linear_model)
alias(linear_model)

design_matrix <- model.matrix(~ ., data = df_multi)
qr_matrix <- qr(design_matrix)
qr_matrix$rank

#HLTest <- GTest(multi$fitted.values, df_multi$readmitted)
#print(HLTest)

set.seed(123)
trainIndex <- createDataPartition(df_multi$readmitted, p = 0.7, list = FALSE)
trainData <- df_multi[trainIndex, ]
testData  <- df_multi[-trainIndex, ]

model_multinom <- multinom(readmitted ~ ., data = trainData)
pred_class <- predict(model_multinom, newdata = testData)
mean(pred_class == testData$readmitted)

#ROC and AUC
prob <- predict(model_multinom, newdata = testData, type = "prob")
test_labels <- model.matrix(~ readmitted - 1, data = testData)
auc_results <- sapply(1:ncol(prob), function(i) {
  roc_obj <- roc(test_labels[, i], prob[, i])
  auc(roc_obj)
})
print(auc_results)

#PCA
pca_features <- trainData %>% select(-readmitted)
pca_numeric <- model.matrix(~ . - 1, data = pca_features)
pca_scaled <- scale(pca_numeric)
pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_scaled_df <- as.data.frame(pca_result$x)
#
test_x <- testData %>% select(-readmitted)
test_numeric <- model.matrix(~ . -1, data=test_x)
test_clean <- test_numeric[, nzv_cols]
test_scaled <- scale(test_clean,
                     center = attr(pca_scaled, "scaled:center"),
                     scale  = attr(pca_scaled, "scaled:scale"))


pca_train_df <- as.data.frame(pca_result$x[, 1:41])
pca_test_df  <- as.data.frame(pca_test[, 1:41])

pca_train_df$readmitted <- trainData$readmitted
pca_test_df$readmitted  <- testData$readmitted

nzv_cols <- apply(pca_numeric, 2, function(x) length(unique(x)) > 1)
pca_clean <- pca_numeric[, nzv_cols]
# Step 2: Scale
pca_scaled <- scale(pca_clean)
 
# Step 3: PCA
pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)

fviz_eig(pca_result)

# A matrix of variables × PCs
loadings <- pca_result$rotation  
head(loadings)
top_PC1 <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:10]
print(top_PC1)

pc8 <- sort(abs(loadings[, 8]), decreasing = TRUE)[1:10]
print(pc8)

#Gradient Boosting
pca_train <- as.data.frame(pca_result$x[, 1:41])
pca_train$readmitted <- trainData$readmitted

set.seed(123)
gbm_model <- gbm(readmitted ~ ., data = pca_train_df,
                 distribution = "multinomial",
                 n.trees = 150, interaction.depth = 3, shrinkage = 0.1,
                 cv.folds = 5, n.cores = NULL, verbose = FALSE)
# Predict and accuracy
gbm_pred <- predict(gbm_model, newdata = pca_test_df, n.trees = gbm_model$n.trees, type = "response")
pca_test <- predict(pca_result, newdata = test_scaled)

gbm_class <- colnames(gbm_pred)[apply(gbm_pred, 1, which.max)]
mean(gbm_class == pca_test_df$readmitted)

#ANOVA
df %>% group_by(readmitted) %>% shapiro_test(num_medications)

# Check variance homogeneity
leveneTest(num_medications ~ readmitted, data = df)

# If assumptions hold, do ANOVA
anova_res <- aov(num_medications ~ readmitted, data = df)
summary(anova_res)
TukeyHSD(anova_res)

df %>% group_by(readmitted) %>% shapiro_test(time_in_hospital)

leveneTest(time_in_hospital ~ readmitted, data = df)

kruskal.test(time_in_hospital ~ readmitted, data = df)
dunnTest(time_in_hospital ~ readmitted, data = df, method = "bonferroni")

df %>% group_by(readmitted) %>% shapiro_test(num_lab_procedures)

leveneTest(num_lab_procedures ~ readmitted, data = df)

kruskal.test(num_lab_procedures ~ readmitted, data = df)
dunnTest(num_lab_procedures ~ readmitted, data = df, method = "bonferroni")

df %>% group_by(readmitted) %>% shapiro_test(number_inpatient)

leveneTest(number_inpatient ~ readmitted, data = df)

kruskal.test(number_inpatient ~ readmitted, data = df)
dunnTest(number_inpatient ~ readmitted, data = df, method = "bonferroni")

df %>% group_by(readmitted) %>% shapiro_test(num_procedures)

leveneTest(num_procedures ~ readmitted, data = df)

kruskal.test(num_procedures ~ readmitted, data = df)
dunnTest(num_procedures ~ readmitted, data = df, method = "bonferroni")


# View summary
summary(log_model)

# Extract coefficients
coef_table <- summary(log_model)$coefficients

# Calculate odds ratios
odds_ratios <- exp(coef(log_model))

# Calculate 95% confidence intervals for odds ratios
confint_odds <- exp(confint(log_model))

# Combine into a table
or_table <- cbind(Estimate = coef_table[, "Estimate"],
                  OR = odds_ratios,
                  `2.5 %` = confint_odds[, 1],
                  `97.5 %` = confint_odds[, 2])

# View table
print(or_table)
