#######LOADING LIBRARIES######################
##############################################
library(tidyverse)
library(oaxaca)
library(fastDummies)
#############LOADINGDATASETS##################
cps_raw <- read.csv("cps_00005.csv.gz")
#############################################

#################################################
#DATACLEANING
##################################################

vars_to_remove <- c( "INCTOT", "INCSS", "INCWELFR", 
                     "CHSUPPAID", "INCTOT_HEAD", "INCWAGE_HEAD", "INCSS_HEAD")
#For time being removing Income variables since these values are kind of function of the outcome and would cause an almost perfect fit in an ML or linear model despite innacurate estimation of F(X)

cps_clean <- cps_raw %>% select(-all_of(vars_to_remove))

cps_clean <- cps_clean %>%
  filter(INCWAGE > 0 & INCWAGE < 9999999)
cps_clean$SEX <- factor(cps_clean$SEX, levels = c(1, 2), labels = c("Male", "Female"))


id_vars_to_drop <- c(
  "SERIAL",     # Household serial
  "CPSID",     # Household ID
  "CPSIDV",    #Longitudinal Personal ID
  "CPSIDP",     # Person ID     
  "PERNUM"      # Person number within household
)

other_vars_to_drop <- c("ASECFLAG",
"ASECWT",
"ASECWTH",
"MONTH"
)

Data_for_analysis <- cps_clean %>% select(-id_vars_to_drop)

Data_for_analysis <- Data_for_analysis %>% select(-other_vars_to_drop)

vars_to_drop_analysis_DF <- c("INCTOT_MOM2", "INCTOT_POP2", 
                              "INCWAGE_MOM2", "INCWAGE_POP2", 
                              "INCSS_MOM2",  "INCSS_POP2")
DF_analysis <- DF_analysis %>% select(-all_of(vars_to_drop_analysis_DF))


DF_analysis <- DF_analysis %>%
  mutate(
    INCWAGE_POP = ifelse(is.na(INCWAGE_POP), 0, INCWAGE_POP),
    INCWAGE_POP_MISSING = ifelse(INCWAGE_POP == 0, 1, 0),
    
    INCTOT_MOM = ifelse(is.na(INCTOT_MOM), 0 , INCTOT_MOM),
    INCTOT_MOM_MISSING = ifelse(INCTOT_MOM == 0, 1, 0),
    
    INCTOT_POP = ifelse(is.na(INCTOT_POP), 0 , INCTOT_POP),
    INCTOT_POP_MISSING = ifelse(INCTOT_POP == 0, 1, 0),
    
    INCTOT_SP = ifelse(is.na(INCTOT_SP), 0 , INCTOT_SP),
    INCTOT_SP_MISSING = ifelse(INCTOT_SP == 0, 1, 0),
    
    INCSS_MOM = ifelse(is.na(INCSS_MOM), 0 , INCSS_MOM),
    INCSS_MOM_MISSING = ifelse(INCSS_MOM == 0, 1, 0),
    
    INCSS_POP = ifelse(is.na(INCSS_POP), 0 , INCSS_POP),
    INCSS_POP_MISSING = ifelse(INCSS_POP == 0, 1, 0),
    
    INCSS_SP = ifelse(is.na(INCSS_SP), 0 , INCSS_SP),
    INCSS_SP_MISSING = ifelse(INCSS_SP == 0, 1, 0),
    
    INCWAGE_MOM = ifelse(is.na(INCWAGE_MOM), 0, INCWAGE_MOM),
    INCWAGE_MOM_MISSING = ifelse(INCWAGE_MOM == 0, 1, 0),
    
    INCWAGE_SP = ifelse(is.na(INCWAGE_SP), 0, INCWAGE_SP),
    INCWAGE_SP_MISSING = ifelse(INCWAGE_SP == 0, 1, 0),
    
    # New: Child support-related variables
    CSOUT_2 = ifelse(is.na(CSOUT_2), 0, CSOUT_2),
    CSOUT_2_MISSING = ifelse(is.na(CSOUT_2), 1, 0),
    
    CSOUT_98 = ifelse(is.na(CSOUT_98), 0, CSOUT_98),
    CSOUT_98_MISSING = ifelse(is.na(CSOUT_98), 1, 0),
    
    CSOUT_99 = ifelse(is.na(CSOUT_99), 0, CSOUT_99),
    CSOUT_99_MISSING = ifelse(is.na(CSOUT_99), 1, 0),
    
    CSLEGAL_2 = ifelse(is.na(CSLEGAL_2), 0, CSLEGAL_2),
    CSLEGAL_2_MISSING = ifelse(is.na(CSLEGAL_2), 1, 0),
    
    CSLEGAL_3 = ifelse(is.na(CSLEGAL_3), 0, CSLEGAL_3),
    CSLEGAL_3_MISSING = ifelse(is.na(CSLEGAL_3), 1, 0),
    
    CSLEGAL_4 = ifelse(is.na(CSLEGAL_4), 0, CSLEGAL_4),
    CSLEGAL_4_MISSING = ifelse(is.na(CSLEGAL_4), 1, 0),
    
    CSLEGAL_6 = ifelse(is.na(CSLEGAL_6), 0, CSLEGAL_6),
    CSLEGAL_6_MISSING = ifelse(is.na(CSLEGAL_6), 1, 0),
    
    CSLEGAL_98 = ifelse(is.na(CSLEGAL_98), 0, CSLEGAL_98),
    CSLEGAL_98_MISSING = ifelse(is.na(CSLEGAL_98), 1, 0),
    
    CSLEGAL_99 = ifelse(is.na(CSLEGAL_99), 0, CSLEGAL_99),
    CSLEGAL_99_MISSING = ifelse(is.na(CSLEGAL_99), 1, 0),
    
    CSAGREE_2 = ifelse(is.na(CSAGREE_2), 0, CSAGREE_2),
    CSAGREE_2_MISSING = ifelse(is.na(CSAGREE_2), 1, 0),
    
    CSAGREE_3 = ifelse(is.na(CSAGREE_3), 0, CSAGREE_3),
    CSAGREE_3_MISSING = ifelse(is.na(CSAGREE_3), 1, 0),
    
    CSAGREE_98 = ifelse(is.na(CSAGREE_98), 0, CSAGREE_98),
    CSAGREE_98_MISSING = ifelse(is.na(CSAGREE_98), 1, 0),
    
    CSAGREE_99 = ifelse(is.na(CSAGREE_99), 0, CSAGREE_99),
    CSAGREE_99_MISSING = ifelse(is.na(CSAGREE_99), 1, 0)
  )
###########################################################################
#VARIABLEFORMATTINGCHANGES
##########################################################################

cat_vars <- c(
  "YEAR","SEX", "RACE", "ASIAN", "EDUC", "EDUC99", "SCHLCOLL",
  "DIFFHEAR", "DIFFEYE", "DIFFREM", "DIFFPHYS", "DIFFMOB", "DIFFCARE",
  "OCCLY", "INDLY", "CLASSWLY", "PENSION", 
  "OWNERSHP", "FIRMSIZE", "PUBHOUS", "RENTSUB", "REGION",
  "MIGSTA1", "MIGRATE1", "WHYMOVE",
  "DISABWRK", "HEALTH", "QUITSICK",
  "PAIDGH", "HIMCAIDLY", "HICHAMP", "PRVTOWNLY", "MRKTYPLY",
  "VETLAST", "VET1", "VET2", "VET3", "VET4",
  "CSOUT", "CSLEGAL", "CSAGREE", "UNION",
  "PAYIFABS", "SPMPOV"
)

Data_for_analysis <- Data_for_analysis %>%
  mutate(across(all_of(cat_vars), ~ as.factor(.)))

DF_analysis <- dummy_cols(
  Data_for_analysis,
  select_columns = cat_vars,
  remove_first_dummy = TRUE,    # avoid multicollinearity
  remove_selected_columns = TRUE  # drop the original categorical columns
)

hist(DF_analysis$INCWAGE, 
       +      breaks = 50, 
       +      main = "Histogram of INCWAGE", 
       +      xlab = "Income from Wages", 
       +      col = "skyblue", 
       +      border = "white")


DF_analysis <- DF_analysis %>%
  filter(INCWAGE >= quantile(DF_analysis$INCWAGE, 0.025, na.rm = TRUE) & INCWAGE <= quantile(DF_analysis$INCWAGE, 0.975, na.rm = TRUE))

###############################################################################
#SUBSET SELECTION FOR LINEAR BLINDER OAXACA (LASSO)
##############################################################################


set.seed(1)


train_index <- sample(seq_len(nrow(DF_analysis)), size = 0.8 * nrow(DF_analysis))

train_df <- DF_analysis[train_index, ]
test_df  <- DF_analysis[-train_index, ]



# Define response variable (log of income)
y_train <- log(train_df$INCWAGE)

# Drop the outcome variable + any non-predictors
x_train <- train_df %>%
  select(-INCWAGE) %>%
  as.matrix()

lasso_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1,               # 1 = LASSO
  nfolds = 5,              # 5-fold CV
  standardize = TRUE,      # recommended unless already standardized
  family = "gaussian"      # for continuous outcomes
)

plot(lasso_cv)  # CV error vs. log(lambda)

# Optimal lambda (1SE and min)
lasso_cv$lambda.min      # lambda with minimum CV error
lasso_cv$lambda.1se      # more regularized (sparser model)

x_test <- test_df %>%
  select(-INCWAGE) %>%
  as.matrix()

y_test <- log(test_df$INCWAGE)

y_test_acc <- test_df$INCWAGE

pred_lasso <- predict(lasso_cv, newx = x_test, s = "lambda.min")


# Evaluate performance
MSE_lasso <- mean((exp(pred_lasso) - y_test_acc)^2)  # Mean squared error
RMSE_Lasso <- sqrt(MSE)
plot(pred_lasso, y_test_acc)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

lasso_coef <- coef(lasso_cv, s = "lambda.min")

lasso_coef_df <- as.matrix(lasso_coef) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  rename(Coefficient = s1) %>%  # s1 is the name glmnet gives
  filter(Coefficient != 0)


paste("log_INCWAGE ~", paste(selected_top_vars, collapse = " + "))

model_check_0 <- lm(formula(paste("log_INCWAGE ~", paste(selected_vars, collapse = " + ")), data = filter(DF_sampled, SEX_Female == 0))
)
summary(model_check_0)

model_check_1 <- lm(formula(paste("log_INCWAGE ~", paste(selected_vars, collapse = " + "))),
                    data = filter(DF_sampled, SEX_Female == 1))
summary(model_check_1)


na_male <- names(coef(model_check_0))[is.na(coef(model_check_0))]
na_female <- names(coef(model_check_1))[is.na(coef(model_check_1))]


all_na_vars <- union(na_male, na_female)      #Geting rid of zero var or collinear variables
selected_vars_clean <- setdiff(selected_vars, all_na_vars)

intersect(na_male, na_female)         # Predictors dropped in both
setdiff(na_male, na_female)           # Only dropped in male model
setdiff(na_female, na_male)           # Only dropped in female model

# Get p-values
pvals_0 <- summary(model_check_0)$coefficients[, 4]
pvals_1 <- summary(model_check_1)$coefficients[, 4]

# Drop intercepts
pvals_0 <- pvals_0[names(pvals_0) != "(Intercept)"]
pvals_1 <- pvals_1[names(pvals_1) != "(Intercept)"]

# Threshold for significance (e.g., p < 0.05)
sig_vars_0 <- names(pvals_0)[pvals_0 < 0.05]
sig_vars_1 <- names(pvals_1)[pvals_1 < 0.05]

# Option 1: Significant in BOTH groups
sig_vars_both <- intersect(sig_vars_0, sig_vars_1)

# Option 2: Significant in EITHER group
sig_vars_either <- union(sig_vars_0, sig_vars_1)

all_vars <- union(names(pvals_0), names(pvals_1))

avg_pvals <- sapply(all_vars, function(v) {
  p0 <- ifelse(v %in% names(pvals_0), pvals_0[v], NA)
  p1 <- ifelse(v %in% names(pvals_1), pvals_1[v], NA)
  mean(c(p0, p1), na.rm = TRUE)
})

sorted_vars <- names(sort(avg_pvals, decreasing = FALSE))

# Choose top N variables
top_n <- 50   # You can adjust this to 100 or 30 etc
selected_top_vars <- sorted_vars[1:top_n]

################################################################################
#LINEAR BLINDER OAXACA
###############################################################################
# Extract non-zero coefficients at lambda.min
selected_vars<- lasso_coef_df$Variable
selected_vars <- setdiff(selected_vars, "(Intercept)")
selected_vars <- setdiff(selected_vars, "SEX_Female")
set.seed(1)
# Use full data (not just train/test) for decomposition
DF_selected <- DF_analysis %>%
  select(all_of(c("INCWAGE", "SEX_Female", selected_vars))) %>%
  mutate(log_INCWAGE = log(INCWAGE)) %>%
  filter(!is.na(log_INCWAGE))

DF_analysis_1 <- DF_analysis %>% 
  mutate(log_INCWAGE = log(INCWAGE)) %>%
  filter(!is.na(log_INCWAGE))


DF_selected <- DF_selected %>% select(-INCWAGE) 

DF_sampled <- DF_selected %>% dplyr::sample_frac(0.1) 


# Create group variable: numeric 0/1
DF_sampled$SEX_Female <- ifelse(DF_sampled$SEX == "Female", 1, 0)
selected_vars <- setdiff(selected_vars, "SEX_Female")
selected_vars_clean <- setdiff(selected_vars_clean, zero_var_cols)
sorted_vars_clean <- setdiff(selected_top_vars, zero_var_cols)
selected_top_vars <- setdiff(selected_top_vars, "OCCLY_7210")
form <- as.formula(paste("log_INCWAGE ~", paste(selected_top_vars, collapse = " + "), "| SEX_Female"))


oaxaca_result <- oaxaca(
 form,
 data = DF_sampled,
 R = 30,
 group.weights = 1
 
)


summary(oaxaca_result)

########################################################################
#VISUALISATION
########################################################################
avg_wage_by_year_sex <- cps_clean %>%
  group_by(YEAR, SEX) %>%
  summarise(avg_wage = mean(INCWAGE, na.rm = TRUE)) %>%
  ungroup()



ggplot(avg_wage_by_year_sex, aes(x = YEAR, y = avg_wage, color = SEX, group = SEX)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Average Annual Wage by Gender (2020–2024)",
    x = "Year",
    y = "Average Wage",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Female" = "#E91E63", "Male" = "#2196F3"))


####################################################################################
#LINEAR MODEL TRAIN AND TEST MALE AND FEMALE
####################################################################################
DF_male <- DF_analysis %>%
  filter(SEX_Female == 0) %>%
  select(selected_top_vars, INCWAGE)


set.seed(1)


train_index <- sample(seq_len(nrow(DF_male)), size = 0.8 * nrow(DF_male))





train_df_m <- DF_male[train_index, ]
test_df_m  <- DF_male[-train_index, ]

train_df_m <- train_df_m %>% select(-INCWAGE)

x_test <- test_df_m %>%
  select(-INCWAGE) %>%
  as.matrix()


y_train <- log(train_df_m$INCWAGE)
y_test <- log(test_df_m$INCWAGE)

Male_linM <- lm(log(INCWAGE) ~ ., data = train_df_m)

LIN_pred <- predict(Male_linM, as.data.frame(x_test) )



# RMSE
rmse <- sqrt(mean((LIN_pred - y_test)^2))

# R-squared
SSE <- sum((LIN_pred - y_test)^2)
SST <- sum((y_test - mean(y_test))^2)
r_squared <- 1 - (SSE/SST)

rmse
rmse_train_m
r_squared

rmse_train_m <- sqrt(mean((Male_linM$fitted.values - y_train_m)^2))

##FemALE

train_f_df_lin <- train_f_df %>% select(selected_top_vars, y_f)

Female_linM <- lm(y_f ~ ., data = train_f_df_lin)

summary(Female_linM)

y_test_f <- test_f_df$y_f 

x_test_f <- test_f_df %>% select(-y_f)

LIN_pred_fem <- predict(Female_linM, x_test_f)

rmse_f <- sqrt(mean((LIN_pred_fem - y_test_f)^2))

rmse_train_f <- sqrt(mean((Female_linM$fitted.values - y_train_f)^2))

rmse_f
rmse_train_f
r_squared_f
# R-squared
SSE_f <- sum((LIN_pred_fem - y_test_f)^2)
SST_f <- sum((y_test_f - mean(y_test_f))^2)
r_squared_f <- 1 - (SSE_f/SST_f)
####################################################################################
#GLM MODEL TRAIN AND TEST MALE
######################################################################################
DF_male_GLM <- DF_analysis %>%
  filter(SEX_Female == 0) 



set.seed(1)


train_index <- sample(seq_len(nrow(DF_male_GLM)), size = 0.8 * nrow(DF_male_GLM))





train_df_m <- DF_male_GLM[train_index, ]
test_df_m  <- DF_male_GLM[-train_index, ]

train_df_m <- train_df_m %>% select(-INCWAGE)

x_test_m <- test_df_m %>%
  select(-INCWAGE) %>%
  as.matrix()

x_train_m <- train_df_m %>%
  select(-INCWAGE) %>%
  as.matrix()

y_train_m <- log(train_df_m$INCWAGE)
y_test_m <- log(test_df_m$INCWAGE)



lasso_cv <- cv.glmnet(
  x = x_train_m,
  y = y_train_m,
  alpha = 1,               # 1 = LASSO
  nfolds = 5,              # 5-fold CV
  standardize = TRUE,      # recommended unless already standardized
  family = "gaussian"      # for continuous outcomes
)

plot(lasso_cv)  # CV error vs. log(lambda)

# Optimal lambda (1SE and min)
lasso_cv$lambda.min      # lambda with minimum CV error
lasso_cv$lambda.1se      # more regularized (sparser model)



pred_lasso <- predict(lasso_cv, newx = x_test_m, s = "lambda.min")


# Evaluate performance
MSE_lasso_m <- mean((pred_lasso - y_test_m)^2)  # Mean squared error
RMSE_Lasso_m<- sqrt(MSE_lasso_m)

# R-squared
SSE_Lasso <- sum(( pred_lasso - y_test_m)^2)
SST <- sum((y_test_m - mean(y_test_m))^2)
r_squared_Lasso <- 1 - (SSE_Lasso/SST)


ridge_cv_m <- cv.glmnet(
  x = x_train_m,
  y = y_train_m,
  alpha = 0,               # 1 = LASSO
  nfolds = 5,              # 5-fold CV
  standardize = TRUE,      # recommended unless already standardized
  family = "gaussian"      # for continuous outcomes
)

plot(ridge_cv_m)  # CV error vs. log(lambda)

# Optimal lambda (1SE and min)
ridge_cv_m$lambda.min      # lambda with minimum CV error
ridge_cv_m$lambda.1se      # more regularized (sparser model)



pred_rige <- predict(ridge_cv_m, newx = x_test_m, s = "lambda.min")


# Evaluate performance
MSE_ridge_m <- mean((pred_rige - y_test_m)^2)  # Mean squared error
RMSE_ridge_m<- sqrt(MSE_ridge_m)

SSE_ridge <- sum(( pred_rige - y_test_m)^2)
SST <- sum((y_test_m - mean(y_test_m))^2)
r_squared_ridge <- 1 - (SSE_ridge/SST)






#############################################################################################
#GLM MODEL FEMALE TRAIN AND TEST
###########################################################################################

y_train_f <- train_f_df$y_f
x_train_f <- train_f_df %>% 
  select(-y_f) %>%
  as.matrix()
            
x_test_f <- x_test_f %>% as.matrix() 

ridge_cv_f <- cv.glmnet(
  x = x_train_f,
  y = y_train_f,
  alpha = 0,               # 1 = LASSO
  nfolds = 5,              # 5-fold CV
  standardize = TRUE,      # recommended unless already standardized
  family = "gaussian"      # for continuous outcomes
)


plot(ridge_cv_f)

pred_rige_f <- predict(ridge_cv_f, newx = x_test_f, s = "lambda.min" )

RMSE_ridge_f <- sqrt(mean((pred_rige_f - y_test_f)^2))



SSE_ridge_f <- sum(( pred_rige_f - y_test_f)^2)
r_squared_ridge_f <- 1 - (SSE_ridge_f/SST_f)


Lasso_cv_f <- cv.glmnet(
  x = x_train_f,
  y = y_train_f,
  alpha = 1,               # 1 = LASSO
  nfolds = 5,              # 5-fold CV
  standardize = TRUE,      # recommended unless already standardized
  family = "gaussian"      # for continuous outcomes
)

plot(Lasso_cv_f)

pred_Lasso_f <- predict(Lasso_cv_f, newx = x_test_f, s = "lambda.min")

RMSE_Lasso_f <- sqrt(mean((pred_Lasso_f - y_test_f)^2))



SSE_Lasso_f <- sum(( pred_Lasso_f - y_test_f)^2)
r_squared_Lasso_f <- 1 - (SSE_Lasso_f/SST_f)

