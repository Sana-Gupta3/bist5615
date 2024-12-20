install.packages("haven")
library(haven)
library(dplyr)
library(MASS)

vit_d <- data.frame(read_xpt("VID_I.XPT"))
supplement_data <- data.frame(read_xpt("DSQTOT_I.XPT"))
diet_day1 <- data.frame(read_xpt("DR1TOT_I.XPT"))
diet_day2 <- data.frame(read_xpt("DR2TOT_I.XPT"))
demographics <- data.frame(read_xpt("DEMO_I.XPT"))
nutrition <- data.frame(read_xpt("DBQ_I.XPT"))
weight <- data.frame(read_xpt("WHQ_I.XPT"))

vit_d$measurement_vitd <- ifelse(
  vit_d$LBXVD3MS > 50, "Sufficiency",
  ifelse(vit_d$LBXVD3MS >= 30 & vit_d$LBXVD3MS <= 50, "Sufficiency",
         ifelse(vit_d$LBXVD3MS >= 10 & vit_d$LBXVD3MS < 30, "Mild Deficiency",
                ifelse(vit_d$LBXVD3MS < 10, "Severe Deficiency", NA)
         )
  )
)


library(ggplot2)

ggplot(df, aes(x = measurement_vitd, fill = measurement_vitd)) +
  geom_bar() +
  labs(
    title = "Distribution of Vitamin D Levels",
    x = "Vitamin D Status",
    y = "Count"
  ) +
  scale_fill_manual(
    values = c("Sufficiency" = "darkgreen", 
               "Mild Deficiency" = "orange", 
               "Severe Deficiency" = "red")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


diet_day1$vit_d_day1 <- ifelse(diet_day1$DR1TVD > 15, "Sufficient", "Insufficient")
diet_day2$vit_d_day2 <- ifelse(diet_day2$DR2TVD > 15, "Sufficient", "Insufficient")

vitd_calcium <- data.frame(as.numeric(supplement_data$DSQTVD), as.numeric(supplement_data$DSQTCALC))
colnames(vitd_calcium) <- c("DSQTVD", "DSQTCALC")
vitd_calcium$taking_vitd <- ifelse(!is.na(vitd_calcium$DSQTVD), "Yes", "No")
vitd_calcium$taking_calcium <- ifelse(!is.na(vitd_calcium$DSQTCALC), "Yes", "No")
vitd_calcium$SEQN <- supplement_data$SEQN

demographics$race <- as.factor(demographics$RIDRETH3)
demographics$education <- ifelse(demographics$education == 5, "CollegeEducated", "NotCollegeEducated")

table(demographics$education)


weight$took_diet_meds <- ifelse(!is.na(weight$WHD080I), "Yes", "No")

weight$weight_loss_surgery <- ifelse(!is.na(weight$WHD080U), "Yes", "No")

nutrition$milk_consumption <- ifelse(nutrition$DBQ197 == 0 | nutrition$DBQ197 == 1, "NonDaily", "Daily")

data_list <- list(vit_d, diet_day1, diet_day2, vitd_calcium, demographics, weight, nutrition)

merged_df <- Reduce(function(x, y) merge(x, y, by = "SEQN"), data_list)

merged_df$measurement_vitd <- ifelse(
  merged_df$LBXVD3MS > 75, "Sufficiency",
         ifelse(merged_df$LBXVD3MS >= 40 & merged_df$LBXVD3MS < 75, "Mild Deficiency",
                ifelse(vit_d$LBXVD3MS < 40, "Severe Deficiency", NA)
         
  )
)
merged_df$education <- ifelse(merged_df$DMDEDUC2 == 5, "College Educated", "Not College Educated")

df <- merged_df[c("measurement_vitd", "vit_d_day1", "vit_d_day2", "taking_vitd", "taking_calcium", "race", "took_diet_meds", "weight_loss_surgery", "milk_consumption", "education")]



df <- df %>%
  mutate(vit_d_food_score = case_when(
    vit_d_day1 == "Sufficient" & vit_d_day2 == "Sufficient" ~ 2,
    vit_d_day1 == "Sufficient" | vit_d_day2 == "Sufficient" ~ 1,
    TRUE ~ 0
  ))

df <- na.omit(df)
write.csv(df, file = "cleaned_data.csv")

summary(df)

df <- df %>%
  mutate(across(everything(), as.factor))
###############################

library(MASS)
df$measurement_vitd <- factor(df$measurement_vitd, 
                                 levels = c("Sufficiency", "Mild Deficiency", "Severe Deficiency"),
                                 ordered = TRUE)

ordinal_model <- polr(measurement_vitd ~ vit_d_day1 + vit_d_day2 + taking_vitd + taking_calcium + race + took_diet_meds + weight_loss_surgery + milk_consumption + education, 
              data = df, 
              Hess = TRUE)
summary(ordinal_model)

coefs <- summary(ordinal_model)$coefficients
p_values <- 2 * (1 - pnorm(abs(coefs[, "Value"] / coefs[, "Std. Error"])))


summary_with_p <- cbind(coefs, "p-value" = p_values)
print(summary_with_p)

reduced_ordinal_model <- polr(measurement_vitd ~ taking_vitd + race + took_diet_meds, 
                      data = df, 
                      Hess = TRUE)
summary(reduced_ordinal_model)|>
  tidy() |> 
  kable() |> 
  kable_classic()



predict(reduced_ordinal_model, type = "probs")

############################

df <- df %>% mutate(across(everything(), droplevels))

library(brms)

df %>% summarise(across(where(is.factor), ~n_distinct(.)))


priors <- c(
  prior(normal(log(9.6), 0.1), class = "b", coef = "race4"),
  prior(normal(log(3.2), 0.1), class = "b", coef = "race2"),
  prior(normal(log(1.3), 0.2), class = "b", coef = "educationNotCollegeEducated"),
  prior(normal(log(1.6), 0.1), class = "b", coef = "milk_consumptionNonDaily"),
  prior(normal(logit(0.349), 0.5), class = "Intercept", coef = "1"),
  prior(normal(logit(0.945), 0.5), class = "Intercept", coef = "2")
)


bayesian_model <- brm(
  measurement_vitd ~ vit_d_day1 + vit_d_day2 + taking_vitd + taking_calcium + race + took_diet_meds + weight_loss_surgery + milk_consumption + education,
  family = cumulative(link = "logit"),
  data = df,
  prior = priors,
  chains = 4,
  iter = 4000,
  seed = 123
)

summary(bayesian_model)

priors2 <- c(
  prior(normal(log(9.6), 0.2), class = "b", coef = "race4"),
  prior(normal(log(3.2), 0.2), class = "b", coef = "race2")
)

reduced_bayes_model <- brm(
  measurement_vitd ~ taking_vitd + race + milk_consumption + education,
  family = cumulative(link = "logit"),
  data = df,
  prior = priors,
  chains = 4,
  iter = 4000,
  seed = 123
)

summary(reduced_bayes_model)

###########################
freq_logLik <- logLik(reduced_ordinal_model)
freq_AIC <- AIC(reduced_ordinal_model) 
freq_BIC <- BIC(reduced_ordinal_model)


bayes_logLik <- log_lik(reduced_bayes_model)
bayes_loo <- loo(reduced_bayes_model)

mean_bayes_logLik <- mean(rowSums(bayes_logLik))


########################

library(pROC)
library(brms)

pred_probs <- predict(reduced_ordinal_model, type = "probs")

roc_sufficiency <- roc(df$measurement_vitd == "Sufficiency", pred_probs[, "Sufficiency"])
roc_mild_deficiency <- roc(df$measurement_vitd == "Mild Deficiency", pred_probs[, "Mild Deficiency"])
roc_severe_deficiency <- roc(df$measurement_vitd == "Severe Deficiency", pred_probs[, "Severe Deficiency"])

auc_sufficiency <- auc(roc_sufficiency)
auc_mild_deficiency <- auc(roc_mild_deficiency)
auc_severe_deficiency <- auc(roc_severe_deficiency)

plot(roc_sufficiency, col = "blue", main = "ROC Curves (Frequentist)", lwd = 2)
plot(roc_mild_deficiency, col = "red", add = TRUE, lwd = 2)
plot(roc_severe_deficiency, col = "green", add = TRUE, lwd = 2)
legend("bottomright",
       legend = c(
         paste("Sufficiency (AUC =", round(auc_sufficiency, 3), ")"),
         paste("Mild Deficiency (AUC =", round(auc_mild_deficiency, 3), ")"),
         paste("Severe Deficiency (AUC =", round(auc_severe_deficiency, 3), ")")
       ),
       col = c("blue", "red", "green"), lty = 1, lwd = 2)


auc_sufficiency <- auc(roc_sufficiency)
auc_mild_deficiency <- auc(roc_mild_deficiency)
auc_severe_deficiency <- auc(roc_severe_deficiency)
print(paste("Frequentist AUC - Sufficiency:", auc_sufficiency))
print(paste("Frequentist AUC - Mild Deficiency:", auc_mild_deficiency))
print(paste("Frequentist AUC - Severe Deficiency:", auc_severe_deficiency))

bayesian_probs <- posterior_epred(reduced_bayes_model)

bayesian_probs_mean <- apply(bayesian_probs, c(2, 3), mean)


roc_sufficiency_bayes <- roc(df$measurement_vitd == "Sufficiency", bayesian_probs_mean[, 1])

plot(roc_sufficiency_bayes, col = "blue", main = "ROC Curve for Sufficiency (Bayesian)")
auc_sufficiency_bayes <- auc(roc_sufficiency_bayes)
print(paste("AUC for Sufficiency (Bayesian):", auc_sufficiency_bayes))

roc_mild_deficiency_bayes <- roc(df$measurement_vitd == "Mild Deficiency", bayesian_probs_mean[, 2])
roc_severe_deficiency_bayes <- roc(df$measurement_vitd == "Severe Deficiency", bayesian_probs_mean[, 3])

print(paste("AUC for Mild Deficiency (Bayesian):", auc(roc_mild_deficiency_bayes)))
print(paste("AUC for Severe Deficiency (Bayesian):", auc(roc_severe_deficiency_bayes)))

auc_sufficiency_bayes <- auc(roc_sufficiency_bayes)
auc_mild_deficiency_bayes <- auc(roc_mild_deficiency_bayes)
auc_severe_deficiency_bayes <- auc(roc_severe_deficiency_bayes)

plot(roc_sufficiency_bayes, col = "blue", main = "ROC Curves (Bayesian)", lwd = 2)
plot(roc_mild_deficiency_bayes, col = "red", add = TRUE, lwd = 2)
plot(roc_severe_deficiency_bayes, col = "green", add = TRUE, lwd = 2)
legend("bottomright",
       legend = c(
         paste("Sufficiency (AUC =", round(auc_sufficiency_bayes, 3), ")"),
         paste("Mild Deficiency (AUC =", round(auc_mild_deficiency_bayes, 3), ")"),
         paste("Severe Deficiency (AUC =", round(auc_severe_deficiency_bayes, 3), ")")
       ),
       col = c("blue", "red", "green"), lty = 1, lwd = 2)
