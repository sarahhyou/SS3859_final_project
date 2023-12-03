sample
lmer <- lm(Rented.Bike.Count ~ . + .^3, data = sample)
check_model_assumption_graphs(lmer)
summary(lmer)$coefficients[, "Pr(>|t|)"][summary(lmer)$coefficients[, "Pr(>|t|)"] < 0.05]
