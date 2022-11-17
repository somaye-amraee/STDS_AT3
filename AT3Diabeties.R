
obese <- read.csv("C:/Users/samra/Downloads/obese.csv", header = TRUE)


summary(obese)
str(obese)

obese$result <- factor(obese$result)

for (i in 2:6) {
  obese <- obese[-which(obese[, i] == 0), ]
}

for (i in 1:8) {
  obese[i] <- scale(obese[i])
}



names(obese) <- tolower(names(obese))

e=dim(obese)[1]
d=dim(obese)[2]
str(obese)
print(paste0("number of observations = ", e))
print(paste0("number of predictors = ", d))


x <- model.matrix(result ~ . - 1, data = obese)
y <- obese$result

library(rstanarm)
options(mc.cores = parallel::detectCores())
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(result ~ ., data = obese,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = 14124869)

library(ggplot2)
pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

round(coef(post1), 2)
round(posterior_interval(post1, prob = 0.9), 2)



