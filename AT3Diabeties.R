
Diabete <- read.csv("C:/Users/samra/Downloads/Diabete.csv", header = TRUE)


summary(Diabete)
str(Diabete)

Diabete$result <- factor(Diabete$result)

for (i in 2:6) {
  Diabete <- Diabete[-which(Diabete[, i] == 0), ]
}

for (i in 1:8) {
  Diabete[i] <- scale(Diabete[i])
}



names(Diabete) <- tolower(names(Diabete))

e=dim(Diabete)[1]
d=dim(Diabete)[2]
str(Diabete)
print(paste0("number of observations = ", e))
print(paste0("number of predictors = ", d))


x <- model.matrix(result ~ . - 1, data = Diabete)
y <- Diabete$result

library(rstanarm)
options(mc.cores = parallel::detectCores())
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(result ~ ., data = Diabete,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = 14124869)

library(ggplot2)
pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

round(coef(post1), 2)
round(posterior_interval(post1, prob = 0.9), 2)



