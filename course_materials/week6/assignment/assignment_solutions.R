
# Use the Meuse dataset. Below is the code to load it.
data(meuse, package="sp")
head(meuse)

# 1) Plot a histogram of the values of zinc. (3 pts)
hist(meuse$zinc)

# 2) What range of values fits better what we observe in the histogram: all real numbers, integers, continuous values between 0 and 1, non-negative numbers? (3 pts)
# Answer: positive numbers

# 3) Using the method caret::createFolds(), define an index to split the meuse data into 2 subsets of data (train and test) (3 pts)
ix = caret::createFolds(meuse$zinc, k = 2)
train_set <- meuse[ix$Fold1, ]
test_set <- meuse[ix$Fold2, ]

# 4) Using only the train set and fit a GLM to explain zinc with cadmium as a single covariate. Set the parameter: family = gaussian(link="log") (3 pts)
m1 <- glm(zinc ~ cadmium, data=train_set, family = gaussian(link="log"))
summary(m1)

# 5) Compute the correlogram based on Moran's coefficient for the residuals (3 pts)
correlog_m1 <- pgirmess::correlog(coords=train_set[,c("x", "y")], z=m1$residuals, method="Moran", nbclass=10)
head(correlog_m1)

plot(correlog_m1[,1], correlog_m1[,2], type = "b")

# 6) Is there any significant spatial autocorrelation in the residuals? (3 pts)
# Answer: Yes, not to strong, but there is.

# 7) Now add a spatial effect with a GAM
m2 <- mgcv::gam(zinc ~ cadmium + s(x, y) , data=train_set, family=gaussian(link="log"))
summary(m2)

# 8) Compute the correlogram based on Moran's coefficient for the residuals. Is there significant spatial autocorrelation in the residuals? (3 pts)
correlog_m2 <- pgirmess::correlog(coords=train_set[,c("x", "y")], z=m2$residuals, method="Moran", nbclass=10)
head(correlog_m2)

plot(correlog_m2[,1], correlog_m2[,2], type = "b")

# 9) Predict the values of the test set using the GLM and the GAM. (3 pts)
pred_1 <- predict.glm(m1, newdata = test_set, type = "response")
pred_2 <- mgcv::predict.gam(m2, newdata = test_set, type = "response")

plot(test_set$zinc, pred_1, col="blue", pch=16, xlim = c(0,2000), ylim = c(0,2000))
points(test_set$zinc, pred_2, col="red", pch=16)
lines(c(0,2000), c(0,2000), col="green")

# 10) Compute the mean squared error for the predicted values and find which one has the smallest generalization error. (3 pts)
mse1 <- mean((test_set$zinc - pred_1)^2)
mse2 <- mean((test_set$zinc - pred_2)^2)

print(c(mse1, mse2))
