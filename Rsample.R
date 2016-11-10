# A comment: this is a sample script.
y=c(12,15,28,17,18)
x=c(22,39,50,25,18)
mean(y)
mean(x)
plot(x,y)
fit <- lm(y~x)
summary(fit)
# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics
# diagnostic plots
#par(mfrow=c(2,2)) # optional 4 graphs/page
#par(mfrow=c(1,1))
plot(fit)
