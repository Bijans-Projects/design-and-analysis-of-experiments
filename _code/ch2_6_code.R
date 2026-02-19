library(tidyverse)
library(here)

x<- rnorm(6)
dat <- tibble(x)
dat

dat$x = rnorm(6); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = rnorm(6); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = rnorm(6); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")


dat$x = runif(6); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = runif(6); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = runif(6); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")


dat <- tibble(x=rnorm(60))
dat$x = rnorm(60); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = rnorm(60); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = rnorm(60); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = runif(60); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = runif(60); 
dat %>%
ggplot(aes(sample=x)) +
geom_qq() +
geom_qq_line() +
ggtitle("Normal Quantile Plot")

dat$x = runif(60); 
dat %>%
	ggplot(aes(sample=x)) +
	geom_qq() +
	geom_qq_line() +
	ggtitle("Normal Quantile Plot")







############ 
## Randomization/simulation inference
## Example 2.14


phat.mg = .29
n.mg		= 2840

num_c_sections.mg = round(phat.mg * n.mg)


phat.nyg = .32
n.nyg 	 = 4939

num_c_sections.nyg = round(phat.nyg * n.nyg)


### simulation
phat.H0 = (num_c_sections.mg + num_c_sections.nyg)/(n.mg + n.nyg)

sim.mg = tibble("hospital"="MG", "C section"=rbinom(n = n.mg, size = 1, prob = phat.H0))

sim.nyg = tibble("hospital"="NYG", "C section"=rbinom(n = n.nyg, size = 1, prob = phat.H0))

phat.mg.sim = mean(sim.mg$`C section`)
phat.nyg.sim = mean(sim.nyg$`C section`)

phatdiff.sim = phat.nyg.sim - phat.mg.sim

for(i in 1:9999){
	sim.mg = tibble("hospital"="MG", "C section"=rbinom(n = n.mg, size = 1, prob = phat.H0))
	
	sim.nyg = tibble("hospital"="NYG", "C section"=rbinom(n = n.nyg, size = 1, prob = phat.H0))
	
	phat.mg.sim = mean(sim.mg$`C section`)
	phat.nyg.sim = mean(sim.nyg$`C section`)
	
	phatdiff.sim = c(phatdiff.sim, phat.nyg.sim - phat.mg.sim)
}

hist(phatdiff.sim)

abline(v=.03)

mean(abs(phatdiff.sim) > .03)



### randomization

truth.mg = tibble("hospital"="MG", "C section"=c(rep(1, num_c_sections.mg), rep(0, n.mg-num_c_sections.mg)))
truth.nyg = tibble("hospital"="NYG", "C section"=c(rep(1, num_c_sections.nyg), rep(0, n.nyg-num_c_sections.nyg)))
truth = full_join(truth.mg, truth.nyg)

truth |> 
	group_by(hospital) |>
	summarise(p = mean(`C section`)) |> 
	pull() |>
	diff()


View(truth)


truth$randomized_hospital = truth$hospital[order(rnorm(n=(n.mg + n.nyg)))]

(phatdiff.r = 
	truth |> 
	group_by(randomized_hospital) |>
	summarise(p = mean(`C section`)) |> 
	pull() |>
	diff()
)

for(i in 1:9999){
	truth$randomized_hospital = truth$hospital[order(rnorm(n=(n.mg + n.nyg)))]
	
	phatdiff.r = c(phatdiff.r,
			truth |> 
			group_by(randomized_hospital) |>
			summarise(p = mean(`C section`)) |> 
			pull() |>
			diff()
	)
	
}

hist(phatdiff.r)

abline(v=.03)

mean(abs(phatdiff.r) > .03)

## 

quantile(phatdiff.r, probs = c(.025, .975))
quantile(phatdiff.sim, probs = c(.025, .975))

## Model-based:

SE = sqrt(phat.H0*(1-phat.H0)*(1/n.mg + 1/n.nyg))

0 + c(-1, 1) * 1.96*SE

.03 + c(-1, 1) * 1.96*SE

sqrt(var(phatdiff.r))
sqrt(var(phatdiff.sim))

qqnorm(phatdiff.sim)
qqnorm(phatdiff.r)

#############
## Example 2.15

library(scidesignR)
library(tidyverse)
library(broom)

data_2017 = lifesat_childmort |> filter(Year==2017)

lm(LifeSatisfaction~Under5mort, data=data_2017)

ggplot(data = data_2017, aes(x = Under5mort, y = LifeSatisfaction)) +
	geom_point(color = "blue", alpha = 0.6) +
	geom_smooth(method="lm") +
	labs(
		title = "Life Satisfaction vs. Under-5 Mortality",
		x = "Under-5 Mortality Rate",
		y = "Life Satisfaction"
	) +
	theme_minimal()

lm(I(log(LifeSatisfaction))~I(log(Under5mort)), data=data_2017)

ggplot(data = data_2017, aes(x = log(Under5mort), y = log(LifeSatisfaction))) +
	geom_point(color = "blue", alpha = 0.6) +
	geom_smooth(method="lm") +
	labs(
		title = "Life Satisfaction vs. Under-5 Mortality",
		x = "log Under-5 Mortality Rate",
		y = "log Life Satisfaction"
	) +
	theme_minimal()


model <- lm(log(LifeSatisfaction) ~ log(Under5mort), data = data_2017)


# Generate predictions with confidence intervals
prediction_data <- data_2017 %>%
	mutate(
		log_Under5mort = log(Under5mort)
	) %>%
	mutate(
		predicted_log_LifeSatisfaction = predict(model, newdata = ., interval = "confidence"),
		lower_log = predicted_log_LifeSatisfaction[, "lwr"], # Lower bound (log-scale)
		upper_log = predicted_log_LifeSatisfaction[, "upr"], # Upper bound (log-scale)
		predicted_LifeSatisfaction = exp(predicted_log_LifeSatisfaction[, "fit"]), # Back-transform
		lower = exp(lower_log), # Back-transform lower bound
		upper = exp(upper_log)  # Back-transform upper bound
	)

# Plot the data with the back-transformed regression line and error bars
ggplot(data = data_2017, aes(x = Under5mort, y = LifeSatisfaction)) +
	geom_point(color = "blue", alpha = 0.6) + # Scatterplot points
	geom_line(
		data = prediction_data %>% arrange(Under5mort),
		aes(x = Under5mort, y = predicted_LifeSatisfaction),
		color = "red",
		size = 1
	) + # Back-transformed regression line
	geom_ribbon(
		data = prediction_data %>% arrange(Under5mort),
		aes(x = Under5mort, ymin = lower, ymax = upper),
		fill = "red",
		alpha = 0.2
	) + # Error band (confidence interval)
	labs(
		title = "Life Satisfaction vs. Under-5 Mortality with Confidence Intervals",
		x = "Under-5 Mortality Rate",
		y = "Life Satisfaction"
	) +
	theme_minimal()




regions = read_csv(here("Data Geographies - v2 - by Gapminder - list-of-countries-etc.csv"))[1:196,]
regions$`World bank region` = factor(regions$`World bank region`, levels=
																		 	c("South Asia", "Sub-Saharan Africa", "Middle East & North Africa",  "East Asia & Pacific" ,  "Latin America & Caribbean", "Europe & Central Asia", 	"North America"), ordered=FALSE) 


lifesat_regions = lifesat_childmort %>% 
	left_join(regions, by = c("Country"="name")) %>% 
	mutate(year = as.integer(Year)) %>% 
	select(country=Country, year, life_satisfaction = LifeSatisfaction, region=`World bank region`) %>% 
	drop_na() 


lifesat_regions %>% 
	filter(year==2017) %>% 
	group_by(region) %>% 
	summarize(mean_life_satisfaction = mean(life_satisfaction)) %>% 
	ggplot(aes(x=mean_life_satisfaction, y=region)) + 
	geom_col()


summary(lm(life_satisfaction~region, data=lifesat_regions %>% filter(year==2017)))




###########

#step-by-step matrix multiplication for weighing problem
X <- matrix(c(1, 1, 1, -1), nrow = 2, ncol = 2) #define X matrix
solve(t(X) %*% X) %*% t(X) #(X'X)^(-1)*X'





X = rbind(diag(7), c(0,0,0,0,0,0,1))
solve(t(X)%*%X) %*% t(X)
solve(t(X)%*%X)
X
X[8,] = c(1,0,0,0,0,0,-1)
X
solve(t(X)%*%X)%*%t(X)
X


Xy = matrix(c(rep(1,7), c(1,1,1,rep(0,4)), c(1,0,0,1,1,0,0), c(1,0,0,0,0,1,1), c(0,1,0,1,0,1,0), c(0,1,0,0,1,0,1), c(0,0,1,1,0,0,1), c(0,0,1,0,1,1,0)), nrow=8, byrow=TRUE)
Xy
solve(t(Xy)%*%Xy)%*%t(Xy)
solve(t(Xy)%*%Xy)%*%t(Xy)*16
solve(t(Xy)%*%Xy)
solve(t(Xy)%*%Xy)*16
X
solve(t(X)%*%X)*16
solve(t(Xy)%*%Xy)*16

Xh = Xy
Xh[Xh==0]=-1
solve(t(Xh)%*%Xh)%*%t(Xh)*16

solve(t(Xh)%*%Xh)
solve(t(Xh)%*%Xh)*16
