### Exploring (esp. p 554 of ) https://web.archive.org/web/20170815080043id_/https://dash.harvard.edu/bitstream/handle/1/11718181/COPSS_50.pdf?sequence=1
#### Using Employment income in Canada: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1110024001

library(tidyverse)

income_classes = c(0, 5, 10, 20, 30, 40, 50, 60, 80, 100, 400)*1000

class_sizes = 21457 * 1000 *c(13.8, 8.6, 12.3, 9.6, 9.3, 8.6, 7.8, 11.2, 8.1, 10.6)/100

income_population = c()

for(i in 1:length(class_sizes))
	{
		income_population = c(income_population, runif(class_sizes[i], income_classes[i], income_classes[i+1]))
	}


population = tibble(income=round(income_population))

(mean_true = population %>% 
	summarize(mean = mean(income)) %>%
	pull())

population %>% 
	sample_n(size = 100) %>% 
	summarize(mean = mean(income), n=n())

population$latent_refusal_threshhold = (rnorm(n = dim(population)[1]) -.1) * 48000
population %>% summarize(rho = cor(pnorm(income/48000 + .1), income))

population$database_ids = population$income > population$latent_refusal_threshhold
population %>% 
	filter(database_ids==1) %>% 
	summarize(mean = mean(income), n=n(), f= n()/dim(population)[1])


means_randomsample = c()
means_database = c()

for (i in 1:1000)
{
	 means_randomsample = c(means_randomsample, 
		population %>% 
		sample_n(size=100) %>% 
		summarize(mean = mean(income)) %>%
			pull()
	 )
		
	population$latent_refusal_threshhold = (rnorm(n = dim(population)[1]) -.1 ) * 48000
	population$database_ids = population$income > population$latent_refusal_threshhold
	
	means_database = c(means_database, 
		population %>% 
		filter(database_ids==1) %>% 
		summarize(mean = mean(income)) %>% 
		pull()
		)
}

p2 <- hist(means_randomsample)
abline(v=mean_true, col="red")
p2 <- hist(means_database, plot=FALSE)
plot(p2, col="blue", add=TRUE)


sum((means_randomsample - mean_true)^2)
sum((means_database - mean_true)^2)
