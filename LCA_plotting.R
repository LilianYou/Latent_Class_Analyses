library(MplusAutomation)
library(tidyverse)

AllOutput <- readModels("C:\\Input\\Your\\File\\Path\\Here")

model_results <- data.frame()

for (i in 1:length(AllOutput)) {
  temp <- AllOutput[[i]]$parameters$unstandardized %>% mutate(model = paste(i, "-Class Model"))
  model_results <- rbind(model_results, temp)
}

rm(temp)

model_results <- model_results %>% 
  filter(paramHeader == "Thresholds") %>% 
  select(est, model, LatentClass, param) %>% 
  mutate(prob = (1 / (1 + exp(est)))) %>% 
  select(-est)

ggplot(model_results, aes(x = param, y = prob, color = LatentClass, shape = LatentClass, group = LatentClass)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ model, ncol = 1) +
  labs(title = "LCA on IES Behavior Data")
