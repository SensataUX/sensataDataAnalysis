#Minimal working example

library(tidyverse)
graphData <- tibble(y = rep(1:5,2),
                    ponde = runif(10, min = 0, max = 1),
                    )

totalData <- graphData %>%
  summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
            val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
            val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
            val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
            val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)

# What I would like

nFinal <- 10 #or any valued

totalData <- graphData %>%
  summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
            val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
            val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
            val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
            val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
            ...
          valnFinal = prop.table(questionr::wtd.table(y, weights = ponde))[nFinal]*100)
