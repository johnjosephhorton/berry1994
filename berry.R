library(extRemes)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lfe)

## About: This is John Horton's <https:://www.john-joseph-horton.com/> attempt to do Berry 1994 in R.
## Note: JJH knows very little about structural IO, so caveat lector

## Link to original paper: https://www.jstor.org/stable/2555829?seq=1#page_scan_tab_contents
## Berry inversion details: https://stats.stackexchange.com/questions/86715/berry-inversion
## Schum's Notes on Berry 1994: http://www.its.caltech.edu/~mshum/gradio/berry.pdf

## To start, we assume there is no price endogeneity and fix price & product characteristics. 

beta.price <- -2
beta.X <- 1
num.consumers <- 100000
num.products <- 500
price <- runif(num.products)
X <- runif(num.products) ## a product characteristics 
xi <- rnorm(num.products, 0, 0.1) ## Unobserved quality, or the structural error 
df.consumers <- data.frame(i = 1:num.consumers)
df.products <- data.frame(price = price, X =  X, xi = xi, j = 1:num.products)

# Cross products and consumers & have them make their choice 
df.choice <- merge(df.consumers, df.products, by = NULL) %>%
    mutate(epsilon = revd(nrow(.))) %>%
    mutate(U = beta.price * price + beta.X * X + xi + epsilon) %>%
    group_by(i) %>%
    mutate(Umax = max(U)) %>%
    ungroup %>%
    mutate(y = Umax == U)

## Compute market shares 
df.shares <- df.choice %>% group_by(j, X, price) %>% summarise(s = mean(y))

## Logistic regression, which "works" in this exogenous case  
m <- glm(s ~ price + X, family = "quasibinomial", data = df.shares)


###################
### Endogenous case 
###################

raw.price <- runif(num.products)
xi <- rnorm(num.products, 0, 0.1) + raw.price # create endogeneity - structural error related to price
Z <- abs(runif(num.products)) # create an instrument 
price <- raw.price + Z # actual price affected by instrument
df.consumers <- data.frame(i = 1:num.consumers)
df.products <- data.frame(price = price,
                          X =  X,
                          xi = xi,
                          j = 1:num.products,
                          Z = Z)

df.choice <- merge(df.consumers, df.products, by = NULL) %>%
    mutate(epsilon = revd(nrow(.))) %>%
    mutate(U = beta.price * price + beta.X * X + xi + epsilon) %>%
    group_by(i) %>%
    mutate(Umax = max(U)) %>%
    ungroup %>%
    mutate(y = Umax == U)

df.shares <- df.choice %>% group_by(j, X, price, Z) %>% summarise(s = mean(y))

## Show naive thing doesn't "work"
m <- glm(s ~ price + X, family = "quasibinomial", data = df.shares)
summary(m)

## Do the Berry inversion 
## Set j = 1 as the outside good.
l.s0 <- df.shares %>% filter(j == 1) %$% s %>% log

## Compute differences in market share 
df.shares %<>% mutate(delta = log(s) - l.s0) %>%
    filter(j != 1) %>%
    filter(s > 0)

## Check it with the wrong-SE naive approach
first.stage <- lm(price ~ Z + X, data = df.shares)
df.shares$price.hat <- predict(first.stage)
second.stage <- lm(delta ~ price.hat + X, data = df.shares)
summary(second.stage)

## Use the lfe package to do 2SLS 
m <- felm(delta ~ X | 0 | (price ~ Z) | 0,
          data = df.shares)

summary(m) # check that it seems to work - price is close. 
