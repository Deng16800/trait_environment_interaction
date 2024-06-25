set.seed(123)

# library ----
library(tidyverse)
library(boral)

# load data
load('fs.full.RData')

# for all recruitment
fs2.tag <- subset(fs.full2, !is.na(date) & status0 == -9, select = tag)
fs3.tag <- subset(fs.full3, !is.na(date) & status0 == -9, select = tag)

fs2_to_fs4 <- subset(fs.full4, tag %in% fs2.tag$tag & status == 'A')
fs3_to_fs4 <- subset(fs.full4, tag %in% fs3.tag$tag & status == 'A')

fs4.20 <- subset(fs.full4, !is.na(date) & status0 == -9)

fsall <- rbind(fs2_to_fs4, fs3_to_fs4, fs4.20)

# 20 m * 20 m ----
fs.full2$q20 <- 25*floor(fs.full2$gx/20) + ceiling(fs.full2$gy/20)
fs.full3$q20 <- 25*floor(fs.full3$gx/20) + ceiling(fs.full3$gy/20)
fs.full4$q20 <- 25*floor(fs.full4$gx/20) + ceiling(fs.full4$gy/20)

## fs2 recruitment ----
# 20 m * 20 m
fs.full2$q20 <- 25*floor(fs.full2$gx/20) + ceiling(fs.full2$gy/20)

fs.full2$sp <- as.factor(fs.full2$sp)
fs.full2$q20 <- as.factor(fs.full2$q20)

fs2.20 <- fs.full2  %>% 
  group_by(sp, q20, .drop = F) %>%
  filter(!is.na(date) & status0 == -9) %>%
  summarize(nrecruits = length(tag))

fs2.20$sp <- as.character(fs2.20$sp)
fs2.20$q20 <- as.numeric(as.character(fs2.20$q20))
which(is.na(fs2.20$q20))
fs2.20 <- fs2.20[-7501,] #NA
fs2.20 <- fs2.20[-15001, ]

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 625, ncol = 111))
for(i in 1:length(unique(fs2.20$sp))){
  v <- vector()
  v <- fs2.20$nrecruits[fs2.20$sp %in% unique(fs2.20$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fs2.20$sp)
head(recruit)
dim(recruit)

# env
load('envfactor/scale_new_env20.rdata')
summary(env20)
env <- env20

# trait
load('trait/newtraits.rdata')
summary(newtraits)

# FS2 pick species
# which species recruit more than 5 quadrat?
# 62(20 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q20', 'nrecruits')
fs2.20 <- as.data.frame(fs2.20)
uni <- unique(as.character(fs2.20$sp))
for (i in 1:length(uni)) {
  f <- subset(fs2.20, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){
    pick  <- rbind(pick, f)
  }
}

# but I only have 59 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = 625, ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]

rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fs2.traits <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                    family = "negative.binomial", lv.control = list(num.lv = 4),
                    calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                    mcmc.control = mcmc, row.eff = "random")
save(fs2.traits, file = 'model_result/fs2traits.rdata')

## fs3 recruitment ----
# 20 m * 20 m
fs.full3$q20 <- 25*floor(fs.full3$gx/20) + ceiling(fs.full3$gy/20)

fs.full3$sp <- as.factor(fs.full3$sp)
fs.full3$q20 <- as.factor(fs.full3$q20)

fs3.20 <- fs.full3  %>% 
  group_by(sp, q20, .drop = F) %>%
  filter(!is.na(date) & status0 == -9) %>%
  summarize(nrecruits = length(tag))

fs3.20$sp <- as.character(fs3.20$sp)
fs3.20$q20 <- as.numeric(as.character(fs3.20$q20))
which(is.na(fs3.20$q20))
fs3.20 <- fs3.20[-7501,] #NA
fs3.20 <- fs3.20[-15001, ]

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 625, ncol = 111))
for(i in 1:length(unique(fs3.20$sp))){
  v <- vector()
  v <- fs3.20$nrecruits[fs3.20$sp %in% unique(fs3.20$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fs3.20$sp)
head(recruit)
dim(recruit)

# env
load('envfactor/scale_new_env20.rdata')
summary(env20)
env <- env20

# trait
load('trait/newtraits.rdata')
summary(newtraits)

# FS3 pick species
# which species recruit more than 5 quadrat?
# 51(20 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q20', 'nrecruits')
fs3.20 <- as.data.frame(fs3.20)
uni <- unique(as.character(fs3.20$sp))
for (i in 1:length(uni)) {
  f <- subset(fs3.20, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){
    pick  <- rbind(pick, f)
  }
}
summary(pick)
unique(pick$sp) #
length(unique(pick$sp))

# but I only have 49 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = 625, ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]

rownames(new) <- new$sp
new <- new[,-1]
dim(new)

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]
which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fs3.traits <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                    family = "negative.binomial", lv.control = list(num.lv = 4),
                    calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                    mcmc.control = mcmc, row.eff = "random")
save(fs3.traits, file = 'model_result/fs3traits.rdata')

## fs4 recruitment ----
# 20 m * 20 m
fs.full4$q20 <- 25*floor(fs.full4$gx/20) + ceiling(fs.full4$gy/20)

fs.full4$sp <- as.factor(fs.full4$sp)
fs.full4$q20 <- as.factor(fs.full4$q20)

fs4.20 <- fs.full4 %>% 
  group_by(sp, q20, .drop = F) %>%
  filter(!is.na(date) & status0 == -9) %>%
  summarize(nrecruits = length(tag))

fs4.20$sp <- as.character(fs4.20$sp)
fs4.20$q20 <- as.numeric(as.character(fs4.20$q20))
which(is.na(fs4.20$q20))
fs4.20 <- fs4.20[-7501,] #NA
fs4.20 <- fs4.20[-15001, ]

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 625, ncol = 111))
for(i in 1:length(unique(fs4.20$sp))){
  v <- vector()
  v <- fs4.20$nrecruits[fs4.20$sp %in% unique(fs4.20$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fs4.20$sp)

# env
load('envfactor/scale_new_env20.rdata')
summary(env20)
env <- env20

# trait
load('trait/newtraits.rdata')
summary(newtraits)

# FS4 pick species
# which species recruit more than 5 quadrat?
# 47(20 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q20', 'nrecruits')
fs4.20 <- as.data.frame(fs4.20)
uni <- unique(as.character(fs4.20$sp))
for (i in 1:length(uni)) {
  f <- subset(fs4.20, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){
    pick  <- rbind(pick, f)
  }
}

# but I only have 46 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = 625, ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]
rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fs4.traits <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                    family = "negative.binomial", lv.control = list(num.lv = 4),
                    calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                    mcmc.control = mcmc, row.eff = "random")
save(fs4.traits, file = 'model_result/fs4traits.rdata')

## fsall recruitment ----
# average model
fsall$q20 <-  25*floor(fsall$gx/20) + ceiling(fsall$gy/20)

fsallnew <- fsall  %>% 
  group_by(sp, q20, .drop = F) %>%
  summarize(nrecruits = length(tag))

fsall.empty <- tibble('sp' = rep(unique(fsallnew$sp), each = 625), 
                      'q20' = rep(1:625, length(unique(fsallnew$sp))), 
                      'nrecruits' = rep(0, 625*length(unique(fsallnew$sp))))

fsall_merged <- left_join(fsall.empty, fsallnew, by = c("sp", "q20"))
fsall_merged$nrecruits <- coalesce(fsall_merged$nrecruits.y, fsall_merged$nrecruits.x)
fsall.20 <- data.frame('sp' = fsall_merged$sp, 'q20' = fsall_merged$q20, 'nrecruits' = fsall_merged$nrecruits)

class(fsall.20)
dim(fsall.20)

# table of recruitment
recruit <- as.data.frame(matrix(nrow = 625, ncol = 111))
for(i in 1:length(unique(fsall.20$sp))){
  v <- vector()
  v <- fsall.20$nrecruits[fsall.20$sp %in% unique(fsall.20$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fsall.20$sp)

# env
load('envfactor/scale_new_env20.rdata')
summary(env20)
env <- env20

# trait
load('trait/newtraits.rdata')

# FSAll pick species
# which species recruit more than 5 quadrat?
# 69(20 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q20', 'nrecruits')
fsall.20 <- as.data.frame(fsall.20)
uni <- unique(as.character(fsall.20$sp))
for (i in 1:length(uni)) {
  f <- subset(fsall.20, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){
    pick  <- rbind(pick, f)
  }
}

# but I only have 61 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = 625, ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]
rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fsall.traits <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                      family = "negative.binomial", lv.control = list(num.lv = 4),
                      calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                      mcmc.control = mcmc, row.eff = "random")
save(fsall.traits, file = 'model_result/fsalltraits.rdata')

# 10 m * 10 m ----
fs.full2$q10 <- 50*floor(fs.full2$gx/10) + ceiling(fs.full2$gy/10)
fs.full3$q10 <- 50*floor(fs.full3$gx/10) + ceiling(fs.full3$gy/10)
fs.full4$q10 <- 50*floor(fs.full4$gx/10) + ceiling(fs.full4$gy/10)

## fs2 recruitment ----
# 10 m * 10 m
fs.full2$q10 <- 50*floor(fs.full2$gx/10) + ceiling(fs.full2$gy/10)

fs.full2$sp <- as.factor(fs.full2$sp)
fs.full2$q10 <- as.factor(fs.full2$q10)

fs2.10 <- fs.full2  %>% 
  group_by(sp, q10, .drop = F) %>%
  filter(!is.na(date) & status0 == -9) %>%
  summarize(nrecruits = length(tag))

fs2.10$sp <- as.character(fs2.10$sp)
fs2.10$q10 <- as.numeric(as.character(fs2.10$q10))
which(is.na(fs2.10$q10))
fs2.10 <- fs2.10[-30001,] #NA
fs2.10 <- fs2.10[-60001, ]

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 2500, ncol = 111))
for(i in 1:length(unique(fs2.10$sp))){
  v <- vector()
  v <- fs2.10$nrecruits[fs2.10$sp %in% unique(fs2.10$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fs2.10$sp)

# cut edge quadrats of recruitment data
# because the edge of aspect variable(environmental variables) are not good enough
source('edge.R')
edge10 <- edge(area = 500, grid = 10)
recruit <- recruit[-edge10,]

# env
load('envfactor/scale_new_env10.rdata')
summary(env10)
env <- env10
rownames(env) <- 1:nrow(env)

# trait
load('trait/newtraits.rdata')

# FS2 pick species
# which species recruit more than 5 quadrat?
# 63(10 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q10', 'nrecruits')
fs2.10 <- as.data.frame(fs2.10)
uni <- unique(as.character(fs2.10$sp))
for (i in 1:length(uni)) {
  f <- subset(fs2.10, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){
    pick  <- rbind(pick, f)
  }
}

# after remove species with less traits 
# also need to cut the edge quadrat
# nrow(pick[pick$q10 %in% edge10,]) # remove part
pick <- pick[!pick$q10 %in% edge10,]

# but I only have 60 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]

recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = nrow(env), ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]

rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fs2.traits.10 <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                       family = "negative.binomial", lv.control = list(num.lv = 4),
                       calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                       mcmc.control = mcmc, row.eff = "random")
save(fs2.traits.10, file = 'model_result/fs2traits10.rdata')

## fs3 recruitment ----
# 10 m * 10 m
fs.full3$q10 <- 50*floor(fs.full3$gx/10) + ceiling(fs.full3$gy/10)

fs.full3$sp <- as.factor(fs.full3$sp)
fs.full3$q10 <- as.factor(fs.full3$q10)

fs3.10 <- fs.full3  %>% 
  group_by(sp, q10, .drop = F) %>%
  filter(!is.na(date) & status0 == -9) %>%
  summarize(nrecruits = length(tag))

fs3.10$sp <- as.character(fs3.10$sp)
fs3.10$q10 <- as.numeric(as.character(fs3.10$q10))
which(is.na(fs3.10$q10))
fs3.10 <- fs3.10[-30001,] #NA
fs3.10 <- fs3.10[-60001, ]

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 2500, ncol = 111))
for(i in 1:length(unique(fs3.10$sp))){
  v <- vector()
  v <- fs3.10$nrecruits[fs3.10$sp %in% unique(fs3.10$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fs3.10$sp)

# cut edge quadrats of recruitment data
# because the edge of aspect variable(environmental variables) are not good enough
source('edge.R')
edge10 <- edge(area = 500, grid = 10)
recruit <- recruit[-edge10,]

# env
load('envfactor/scale_new_env10.rdata')
summary(env10)
env <- env10
rownames(env) <- 1:nrow(env)

# trait
load('trait/newtraits.rdata')

# FS3 pick species
# which species recruit more than 5 quadrat?
# 51(10 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q10', 'nrecruits')
fs3.10 <- as.data.frame(fs3.10)
uni <- unique(as.character(fs3.10$sp))
for (i in 1:length(uni)) {
  f <- subset(fs3.10, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){ #>5
    pick  <- rbind(pick, f)
  }
}

# after remove species with less traits 
# also need to cut the edge quadrat
# nrow(pick[pick$q10 %in% edge10,]) # remove part
pick <- pick[!pick$q10 %in% edge10,]

# but I only have 49 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = nrow(env), ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]
rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fs3.traits.10 <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                       family = "negative.binomial", lv.control = list(num.lv = 4),
                       calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                       mcmc.control = mcmc, row.eff = "random")
save(fs3.traits.10, file = 'model_result/fs3traits10.rdata')

## fs4 recruitment ----
# 10 m * 10 m
fs.full4$q10 <- 50*floor(fs.full4$gx/10) + ceiling(fs.full4$gy/10)

fs.full4$sp <- as.factor(fs.full4$sp)
fs.full4$q10 <- as.factor(fs.full4$q10)

fs4.10 <- fs.full4  %>% 
  group_by(sp, q10, .drop = F) %>%
  filter(!is.na(date) & status0 == -9) %>%
  summarize(nrecruits = length(tag))

fs4.10$sp <- as.character(fs4.10$sp)
fs4.10$q10 <- as.numeric(as.character(fs4.10$q10))
which(is.na(fs4.10$q10))
fs4.10 <- fs4.10[-30001,] #NA
fs4.10 <- fs4.10[-60001, ]

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 2500, ncol = 111))
for(i in 1:length(unique(fs4.10$sp))){
  v <- vector()
  v <- fs4.10$nrecruits[fs4.10$sp %in% unique(fs4.10$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fs4.10$sp)

# cut edge quadrats of recruitment data
# because the edge of aspect variable(environmental variables) are not good enough

source('edge.R')
edge10 <- edge(area = 500, grid = 10)
recruit <- recruit[-edge10,]

# env
load('envfactor/scale_new_env10.rdata')
summary(env10)
env <- env10
rownames(env) <- 1:nrow(env)

# trait
load('trait/newtraits.rdata')
summary(newtraits)

# FS4 pick species
# which species recruit more than 5 quadrat?
# 47(10 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q10', 'nrecruits')
fs4.10 <- as.data.frame(fs4.10)
uni <- unique(as.character(fs4.10$sp))
for (i in 1:length(uni)) {
  f <- subset(fs4.10, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){ #>5
    pick  <- rbind(pick, f)
  }
}

# after remove species with less traits 
# also need to cut the edge quadrat
# nrow(pick[pick$q10 %in% edge10,]) # remove part
pick <- pick[!pick$q10 %in% edge10,]

# but I only have 46 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = nrow(env), ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]

rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fs4.traits.10 <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                       family = "negative.binomial", lv.control = list(num.lv = 4),
                       calc.ics = FALSE, save.model = TRUE, 
                       mcmc.control = mcmc, row.eff = "random"
)
save(fs4.traits.10, file = 'model_result/fs4traits10.rdata')

## fsall recruitment ----
# 10 m * 10 m
fsall$q10 <- 50*floor(fsall$gx/10) + ceiling(fsall$gy/10)

fsallnew <- fsall  %>% 
  group_by(sp, q10, .drop = F) %>%
  summarize(nrecruits = length(tag))

fsall.empty <- tibble('sp' = rep(unique(fsallnew$sp), each = 2500), 
                      'q10' = rep(1:2500, length(unique(fsallnew$sp))), 
                      'nrecruits' = rep(0, 2500*length(unique(fsallnew$sp))))

fsall_merged <- left_join(fsall.empty, fsallnew, by = c("sp", "q10"))
fsall_merged$nrecruits <- coalesce(fsall_merged$nrecruits.y, fsall_merged$nrecruits.x)
fsall.10 <- data.frame('sp' = fsall_merged$sp, 'q10' = fsall_merged$q10, 'nrecruits' = fsall_merged$nrecruits)

# for null model
# table of recruitment   
recruit <- as.data.frame(matrix(nrow = 2500, ncol = 111))
for(i in 1:length(unique(fsall.10$sp))){
  v <- vector()
  v <- fsall.10$nrecruits[fsall.10$sp %in% unique(fsall.10$sp)[i]]
  recruit[,i] <- v
}
colnames(recruit) <- unique(fsall.10$sp)

# cut edge quadrats of recruitment data
# because the edge of aspect variable(environmental variables) are not good enough
source('edge.R')
edge10 <- edge(area = 500, grid = 10)
recruit <- recruit[-edge10,]

# env
load('envfactor/scale_new_env10.rdata')
summary(env10)
env <- env10
rownames(env) <- 1:nrow(env)

# trait
load('trait/newtraits.rdata')
summary(newtraits)

# FS4 pick species
# which species recruit more than 5 quadrat?
# 70(10 m) speices have recruitment including more than 5 quadrat
pick <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pick) <- c('sp' , 'q10', 'nrecruits')
fsall.10 <- as.data.frame(fsall.10)
uni <- unique(as.character(fsall.10$sp))
for (i in 1:length(uni)) {
  f <- subset(fsall.10, sp == uni[i])
  if(sum(f$nrecruits>0) > 5){ #>5
    pick  <- rbind(pick, f)
  }
}

# after remove species with less traits 
# also need to cut the edge quadrat
# nrow(pick[pick$q10 %in% edge10,]) # remove part
pick <- pick[!pick$q10 %in% edge10,]

# but I only have 61 species traits 
havetraitsp <- unique(pick$sp)[unique(pick$sp) %in% newtraits$sp]
recruit_dataframe <- pick[pick$sp %in% havetraitsp,]

# data frame to table
recruit <- as.data.frame(matrix(nrow = nrow(env), ncol = length(unique(recruit_dataframe$sp))))
for(i in 1:length(unique(recruit_dataframe$sp))){
  V <- vector()
  V <- recruit_dataframe$nrecruits[recruit_dataframe$sp %in% unique(recruit_dataframe$sp)[i]]
  recruit[,i] <- V
}
colnames(recruit) <- unique(recruit_dataframe$sp)

## traits
new <- newtraits[newtraits$sp %in% havetraitsp,]
rownames(new) <- new$sp
new <- new[,-1]

# check if there are some sp don't have trait
new[!rownames(new) %in% colnames(recruit),]

which.trait <- vector("list", ncol(env) + 1)
for(i in 1:length(which.trait)) 
{which.trait[[i]] <- 1:ncol(new)}

# MCMC control
mcmc <- list(n.burnin = 5*10^4, n.iteration = 10^5, n.thin = 50)

# boral model
fsall.traits.10 <- boral(y = recruit, X = env, traits = scale(new), which.traits = which.trait, 
                         family = "negative.binomial", lv.control = list(num.lv = 4),
                         calc.ics = FALSE, save.model = TRUE, hypparams = c(100,20,100,50),
                         mcmc.control = mcmc, row.eff = "random")
save(fsall.traits.10, file = 'model_result/fsalltraits10.rdata')
