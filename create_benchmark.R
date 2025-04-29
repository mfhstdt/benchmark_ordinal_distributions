#### CREATE BENCHMARK OF CATEGORICAL DISTRIBUTIONS TO TEST METHODS AGAINST #########

# CASES TO TEST ------------
N <- c(50,500,5000,50000) 
C <- c(5,10) # number of categories

# DISTRIBUTIONS TO TEST ---
#
# UNIFORM
#
# UNIMODAL 
#   Gaussian
#   beta(2,8) 
#   beta(4,4) 
#   beta(100,100) 
#   Special case: staircase
#
# BIMODAL 
#   Mixture of 2 Gaussians (same weight), small overlap
#   Mixture of 2 Gausssians (weights 60-40)
#   Mixture of 2 Gaussians (weights 70-30)
#   Mixture of 2 Gaussians (weights 80-20)
#   Mixture of 2 Gaussians (weights 90-10)
#   Mixture of 2 Gaussians (same weight), large overlap
#   beta(0.5, 0.5)
#   Special case: valley 
#   Special case: small local mode
#   Special case: small local mode weighed 
#
# Trimodal 
#   Mixture of 3 Gaussians (same weight)
#   Mixture of 3 Gaussians (weight 20-50-30)
#   Mixture of 3 Gaussians (weight 50-30-20)
#   Mixture of 3 Gaussians (weight 30-20-50)
#   Mixture of 3 Betas 



benchmark_distributions <- list()

# GENERATE UNIFORM DISTRIBUTIONS --------#
i = 0
for(n in N){
  for(c in C){
    set.seed(2)
    i=i+1
    dist <- sample(1:c, n, replace=TRUE)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Uniform_N_",n,"_C_",c)
  }
}


# GENERATE UNIMODAL DISTRIBUTIONS -----------

# simulate Gaussian
i = 8
for(n in N){
  for(c in C){
    set.seed(1)
    i=i+1
    dist <- rnorm(n, 0, 1) # standard normal
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 # scale to 0-1 without truncating
    dist <- ceiling(dist * c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Unimodal_Gaussian_N_",n,"_C_",c)
  }
}

# simulate betas 
i = 16
betas_unimodal <- list(
  c(2,8),
  c(4,4), 
  c(100,100))
for(k in 1:length(betas_unimodal)){
  for(n in N){
    for(c in C){
      set.seed(2)
      i = i+1
      dist <- rbeta(n, betas_unimodal[[k]][1], betas_unimodal[[k]][2])
      dist <- ceiling(dist*c)
      benchmark_distributions[[i]] <- factor(dist, levels=1:c)
      names(benchmark_distributions)[i] <- paste0("Unimodal_Beta_",betas_unimodal[[k]][1], ",", betas_unimodal[[k]][2],"_N_", n,"_C_",c)
    }
  }
}

# add special case: stair case distribution
i = 40
for(n in N){
  for(c in C){
    set.seed(1)
    i=i+1
    if(c == 5) prob <- seq(0.1, 0.3, by = 0.05) else prob <- (seq(1, 10, by = 1) / 55)
    dist <- rep(1:length(prob), times=prob*n) 
    dist <- factor(dist, levels=1:c)
    benchmark_distributions[[i]] <- dist
    names(benchmark_distributions)[i] <- paste0("Unimodal_Special_Staircase_N_", n,"_C_",c) # TODO: change name
  }
}


# GENERATE BIMODAL DISTRIBUTIONS ----------------

# simulate mixture of 2 Gaussians (small overlap) - equally weighted
i = 48
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(0.5, 0.5) # mixture proportions
    mu = c(0.2, 0.8); sd = (0.2/1.645) # component means and SD for scale 0-1
    # stochastic sampling from 2 Gaussians
    components <- sample(1:2, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1 without truncating
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
    dist <- ceiling(dist * c) # scale to c categories
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Gaussians_weight50,50_N_", n,"_C_",c)
  }
}

# simulate mixture of 2 Gaussians (small overlap) - weight 60-40
i = 56
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(0.6, 0.4) # mixture proportions
    mu = c(0.15, 0.85); sd = (0.2/1.645) # component means and SD for scale 0-1
    # stochastic sampling from 2 Gaussians
    components <- sample(1:2, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1 without truncating
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
    dist <- ceiling(dist * c) # scale to c categories
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Gaussians_weight60,40_N_", n,"_C_",c)
  }
}

# simulate mixture of 2 Gaussians (small overlap) - weight 70-30
i = 64
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(0.7, 0.3) # mixture proportions
    mu = c(0.15, 0.9); sd = (0.2/1.645) # component means and SD for scale 0-1
    # stochastic sampling from 2 Gaussians
    components <- sample(1:2, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1 without truncating
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
    dist <- ceiling(dist * c) # scale to c categories
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Gaussians_weight70,30_N_", n,"_C_",c)
  }
}

# simulate mixture of 2 Gaussians (small overlap) - weight 80-20
i = 72
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(0.8, 0.2) # mixture proportions
    mu = c(0.15, 0.85); sd = (0.2/1.645) # component means and SD for scale 0-1
    # stochastic sampling from 2 Gaussians
    components <- sample(1:2, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1 without truncating
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
    dist <- ceiling(dist * c) # scale to c categories
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Gaussians_weight80,20_N_", n,"_C_",c)
  }
}

# simulate mixture of 2 Gaussians (small overlap) - weight 90-10
i = 80
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(0.8, 0.2) # mixture proportions
    mu = c(0.15, 0.95); sd = (0.2/1.645) # component means and SD for scale 0-1
    # stochastic sampling from 2 Gaussians
    components <- sample(1:2, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1 without truncating
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
    dist <- ceiling(dist * c) # scale to c categories
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Gaussians_weight90,10_N_", n,"_C_",c)
  }
}


# simulate mixture of 2 Gaussians (larger overlap)
i = 88
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(0.5, 0.5) # mixture proportions
    mu = c(0.2, 0.8); sd = (0.2/1.645) # component means and SD for scale 0-1
    # stochastic sampling from 2 Gaussians
    components <- sample(1:2, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale without truncating
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
    dist <- ceiling(dist * c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Gaussians_larger_overlap_N_", n,"_C_",c)
  }
}


# simulate from beta(0.5, 0.5) 
i = 96
for(n in N){  
  for(c in C){
    set.seed(1)
    i = i+1
    dist <- rbeta(n, 0.5, 0.5)
    dist <- ceiling(dist*c)
    dist <- factor(dist, levels=1:c)
    benchmark_distributions[[i]] <- dist
    names(benchmark_distributions)[i] <- paste0("Bimodal_Beta_0.5,0.5_N_", n,"_C_",c)
  }
}

# add special cases
# valley
i = 104
for(n in N){
  for(c in C){
    set.seed(1)
    i=i+1
    if(c == 5) prob=c(0.24, 0.24, 0.04, 0.24, 0.24) else prob=c(0.154, 0.154, 0.154, 0.03, 0.008, 0.008, 0.03, 0.154, 0.154, 0.154)
    dist <- rep(1:length(prob), times=prob*n)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Special_Valley_N_", n,"_C_",c) # TODO: change name?
  }
}

# small local mode
i = 112
for(n in N){
  for(c in C){
    set.seed(1)
    i=i+1
    if(c == 5) prob=c(0.84, 0.03, 0.03, 0.03, 0.07) else prob=c(0.474, 0.474, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.02, 0.02)
    dist <- rep(1:length(prob), times=prob*n)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Special_Small_Mode_N_", n,"_C_",c) 
  }
}

# small local mode with stronger weight 
i = 120
for(n in N){
  for(c in C){
    set.seed(1)
    i=i+1
    if(c == 5) prob=c(0.64, 0.03, 0.03, 0.03, 0.27) else prob=c(0.394, 0.394, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.1, 0.1)
    dist <- rep(1:length(prob), times=prob*n)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Bimodal_Special_Small_Mode_weighted_N_", n,"_C_",c) 
  }
}

# GENERATE TRIMODAL DISTRIBUTIONS --------------

# mixture of 3 Gaussians (same weight)
i = 128
for(n in N){
  for(c in C){
    set.seed(1) 
    i = i+1
    pi = c(1/3, 1/3, 1/3) # mixture proportions
    mu = c(1/6, 0.5, 5/6) # component means
    sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
    # stochastic sampling from 3 Gaussians
    components <- sample(1:3, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
    dist <- ceiling(dist * c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Trimodal_Gaussians_N_", n,"_C_",c)
  }
}

# mixture of 3 Gaussians (weight 20-50-30)
i = 136
for(n in N){
  for(c in C){
    set.seed(1) 
    i = i+1
    pi = c(0.2, 0.5, 0.3) # mixture proportions
    mu = c(1/6, 0.5, 5/6) # component means
    sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
    # stochastic sampling from 3 Gaussians
    components <- sample(1:3, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
    dist <- ceiling(dist * c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Trimodal_Gaussians_weight20,50,30_N_", n,"_C_",c)
  }
}

# mixture of 3 Gaussians (weight 50-30-20)
i = 144
for(n in N){
  for(c in C){
    set.seed(1234) 
    i = i+1
    pi = c(0.5, 0.3, 0.2) # mixture proportions
    mu = c(1/6, 0.5, 5/6) # component means
    sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
    # stochastic sampling from 3 Gaussians
    components <- sample(1:3, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
    dist <- ceiling(dist * c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Trimodal_Gaussians_weight50,30,20_N_", n,"_C_",c)
  }
}

# mixture of 3 Gaussians (weight 30-20-50)
i = 152
for(n in N){
  for(c in C){
    set.seed(1234) 
    i = i+1
    pi = c(0.3, 0.2, 0.5) # mixture proportions
    mu = c(1/6, 0.5, 5/6) # component means
    sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
    # stochastic sampling from 3 Gaussians
    components <- sample(1:3, size = n, replace = TRUE, prob = pi)
    dist <- rnorm(n, mean = mu[components], sd = sd)
    # rescale to 0-1
    dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
    dist <- ceiling(dist * c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Trimodal_Gaussians_weight30,20,50_N_", n,"_C_",c)
  }
}


# simulate mixture of 3 betas
i = 160
for(n in N){
  for(c in C){
    set.seed(1)
    i = i+1
    pi = c(1/3, 1/3, 1/3) # mixture proportions
    shape1 = c(50, 1, 8) # beta dist. parameters of components
    shape2 = c(50, 8, 1)
    # stochastic sampling from 3 beta distributions
    components <- sample(1:3, size=n, replace=TRUE, prob=pi)
    dist <- rbeta(n, shape1[components], shape2[components])
    # rescale to 0-1
    dist <- ceiling(dist*c)
    benchmark_distributions[[i]] <- factor(dist, levels=1:c)
    names(benchmark_distributions)[i] <- paste0("Trimodal_beta_mixtures_N_", n,"_C_",c)
  }
}

# lastly: claw distribution
i = 168
for(n in N){
  set.seed(1)
  i=i+1
  dist=rnorm(.5*n,0,1)
  for(j in 0:4) dist = c(dist,rnorm(.1*n, j/2-1, .1))
  xt=hist(dist,breaks=30, plot=F)$counts
  dist <- rep(seq_along(xt), xt)
  benchmark_distributions[[i]] <- factor(dist)
  names(benchmark_distributions)[i] <- paste0("Claw_N_", n)
}

# save list as RData object
#saveRDS(benchmark_distributions, "benchmark_distributions.RData")


## save distributions as csv
#benchmark_df <- sapply(benchmark_distributions, function(x) { # make equal length
#  length(x) <- 275000 # max length  
#  x
#})
#benchmark_df <- as.data.frame(benchmark_df) 
#write.csv(benchmark_df, "benchmark_distributions.csv", row.names = FALSE)

#-----------------------

# Plot all distributions ---
#pdf("benchmark_distributions.pdf", width=12, height = 8)
#par(mfrow=c(2,4))
#for(n in 1:length(benchmark_distributions)){
#  barplot(table(benchmark_distributions[[n]]), main = names(benchmark_distributions)[n], cex.main=1)
#}
#dev.off()

# illustrate certain samples for paper--
#par(mfrow = c(3,3))
#barplot(table(benchmark_distributions[[13]]), main = "Unimodal Gaussian")
#barplot(table(benchmark_distributions[[22]]), main = "Unimodal Non-Gaussian")
#barplot(table(benchmark_distributions[[52]]), main = "Bimodal Gaussian")
#barplot(table(benchmark_distributions[[102]]), main = "Bimodal Non-Gaussian")
#barplot(table(benchmark_distributions[[144]]), main = "Trimodal Gaussian")
#barplot(table(benchmark_distributions[[166]]), main = "Trimodal Non-Gaussian")
#barplot(table(benchmark_distributions[[5]]), main = "Uniform")
#barplot(table(benchmark_distributions[[171]]), main = "Claw")
#par(mfrow=c(1,1))
