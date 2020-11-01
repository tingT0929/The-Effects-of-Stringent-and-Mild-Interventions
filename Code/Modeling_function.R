## Package
library(deSolve)
library(extraDistr)
library(EnvStats)
library(truncdist)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)

## Function Time-varying Transmissibility
f_alp <- function(k, alp) {
    (k <= policy_change_1) * alp[1] + (k > policy_change_1) * (k <= policy_change_2) * alp[2] + (k > 
        policy_change_2) * alp[3]
}

# Dynamic ODE
eqn <- function(time, init, para, N) {
    dS <- -para[1] * init[2] * init[1]/N
    dI <- -dS - init[2]/para[2] - init[2]/para[3]
    dR <- init[2]/para[2]
    return(list(c(dS, dI, dR)))
}

# Likelihood
time_l <- function(k, Y, para_1, para, N, cmark) {
    para[1] <- para_1[k]
    out <- as.numeric(ode(y = Y[k, ], times = k:(k + 1), eqn, parms = para, N = N, atol = 1)[2, -1]) + 
        10^(-10)
    Lik <- sum(dtpois(floor(Y[k + 1, cmark]), out[cmark], -10^(-10), b = Inf, log = T))
    return(Lik)
}

l <- function(para, I, R, N, mark, para_1, cmark) {
    Y <- cbind(N - I - R, I, R)
    logLik <- sum(sapply(mark, time_l, Y = Y, para_1 = para_1, para = para, N = N, cmark = cmark))
    if (is.na(logLik) == T) {
        logLik <- -Inf
    }
    return(logLik)
}

gibbs <- function(para_t, R, N) {
    
    para <- para_t[[1]]
    I <- para_t[[2]]
    R_L <- para_t[[3]]
    sigma_R_L <- para_t[[4]]
    alp <- para_t[[5]]
    max_l <- para_t[[6]]
    
    para_1 <- f_alp(1:(nrow(Y) - 1), alp)
    
    # Update sigma_C_L
    sigma_R_L_t <- rinvgamma(1, length(R)/2, sum((R - R_L)^2)/2)
    r <- sum(ddnorm(R, R_L, sqrt(sigma_R_L_t), log = T) - ddnorm(R, R_L, sqrt(sigma_R_L), log = T)) + 
        dinvgamma(sigma_R_L, length(R)/2, sum((R - R_L)^2)/2, log = T) - dinvgamma(sigma_R_L_t, length(R)/2, 
        sum((R - R_L)^2)/2, log = T)
    U <- log(runif(1))
    if (U < r) {
        sigma_R_L <- sigma_R_L_t
    }
    
    # Updata para2
    para_t <- para
    para_t[2] <- runif(1, 5.1, 9.5)
    l_t <- l(para_t, I, R_L, N, 1:(length(R) - 1), f_alp(1:(nrow(Y) - 1), alp), 2:3)
    r <- l_t - max_l
    U <- log(runif(1))
    if (U < r) {
        para <- para_t
        max_l <- l_t
    }
    
    # Update R
    R_L[1] <- R[1]
    for (k in 2:(length(R) - 1)) {
        R_L_t <- R_L
        R_L_t[k] <- rdnorm(1, R[k], sqrt(sigma_R_L))
        r <- l(para, I, R_L_t, N, (k - 1):k, para_1, 2:3) - l(para, I, R_L, N, (k - 1):k, para_1, 
            2:3)
        U <- log(runif(1))
        if (U < r) {
            R_L <- R_L_t
        }
    }
    
    for (k in length(R)) {
        R_L_t <- R_L
        R_L_t[k] <- rdnorm(1, R[k], sqrt(sigma_R_L))
        r <- l(para, I, R_L_t, N, k - 1, para_1, 2:3) - l(para, I, R_L, N, k - 1, para_1, 2:3)
        U <- log(runif(1))
        if (U < r) {
            R_L <- R_L_t
        }
    }
    
    Y <- cbind(R_L, R_L, R_L)
    for (h in 1:3) {
        if (runif(1) < 0.5) {
            Y[1, 2] <- rtnorm(1, 10, 5, 1, Inf)
            Y[1, 1] <- N - sum(Y[1, -1])
            mark <- 1:(length(R) - 1)
            
            for (k in mark) {
                para[1] <- para_1[k]
                out <- as.numeric(ode(y = as.numeric(Y[k, ]), times = k:(k + 1), eqn, parms = para, 
                  N = N)[2, -1])
                Y[k + 1, 2] <- rtpois(1, out[2], a = -10^(-10), b = Inf)
                Y[k + 1, 1] <- N - sum(Y[k + 1, -1])
            }
            
            I_t <- Y[, 2]
            r <- l(para, I_t, R_L, N, 1:(length(R) - 1), para_1, 3) - l(para, I, R_L, N, 1:(length(R) - 
                1), para_1, 3)
            U <- log(runif(1))
            if (U < r) {
                I <- I_t
            }
            
        } else {
            for (k in 1) {
                I_t <- I
                I_t[k] <- rtnorm(1, I[k], 2, 1, Inf)
                r <- l(para, I_t, R_L, N, k, para_1, 2:3) - l(para, I, R_L, N, k, para_1, 2:3) + 
                  dtnorm(I[k], I_t[k], 2, 1, Inf, log = T) - dtnorm(I_t[k], I[k], 2, 1, Inf, log = T) + 
                  dtnorm(I_t[k], 10, 5, 1, Inf, log = T) - dtnorm(I[k], 10, 5, 1, Inf, log = T)
                U <- log(runif(1))
                if (U < r) {
                  I <- I_t
                }
            }
            
            for (k in 2:(length(R) - 1)) {
                I_t <- I
                
                a <- c(0, I[k - 1], R_L[k - 1])
                a[1] <- N - sum(a)
                para[1] <- para_1[k - 1]
                
                out <- as.numeric(ode(y = a, times = (k - 1):k, eqn, parms = para, N = N)[2, -1])
                I_t[k] <- rtpois(1, out[2], a = -10^(-10), b = Inf)
                
                r <- l(para, I_t, R_L, N, k, para_1, 2:3) - l(para, I, R_L, N, k, para_1, 2:3)
                U <- log(runif(1))
                if (U < r) {
                  I <- I_t
                }
            }
            
            for (k in (length(R))) {
                I_t <- I
                
                a <- c(0, I[k - 1], R_L[k - 1])
                a[1] <- N - sum(a)
                para[1] <- para_1[k - 1]
                
                out <- as.numeric(ode(y = a, times = (k - 1):k, eqn, parms = para, N = N)[2, -1])
                I_t[k] <- rtpois(1, out[2], a = -10^(-10), b = Inf)
            }
        }
    }
    
    max_l <- l(para, I, R_L, N, 1:(length(R) - 1), para_1, 2:3)
    
    # Update alp
    for (i in 1) {
        alp_t <- alp
        alp_t[i] <- rtrunc(1, "gamma", alp[2], Inf, 2, 2)
        l_t <- l(para, I, R_L, N, 1:(length(R) - 1), f_alp(1:(nrow(Y) - 1), alp_t), 2:3)
        r <- l_t - max_l
        U <- log(runif(1))
        if (U < r) {
            alp <- alp_t
            max_l <- l_t
        }
    }
    
    for (i in 2) {
        alp_t <- alp
        alp_t[i] <- rtrunc(1, "gamma", alp[3], alp[1], 2, 2)
        l_t <- l(para, I, R_L, N, 1:(length(R) - 1), f_alp(1:(nrow(Y) - 1), alp_t), 2:3)
        r <- l_t - max_l
        U <- log(runif(1))
        if (U < r) {
            alp <- alp_t
            max_l <- l_t
        }
    }
    
    for (i in 3) {
        alp_t <- alp
        alp_t[i] <- rtrunc(1, "gamma", 0, alp[2], 2, 2)
        l_t <- l(para, I, R_L, N, 1:(length(R) - 1), f_alp(1:(nrow(Y) - 1), alp_t), 2:3)
        r <- l_t - max_l
        U <- log(runif(1))
        if (U < r) {
            alp <- alp_t
            max_l <- l_t
        }
    }
    
    return(list(para, I, R_L, sigma_R_L, alp, max_l))
}

