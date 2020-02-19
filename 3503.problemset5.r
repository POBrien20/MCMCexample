#Stat 3503 HW 3


#1  
y1 <- .5
y2 <- .9
m <- c(y1, y2)
rho <- c(.3, .6, .9)
for (p in rho){
  theta <- matrix(m, byrow = TRUE, ncol = 2)
  n <-1000
  theta2 <- y2
  for (i in 1:n){
    theta1 <- rnorm(n = 1, y1 + p*(theta2-y2), 1-p^2)
    theta <- rbind(theta, c(theta1, 0))
    theta2 <- rnorm(n = 1, y2 + p*(theta1-y1), 1-p^2)
    theta[i+1, 2] <- theta2
  }
  heading1 <- paste("rho = ", p)
  heading2 <- paste("traceplot for rho = ", p)
  
  #scatterplot
  #plot(theta[,1], theta[,2], main = heading1, xlim = c(-3,4), ylim = c(-3,4))
  
  #traceplot
  #plot(1:1001, theta[,1], main = heading2, ylim = c(-3,4))
  #lines(1:1001, theta[,1], ylim = c(-3,4))
  #abline(a = y1, b = 0, col = "blue")
  
  #Autocorrelation
  #acf(theta[,1], lag.max = 10, type = "correlation", plot = TRUE)
  
  ##effective sample size
  #print(p)
  #print(effectiveSize(theta[,1]))
}

#2
sigs <- c(.1, .2, .5, 1, 2, 4, 6, 9)
p <- .9
y1 <- 1
y2 <- 1
m <- c(y1, y2)
sig <- matrix(c(1, p, p, 1), byrow = TRUE, nrow = 2)
proposal_sig_root <- matrix(c(1, 0, 0, 1), byrow = TRUE, nrow = 2)
n <- 10000
sigdf <- data.frame(sigma_val = c(), eff_size = c(), acceptance_ratio = c())
for (s in sigs){
  theta <- matrix(m, byrow = TRUE, ncol = 2)
  proposal_sig <- s * proposal_sig_root
  count <- 0
  
  for(i in 1:n){
    count <- count + 1
    new_theta_proposal <- mvrnorm(n = 1, mu = m, Sigma = proposal_sig)
    previous_theta <- theta[i,]
    acceptance_probability <- dmvnorm(new_theta_proposal, mean = m, sigma = sig) / dmvnorm(m, mean = m, sigma = sig)
    A <- min(1, acceptance_probability)
    if (runif(1) <= A){
      new_theta_proposal <- previous_theta
      count <- count - 1
    }
    theta <- rbind(theta, new_theta_proposal)
    
  }
  newdf <- data.frame(sigma_val = s, eff_size = effectiveSize(theta[,1]), acceptance_ratio = count/n)
  sigdf <- rbind(sigdf, newdf)
  
  
  
  #scatterplot
  #plot(theta[,1], theta[,2])
  
  #traceplot
  #heading <- paste("traceplot when sigma = ", s)
  #plot(1:10001, theta[,1], main = heading)
  #lines(1:10001, theta[,1])
  #abline(a = y1, b = 0, col = "blue")

  #acf
  #acf(theta[,1], lag.max = 10, type = "correlation", plot = TRUE)
}

#plot(sigdf$sigma_val, sigdf$eff_size)
#lines(sigdf$sigma_val, sigdf$eff_size)

#plot(sigdf$sigma_val, sigdf$acceptance_ratio)
#lines(sigdf$sigma_val, sigdf$acceptance_ratio)

plot(sigdf$eff_size, sigdf$sigma_val, xlab = "effectiveSize", ylab = "sigma^2")
lines(sigdf$eff_size, sigdf$sigma_val)

plot(sigdf$acceptance_ratio, sigdf$sigma_val, xlab = "acceptanceRatio", ylab = "sigma^2")
lines(sigdf$acceptance_ratio, sigdf$sigma_val)

sigdf

#2e
sigs <- c(.1, .2, .5, 1, 2, 4, 6, 9)
p <- .9
y1 <- 1
y2 <- 1
m <- c(y1, y2)
sig <- matrix(c(1, p, p, 1), byrow = TRUE, nrow = 2)
proposal_sig_root <- matrix(c(1, p, p, 1), byrow = TRUE, nrow = 2)
n <- 10000
sigdf <- data.frame(sigma_val = c(), eff_size = c(), acceptance_ratio = c())
for (s in sigs){
  theta <- matrix(m, byrow = TRUE, ncol = 2)
  proposal_sig <- s * proposal_sig_root
  count <- 0

  for(i in 1:n){
    count <- count + 1
    new_theta_proposal <- mvrnorm(n = 1, mu = m, Sigma = proposal_sig)
    previous_theta <- theta[i,]
    acceptance_probability <- dmvnorm(new_theta_proposal, mean = m, sigma = sig) / dmvnorm(m, mean = m, sigma = sig)
    A <- min(1, acceptance_probability)
    if (runif(1) <= A){
      new_theta_proposal <- previous_theta
      count <- count - 1
    }
    theta <- rbind(theta, new_theta_proposal)
    
  }
  newdf <- data.frame(sigma_val = s, eff_size = effectiveSize(theta[,1]), acceptance_ratio = count/n)
  sigdf <- rbind(sigdf, newdf)
  
  
  
  #scatterplot
  #plot(theta[,1], theta[,2])
  
  #traceplot
  #plot(1:10001, theta[,1])
  #lines(1:10001, theta[,1])
  #abline(a = y1, b = 0, col = "blue")
  
  #acf
  #acf(theta[,1], lag.max = 10, type = "correlation", plot = TRUE)
}

#plot(sigdf$sigma_val, sigdf$eff_size)
#lines(sigdf$sigma_val, sigdf$eff_size)

#plot(sigdf$sigma_val, sigdf$acceptance_ratio)
#lines(sigdf$sigma_val, sigdf$acceptance_ratio)

plot(sigdf$eff_size, sigdf$sigma_val, xlab = "effectiveSize", ylab = "sigma^2")
lines(sigdf$eff_size, sigdf$sigma_val)

plot(sigdf$acceptance_ratio, sigdf$sigma_val, xlab = "acceptanceRatio", ylab = "sigma^2")
lines(sigdf$acceptance_ratio, sigdf$sigma_val)

sigdf
