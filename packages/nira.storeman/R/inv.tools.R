

  # Header
  # Filename:      inv.tools.R
  # Description:   Contains various functions including some
  #                inventory cost prediction models and optimal roster planners.
  # Author:        Nima Ramezani Taghiabadi
  # Email :        N.RamezaniTaghiabadi@uws.edu.au
  # Start Date:    13 January 2016
  # Last Revision: 15 July 2016
  # Version:       4.3.1

# Version History:
#
# Version    Date          Actions
# ----------------------------------------------------
# 4.3.1      15 July 2016  function opt.roster() exported

#' @export
futureCostPI <- function (N, B, mu, sigma, P, h, base = 0) {
  # Given stochastic usage mean and standard deviation and associated costs, predicts future cost per unit time for a desired
  # replenishment amount after a given amount of time
  # Input 1: N Number of time intervals ahead from now until the replenishment
  # Input 2: B Current opening balance or Current amount of inventory (cash in the ATM) (Only used if argument replenish = FALSE)

  p     = rep(0,N)
  f     = rep(0,N)
  s.penalty = 0
  s.holding = 0

  for (i in 1:N){
    mu.i    = B - i*mu
    sigma.i = sqrt(i)*sigma
    z0      = (base - mu.i)/sigma.i

    f[i] = pnorm(z0)
    if (i == 1){
      p[i] = f[i]
    } else{
      p[i] = f[i] - f[i-1]
    }
    s.penalty = s.penalty + (N - i + 1)*p[i]
    s.holding = s.holding + i*p[i]
  }

  mu.N    = B - N*mu
  sigma.N = sqrt(N)*sigma
  z0      = (base - mu.N)/sigma.N
  PHI     = pnorm(z0)
  phi     = exp(- 0.5*z0*z0)/sqrt(2*pi)

  I       = (B + mu.N)*(1 - PHI) + sigma.N*phi

  cost.N  = P*s.penalty + 0.5*h*B*s.holding + 0.5*N*h*I
  cost    = cost.N/N
  return(cost)
}

#' @export
futureCostsPI <- function (N, B, mu, sigma, P, h, base = 0) {
  # Given stochastic usage mean and standard deviation and associated costs, predicts future cost per unit time for a desired
  # replenishment amount after a given amount of time
  # Input 1: Q - The amount of replenishment (Only used if argument replenish is TRUE)
  # Input 2: N - number of time intervals in the future from now for prediction
  # Input 3: B - cur opening balance or cur amount of inventory (cash in the ATM) (Only used if argument replenish = FALSE)

  assert(length(mu) == N, "Error")
  assert(length(sigma) == N, "Error")

  p     = rep(0,N)
  f     = rep(0,N)
  s.penalty = 0
  s.holding = 0

  for (i in 1:N){
    z0      = (base - mu[i])/sigma[i]

    f[i] = pnorm(z0)
    if (i == 1){
      p[i] = f[i]
    } else{
      p[i] = f[i] - f[i-1]
    }
    s.penalty = s.penalty + (N - i + 1)*p[i]
    s.holding = s.holding + i*p[i]
  }

  # mu.N    = mu[N]
  # sigma.N = sqrt(sum(sigma[1:N]^2))

  # z0      = (base - mu.N)/sigma.N
  PHI     = pnorm(z0)
  phi     = exp(- 0.5*z0*z0)/sqrt(2*pi)

  I       = (B + mu[N])*(1 - PHI) + sigma[i]*phi

  cost.N  = P*s.penalty + 0.5*h*B*s.holding + 0.5*N*h*I
  cost    = cost.N/N
  return(cost)
}

# fr : 1  1  NA  NA  1  0  NA
# a  : 1  0  0   1   1  0  1
# out: 1  0  NA  NA  0  1  NA
int.to.roster = function(x, fr){
  a = as.integer(intToBits(x))
  y = fr
  y[!is.na(fr)] = a[sequence(length(y[!is.na(fr)]))]
  return(y)
}

# roster.to.int

roster.cost = function(r, q = NA, demands = rep(1.0, 14)){
  r = as.logical(r)
  s = numeric()
  j = 1
  for (i in 1:14){
    if (!is.na(r[i])){
      if (r[i]) {j = j + 1}
    }

    s[j] = add(s[j], demands[i])
  }

  s[1] = add(s[1], s[j])
  s    = s[-j]
  # if (is.na(q)){return(sd(s))} else {return(sum((s - q) > 0)*1000000 + mean((q - s)^2))}

  if (is.na(q)){return(sd(s))} else {
    cost = mean((q - s)^2) + roster.cost.sim(r, q, demands = demands)
    return(cost)
  }
}


# r: fortnight roster
# p: pernalty
# q: order(replenishment) amount
roster.cost.sim = function(r, q, p = 1000*q, oc = 133, hcr = 0.0225/365, demands = rep(1.0, 14)){
  r[is.na(r)] <- 0
  assert(sum(r) > 0, "Given roster adds up to zero")
  r     = as.logical(r)
  st    = which(r)[1]
  b     = numeric()

  b[st] = q - demands[st]
  if (st < 14){for (i in (st + 1):14){b[i] = ifelse(r[i], q, b[i - 1]) - demands[i]}}
  if (st > 1) {b[1] = ifelse(r[1],q, b[14]) - demands[1]}
  if (st > 2) {for (i in 2:(st - 1)) {b[i] = ifelse(r[i], q, b[i - 1]) - demands[i]}}
  b[b < 0] = p
  cost     = sum(b*hcr) + sum(r*oc)
  return(cost)
}

#' @export
opt.roster = function(demands, freq = 2, q = NA, fr = c(1,1,1,1,1,NA,NA,1,1,1,1,1,NA,NA)){
  N   = 2^length(fr[!is.na(fr)]) - 1
  cst = numeric()
  RST = numeric()
  for (i in sequence(N)){
    rst    = int.to.roster(i, fr)
    if (is.na(freq)){
      cst = c(cst, roster.cost(rst, q = q, demands = demands))
      RST = rbind(RST, rst)
    } else {
      if (sum(rst, na.rm = T) == freq){
        cst = c(cst, roster.cost(rst, q = q, demands = demands))
        RST = rbind(RST, rst)
      }
    }
  }
  if (length(cst) < 1){return(rep(NA, 14))} else {
    min.i = order(cst)[1]
    return(RST[min.i,])
  }
}

