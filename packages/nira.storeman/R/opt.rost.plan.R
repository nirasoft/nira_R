

  # Header
  # Filename:      opt.rost.plan.R
  # Description:   This module contains tools to plan an optimal seasonal roster for store replenishments
  #                according to the given seasonal usage
  # Author:        Nima Ramezani Taghiabadi
  # Email :        N.RamezaniTaghiabadi@uws.edu.au
  # Start Date:    17 January 2016
  # Last Revision: 19 February 2016
  # Version:       1.2.0

# Version History:

# Version   Date                Action
# -------------------------------------
# 1.2.0     19 February 2016    function roster.cost() changed. Roster cost is now computed in a different way

opt.bin.split = function(v, free = rep(1, length(v))){
  N = length(v)
  r.min = 1
  for (i in 2:N){
    nom = sum(v[i:N])
    den = sum(v[1:(i-1)])
    permit = (den > 0) & free[i]
    if (permit){
      r = abs(nom/den - 1)
      if (r < r.min) {
        r.min = r
        i.min = i
      }
    }
  }
  return(i.min)
}

#  r: vector of length 14: given roster
# fr: vector of length 14: Any feasible fortnightly roster
is.feasible = function(r, fr){
  sum(r[is.na(fr)], na.rm = T) == 0
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

