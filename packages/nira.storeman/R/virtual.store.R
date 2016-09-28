

# Header
# Filename:       virtual.store.R
# Description:    Contains a class for simulating various inventory management strategies on a virtual store
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     22 December 2015
# Last Revision:  02 September 2016
# Version:        4.6.0


# Version History:

# Version   Date            Action
# ----------------------------------
# 4.4.1     05 July 2016    method goto changed name to goto.active() as goto() runs the inactive method of the super class: TIME.SERIES
# 4.4.2     05 July 2016    forec and fserr transferred as extra columns of data
# 4.4.3     05 July 2016    method cost.piuno() changed. It now, subtracts the before-replenishment part of usage on the order date from the previous estimate of balance when estimating the cost
# 4.4.4     05 July 2016    method clearFutureForecasts() added
# 4.4.5     18 July 2016    A logical column named as 'submitted' is added to the data structure showing which orders have been submitted.
#                           Pre-filled by True for all history data and False for calls to set.order() in optimization.
# 4.4.6     18 July 2016    A logical argument named as 'submit' (default False) added to method set.order()
# 4.4.7     18 July 2016    A logical argument named as 'submited_only' (default False) added to method last.order()
# 4.4.8     18 July 2016    Method jump.optimal() changed
# 4.4.9     18 July 2016    method gen.report() eliminated
# 4.4.10    18 July 2016    A character column named as 'Order.Date' is added to the data structure showing the time stamp in which the order is set or submitted.
# 4.4.11    18 July 2016    method set.order() modified and now fills column 'Order.Date'
# 4.4.12    18 July 2016    All data column names now start with capitals and order.return renamed to Return
# 4.4.13    29 July 2016    Adjusted Demand added taking into account the availability data
# 4.4.14    03 August 2016  Method jump.optimal() modified: Orders can never be set on forbidden days.
# 4.4.15    15 August 2016  Method recommend.order() modified: When only_fontial is TRUE, the function does not return NULL if next day is a forbidden day.
#                           This modification is required in association with change made in version 4.4.14
# 4.4.16    15 August 2016  Method next.order.report() modified: Usage includes both the first and next order days and all days between them
#                           respecting ard ratios on the first and next order days.
# 4.4.17    19 August 2016  Some errors in Method cost.piuno() fixed.
# 4.4.18    19 August 2016  In method recommend.orders(), if cost is greater than penalty, the loop breaks and 'j.min' next intervals is selected for the next service
# 4.5.0     30 August 2016  Setting property 'nDaysAhead' added. Specifies number of days to predict ahead each time forecast values are missing. Default is 60.
# 4.5.1     30 August 2016  Methods predict.demand(), predict.demand.serr(), predict.balance() and predict.balance.serr(0 eliminated
# 4.5.1     30 August 2016  Figure 'Demand.Adjusted'renamed to 'Demand.Adj'
# 4.5.2     30 August 2016  Default forecast is defined for figure 'Demand.Adj' as an abstract instance of class TSP.MODEL()
# 4.6.0     02 Spetember 2016 Method updateDemandForecast() added.


# --- --- --- --- --- --- -
#' @export VS.SETTINGS
#' @exportClass VS.SETTINGS
VS.SETTINGS <- setRefClass("VS.SETTINGS", fields = list(
  capacity     = 'numeric',
  order.capacity = 'numeric',
  order.fee   = 'numeric',
  order.weight = 'numeric',
  order.roster = 'logical',
  hc.rate      = 'numeric',
  lead.time    = 'integer',
  max.off.srv  = 'integer',
  nDaysAhead   = 'integer',
  es.penalty   = 'numeric',
  top.up       = 'numeric',
  serr.gain    = 'numeric',
  base.stock   = 'numeric'), methods = list(
    # Ordering Roster Data
    # todo: order.fee may not be flat, support order cost per good
    # todo: support for the other return policy (No return)

    apply.roster.fortnightly = function(time, start, end, f_rost, in_roster_fee, off_roster_fee, in_roster_weight = NA, off_roster_weight = NA){
      # Changes the roster plan, costs and weights based on the given order rules.
      # todo: should change from ctn to N.int, currently changes the roster for all the life time
      # Verifying in_roster_fee:
      assert(in_roster_fee >= 0.0, "Argument 'in_roster_fee' cannot be negative!", err_src = match.call()[[1]])
      peri = start:end
      if (is.na(in_roster_weight)){in_roster_weight = in_roster_fee}
      if (is.na(off_roster_weight)){off_roster_weight = off_roster_fee}
      f_rost     = as.logical(f_rost)
      ord.roster = f_rost
      ord.cost   = in_roster_fee*f_rost
      ord.weight = in_roster_weight*f_rost

      # Emergencies:
      ord.cost[f_rost == 0]   = off_roster_fee
      ord.weight[f_rost == 0] = off_roster_weight

      order.fee[peri]    <<- date.adjust(ord.cost, time[peri])
      order.roster[peri] <<- date.adjust(ord.roster, time[peri])
      order.weight[peri] <<- date.adjust(ord.weight, time[peri])
    },

    apply.roster = function(rost, in_roster_fee, off_roster_fee, in_roster_weight = NA, off_roster_weight = NA){
      # Changes the roster plan, costs and weights based on the given roster.
      # todo: should change from ctn to N.int, currently changes the roster for all the life time
      # Verifying in_roster_fee:
      assert(in_roster_fee >= 0.0, "Argument 'in_roster_fee' cannot be negative!", err_src = match.call()[[1]])
      verify(rost, c("logical", "numeric", "integer"), lengths = length(order.fee), varname = 'rost')
      names(rost) <- names(in_roster_fee)

      if (is.na(in_roster_weight)){in_roster_weight = in_roster_fee}
      if (is.na(off_roster_weight)){off_roster_weight = off_roster_fee}
      rost       = as.logical(rost)
      order.fee   <<- in_roster_fee*rost
      order.weight <<- in_roster_weight*rost

      # Emergencies:
      order.fee[!rost]    <<- off_roster_fee
      order.weight[!rost] <<- off_roster_weight
      order.roster        <<- rost
    },

    get.fortnightly.roster = function(time){
      dof = fday(time)
      x   = data.frame(cost = order.fee, weight = order.weight, roster = order.roster)
      R   = aggregate(x, by = list(f.day = dof), FUN = mean)
      rownames(R) = R[, 1]
      R[,-1]
    },

    get.weekly.roster = function(time){
      dow = dayOfWeek(time)
      x   = data.frame(cost = order.fee, weight = order.weight, roster = order.roster)
      R   = aggregate(x, by = list(week.day = dow), FUN = mean)
      rownames(R) = R[, 1]
      R[,-1]
    },

    forbid.weekends = function(time, cost = NA, weight = NA){
      # assert length(time) == length(order.weight)
      forbidden = dayOfWeek(time) %in% c('Sat', 'Sun')
      order.roster[forbidden] <<- NA
      if (!is.na(cost)){order.fee[forbidden] <<- cost}
      if (!is.na(weight)){order.weight[forbidden] <<- weight}
    }
  ))

default.vs.settings <<- VS.SETTINGS(
  capacity       = 10000,
  order.capacity = Inf,
  hc.rate        = 0.01/365,
  lead.time    = as.integer(1),
  es.penalty   = 10,
  max.off.srv  = as.integer(60),
  nDaysAhead   = as.integer(60),
  serr.gain    = 1.0,
  top.up       = 0,
  base.stock   = 0)


#' @include inv.tools.R

# Creating a VIRTUAL.STORE class
#' A Reference Class representing a store or inventory.
#'
#' @field N.int An integer representing the number of time intervals in the time series
#' @field ctn An integer representing the current time interval number
#' @field stn An integer representing the starting time interval number of the control window
#' @field etn An integer representing the end time interval number of the control window
#' @export VIRTUAL.STORE
#' @exportClass VIRTUAL.STORE
VIRTUAL.STORE <- setRefClass("VIRTUAL.STORE", contains = 'TIME.SERIES',
                             fields = list(
                               address    = "character",
                               ard.ratio  = "numeric",
                               initial.balance = "numeric",
                               settings   = "VS.SETTINGS"
                             ),

                             methods = list(
                               initialize = function(timeset = NULL, demandset = NULL, orderset = NULL, initial_balance = 0, vs_settings = NULL, ard_ratio = NULL, availability = NULL, ...){
                                 # Argument verifications:
                                 timeset   = verify(timeset, valid.time.classes)

                                 if (is.null(timeset) | (length(timeset) == 0)){
                                   callSuper(timeset = NULL, dataset = NULL, ...)
                                 } else {
                                   N  = length(timeset)
                                   # Argument verifications:
                                   settings <<- verify(vs_settings, "VS.SETTINGS", varname = "vs_settings", default = default.vs.settings)

                                   demandset = verify(demandset, c('integer', 'numeric'), range = c(0, 10*settings$capacity), lengths = N, varname = "demandset")
                                   orderset  = verify(orderset,  c('integer', 'numeric'), range = c(0, 20*settings$order.capacity), lengths = N, varname = "orderset") # change it later to 1.0
                                   initial_balance <- verify(initial_balance, c("integer", "numeric"), lengths = 1, range = c(0, 10*settings$capacity), varname = "initial_balnce")
                                   if (is.null(initial_balance)){initial.balance <<- 0} else {initial.balance <<- as.numeric(initial_balance)}

                                   ard_ratio = verify(ard_ratio, c('numeric'), range = c(0, 1), lengths = N)
                                   if (is.null(ard_ratio)){ard.ratio <<- rep(0.5, N)} else {ard.ratio <<- ard_ratio}
                                   if (!is.null(availability)){
                                     assert(length(availability) == N, "Argument 'avaialability' does not match length!" )
                                     data$Availability <<- availability
                                   }
                                   ard.ratio[is.na(ard.ratio)] <<- mean(ard.ratio, na.rm = T)

                                   # Set defaults for optional columns and assign fields:
                                   if (is.null(orderset)){orderset <- rep(0, N)}

                                   dataset = data.frame(
                                     Demand       = demandset,
                                     Order        = orderset,
                                     Order.Date   = rep(NA, N),
                                     Balance      = rep(NA, N),
                                     Return       = rep(NA, N),
                                     Hold.Cost    = rep(NA, N),
                                     Order.Cost   = rep(NA, N),
                                     Total.Cost   = rep(NA, N),
                                     Availability = rep(NA, N),
                                     Demand.Adj = rep(NA, N),
                                     forec        = rep(NA, N),
                                     fserr        = rep(NA, N),
                                     Submitted    = rep(TRUE, N),
                                     stringsAsFactors = F)

                                   callSuper(dataset = dataset, timeset = timeset, ...)

                                   # todo: transfer to the validity method of class VS.SETTINGS
                                   # err.lead.time = make.err.msg("lead_time can not be negative", err_src = match.call()[[1]])
                                   # assert(settings$lead.time >= 0, err.lead.time)
                                   if (is.null(vs_settings)){settings$apply.roster.fortnightly(time, f_rost = c(T,T,T,T,T,T,T,T,T,T,T,T,T,T), in_roster_fee = 0, off_roster_fee = 0, in_roster_weight = 1, off_roster_weight = 1)}
                                   # forecast[['Demand.Adj']] <<- TSP.MODEL(seasonalities = c('moy'))
                                   forecast[['Demand.Adj']] <<- TSP.MODEL(seasonalities = character())
                                   update(1)
                                   adjust.demand()
                                 }
                               },

                               adjust.demand = function(threshold_high = 0.8, threshold_low = 0.2){
                                 missing = which(is.na(data$Availability))
                                 high    = which(data$Availability >= threshold_high)
                                 low     = which((data$Availability >= threshold_low) & (data$Availability < threshold_high))
                                 zero    = which(data$Availability < threshold_low)

                                 s       = aggregate(data$Demand, by = list(DOW = dayOfWeek(time)), FUN = max)
                                 rownames(s) <- s$DOW

                                 data$Demand.Adj[missing] <<- data$Demand[missing]
                                 data$Demand.Adj[high]    <<- data$Demand[high]/data$Availability[high]
                                 for (i in low){
                                   data$Demand.Adj[i]    <<- max(data$Demand[i], min(data$Demand[i]/data$Availability[i], s[dayOfWeek(time[i]), 'x']))
                                 }

                                 for (i in zero){
                                   data$Demand.Adj[i]    <<- max(0.5*data$Demand[i]/threshold_low, s[dayOfWeek(time[i]), 'x'])
                                 }
                               },

                               reset = function(){
                                 stn <<- ctn
                                 if (ctn > 1){
                                   data$Hold.Cost[ctn] <<- data$Balance[ctn]*settings$hc.rate
                                   data$Order.Cost[ctn]   <<- (data$Order[ctn] > 0)*settings$order.fee[ctn]
                                   if (is.na(data$Order.Cost[ctn])){data$Order.Cost[ctn] <<- 0}
                                   data$Total.Cost[ctn]   <<- data$Hold.Cost[ctn] + data$Order.Cost[ctn]
                                 }
                               },

                               # removes missing values from time series
                               remove.missing = function(give_missing = F, ...){
                                 misindex = callSuper(..., give_missing = T)
                                 ard.ratio             <<- ard.ratio[!misindex]
                                 settings$order.fee   <<- settings$order.fee[!misindex]
                                 settings$order.weight <<- settings$order.weight[!misindex]
                                 settings$order.roster <<- settings$order.roster[!misindex]
                               },

                               extend = function(..., period = 86400, vs_settings = NULL, R = NULL, forecast_demand = T, demands = NULL, orders = NULL){
                                 m = N.int
                                 callSuper(period = period, ...)
                                 data$Order[(m + 1):N.int] <<- 0
                                 if (is.null(vs_settings)){
                                   if(is.null(R)){R = settings$get.fortnightly.roster(time = time)}
                                   dof = fday(time)
                                   dof = dof[(m + 1):N.int]
                                   nr  = rep(NA, length(dof)) # new roster
                                   nc  = rep(NA, length(dof)) # new costs
                                   nw  = rep(NA, length(dof)) # new weigthts
                                   for (fd in rownames(R)){
                                     nr[dof == fd ] = R[fd, 'roster']
                                     nw[dof == fd ] = R[fd, 'weight']
                                     nc[dof == fd ] = R[fd, 'cost']
                                   }
                                   settings$order.roster <<- c(settings$order.roster, as.logical(nr))
                                   settings$order.weight <<- c(settings$order.weight, nw)
                                   settings$order.fee   <<- c(settings$order.fee, nc)
                                 } else {
                                   verify(vs_settings, 'VS.SETTINGS', varname = 'settings')
                                   dates = rownames(data)
                                   verify(vs_settings$order.roster, names_include = dates, varname = 'vs_settings$order.roster')
                                   verify(vs_settings$order.fee, names_include = dates, varname = 'vs_settings$order.fee')
                                   verify(vs_settings$order.weight, names_include = dates, varname = 'vs_settings$order.weight')

                                   settings$order.roster[dates]  <<- vs_settings$order.roster[dates]
                                   settings$order.fee[dates]     <<- vs_settings$order.fee[dates]
                                   settings$order.weight[dates] <<- vs_settings$order.weight[dates]
                                 }
                                 ard.ratio             <<- c(ard.ratio, rep(ard.ratio[m], N.int - m))

                                 if (forecast_demand){
                                   forecast.arima(figure = 'Demand.Adj', from = ctn + 1, jumper = N.int - ctn + 2)
                                   peri = (ctn + 1):N.int
                                   tc = as.character(time[peri])
                                   data$forec[peri] <<- forecast[['Demand.Adj']]$pred[tc]
                                   data$fserr[peri] <<- settings$serr.gain*forecast[['Demand.Adj']]$serr[tc]
                                   data$forec[peri][data$forec[peri] < 0] <<- 0
                                 }

                                 peri = (m + 1):N.int

                                 if (sum(is.na(data$forec[peri])) == 0){etn <<- N.int}

                                 if (!is.null(demands)){
                                   verify(demands, c('integer','numeric'), lengths = N.int - m)
                                   assert(feasible(demands), "Given demand values are not feasible! Check for missing values.", err_src = match.call()[[1]])
                                   data$Demand[peri] <<- demands
                                 }

                                 if (!is.null(orders)){
                                   verify(orders, c('integer','numeric'), lengths = N.int - m)
                                   assert(feasible(orders), "Given order values are not feasible! Check for missing values.", err_src = match.call()[[1]])
                                   data$Order[peri] <<- orders
                                 } else {data$Order[peri] <<- 0}
                               },

                               subset = function(period = stn:etn, settings = settings){
                                 if (class(period) %in% c('numeric', 'integer')){sort(period)}
                                 if (is.na(data$Balance[period[1]])){jump(period[1])}
                                 VIRTUAL.STORE(timeset = time[period], demandset = data$Demand[period], orderset = data$Order[period],
                                               initial_balance = data$Balance[period[1]], vs_settings = settings)
                               },

                               set.order = function(amount = NA, N.next = settings$lead.time, persist = TRUE, submit = FALSE){
                                 # If order is set, the day number in which order is set is returned otherwise FALSE is returned
                                 # Check Input Arguments:
                                 err.amount.neg = make.err.msg("Negative value chained to argument amount", err_src = match.call()[[1]])
                                 assert (amount >= 0, err.amount.neg)
                                 if (N.next < settings$lead.time){
                                   print("Warning: N.next is lower than lead_time! Changed to lead_time.")
                                   N.next = settings$lead.time
                                 }

                                 dtn = ctn + N.next # Delivery Time Number
                                 if (dtn > N.int){return(FALSE)}
                                 # Make sure order time is not a restricted date-time:
                                 while (is.na(settings$order.roster[dtn]) & (dtn < N.int) & persist){N.next = N.next + 1}
                                 if (!is.na(settings$order.roster[dtn])){
                                   mx = min(N.int, dtn + settings$lead.time)
                                   data$Order[(ctn + 1):mx] <<- 0
                                   data$Order[dtn]      <<- min(settings$order.capacity, roundto.multiple(amount + settings$top.up, 2000, adjust = 'top'))
                                   data$Submitted[dtn]  <<- submit
                                   data$Order.Date[dtn] <<- as.character(time[ctn])
                                   return(dtn)
                                 } else {return(FALSE)}
                               },

                               fit.fortnight.roster = function(period = stn:ctn, threshold = 0.8, forbid_weekends = T){
                                 roster = rep(0, 14)
                                 if (forbid_weekends){roster[c(6,7,13,14)] = NA}
                                 names(roster) = fdlabel

                                 foc = fortnight.order.counts(period = period)

                                 ttc = tab.top.cumulative(foc, threshold = threshold)
                                 roster[ttc] = 1
                                 # roster[names(roster) %in% ttc] = 1
                                 roster = as.logical(roster)
                                 names(roster) = fdlabel
                                 return(roster)
                               },

                               close.order = function(N.next = 1){
                                 # I tested, only the first line is read/use markdown format
                                 "
                                 Closes all orders \n
                                 \n
                                 Arguments: \n
                                 N.next integer specifies the number of time intervals after the current time interval on which the order must be closed  \n
                                 Returns: closes the order on the specified time interval \n
                                 Example: \n
                                 x$close.order(5) \n
                                 "
                                 dtn = ctn + N.next

                                 err.order.np = make.err.msg("Order is not possible for the specified date", err_src = match.call()[[1]])
                                 assert ( dtn <= N.int, err.order.np)
                                 data$Order[ctn + N.next] <<- amount
                               },

                               fill.rate = function(threshold = settings$base.stock){
                                 threshold = threshold + 0.0001
                                 return(1.0 - sum(data$Balance[stn:ctn] < threshold)/(ctn - stn + 1))
                               },

                               holding.cost  = function(){return(sum(data$Balance[stn:ctn]*settings$hc.rate))},

                               ordering.cost = function(){return(sum((data$Order[stn:ctn] > 0)*settings$order.fee[stn:ctn], na.rm = TRUE))},

                               # Note: interval i-1 must have been updated prior to running this method for i
                               update = function(i){

                                 if (i == 1){
                                   if (data$Order[i] == 0){
                                     data$Balance[i]      <<- max(0, initial.balance - data$Demand[i])
                                     data$Return[i] <<- 0
                                     data$Order.Cost[i]   <<- 0
                                   } else {
                                     stock    = data$Order[i] + initial.balance - data$Demand[i]*(1.0 - ard.ratio[i])
                                     trimmed  = min(stock, settings$capacity)
                                     data$Balance[i]      <<- max(0, trimmed - data$Demand[i]*ard.ratio[i])
                                     data$Return[i] <<- stock - trimmed
                                     data$Order.Cost[i]   <<- settings$order.fee[i]
                                   }
                                   data$Hold.Cost[i] <<- settings$hc.rate*data$Balance[i]
                                 } else{
                                   if (is.na(data$Demand[i])){data$Demand[i] <<- min(data$forec[i], data$Balance[i - 1])}
                                   if (data$Order[i] == 0){
                                     data$Balance[i]      <<- max(0, data$Balance[i - 1] - data$Demand[i])
                                     data$Return[i] <<- 0
                                     data$Order.Cost[i]   <<- data$Order.Cost[i - 1]
                                   } else {
                                     stock    = data$Order[i] + data$Balance[i - 1] - data$Demand[i]*(1.0 - ard.ratio[i])
                                     trimmed  = min(stock, settings$capacity)
                                     data$Balance[i]      <<- max(0, trimmed - data$Demand[i]*ard.ratio[i])
                                     data$Return[i] <<- stock - trimmed
                                     data$Order.Cost[i]   <<- data$Order.Cost[i - 1] + settings$order.fee[i]
                                   }
                                   data$Hold.Cost[i] <<- data$Hold.Cost[i - 1] + settings$hc.rate*data$Balance[i]
                                 }
                                 data$Total.Cost[i]   <<- data$Hold.Cost[i] + data$Order.Cost[i]
                               },

                               cost.piuno = function(Q, N){
                                 # cpiuno: Cost per interval until the next order
                                 # If the store is replenished the next N time intervals (days) with Q amount of goods,
                                 # how much is the cost per time interval(day) from now until the nect N intervals
                                 # According to the cur status of the store, this function returns the expected cost if first replenishment is done N days(intervals) after cur time
                                 # For example if N = 1 it returns the expected cost if replenishment takes place tomorrow
                                 # the cost is estimated per unit time(day) until the first interval after the next replenishment from cur time
                                 # taking property settings$lead.time and settings$order.roster into considerations. If replenishment if not possible after N days NA is returned
                                 if ((N < settings$lead.time) | is.na(settings$order.roster[N + ctn])){return(NA)}
                                 if (N > 1){
                                   FD = data$forec[(ctn+1):(ctn+N-1)]
                                   FE = data$fserr[(ctn+1):(ctn+N-1)]
                                   if (sum(is.na(FD+FE))>0){
                                     updateForecast('Demand.Adj')
                                     res = predictNext(N - 1 + settings$nDaysAhead, 'Demand.Adj', 'arima')
                                     tcx = names(res$pred)
                                     data[tcx, 'forec'] <<- res$pred
                                     data[tcx, 'fserr'] <<- res$serr*settings$serr.gain
                                     data[tcx, 'forec'][data[tcx, 'forec'] < 0] <<- 0
                                     FD = data$forec[(ctn+1):(ctn+N-1)]
                                     FE = data$fserr[(ctn+1):(ctn+N-1)]
                                   }
                                   mu = c()
                                   sd = c()
                                   ss = 0
                                   B  = current('Balance')
                                   for (i in 1:(N-1)){
                                     B  = B - FD[i]
                                     mu = c(mu, B)
                                     ss = ss + FE[i]*FE[i]
                                     sd = c(sd, sqrt(ss))
                                   }
                                   cost.before = (N - 1)*futureCostsPI(N = N - 1, B = current('Balance'), mu = mu, sigma = sd, P = settings$es.penalty, h = settings$hc.rate, base = settings$base.stock)
                                 } else {cost.before = 0}

                                 cost.after   = Q*settings$hc.rate  + settings$order.weight[N + ctn]
                                 return((cost.before + cost.after)/N)
                               },

                               cost.piuno.2 = function(Q, N){
                                 # cpiuno: Cost per interval until the next order
                                 # If the store is replenished the next N time intervals (days) with Q amount of goods,
                                 # how much is the cost per time interval(day) from now until the nect N intervals
                                 # According to the cur status of the store, this function returns the expected cost if first replenishment is done N days(intervals) after cur time
                                 # For example if N = 1 it returns the expected cost if replenishment takes place tomorrow
                                 # the cost is estimated per unit time(day) until the first interval after the next replenishment from cur time
                                 # taking property settings$lead.time and settings$order.roster into considerations. If replenishment if not possible after N days NA is returned
                                 if ((N < settings$lead.time) | is.na(settings$order.roster[N + ctn])){return(NA)}
                                 if (N > 1){
                                   sqn = sequence(N)
                                   FD  = data$forec[ctn + sqn]
                                   FE  = data$fserr[ctn + sqn]
                                   if (sum(is.na(FD+FE))>0){
                                     updateForecast('Demand.Adj')
                                     res = predictNext(N + settings$nDaysAhead, 'Demand.Adj', 'arima')
                                     tcx = names(res$pred)
                                     data[tcx, 'forec'] <<- res$pred
                                     data[tcx, 'fserr'] <<- res$serr*settings$serr.gain
                                     data[tcx, 'forec'][data[tcx, 'forec'] < 0] <<- 0
                                     FD = data$forec[(ctn+1):(ctn+N-1)]
                                     FE = data$fserr[(ctn+1):(ctn+N-1)]
                                   }
                                   mu = c()
                                   sd = c()
                                   ss = 0
                                   B  = current('Balance')
                                   for (i in sequence(N)){
                                     B  = B - (1.0 - ard.ratio[ctn + i])*FD[i]
                                     mu = c(mu, B)
                                     B  = B - ard.ratio[ctn + i]*FD[i]
                                     ss = ss + FE[i]*FE[i]
                                     sd = c(sd, sqrt(ss))
                                   }
                                   cost.before = N*futureCostsPI(N = N, B = current('Balance'), mu = mu, sigma = sd, P = settings$es.penalty, h = settings$hc.rate, base = settings$base.stock)
                                 } else {cost.before = 0}

                                 FDN = data$forec[ctn+N]
                                 if (is.na(FDN)){FDN = predict.demand(N)}
                                 FDN = FDN*ard.ratio[N + ctn]
                                 cost.after   = (Q - FDN)*settings$hc.rate  + settings$order.weight[N + ctn]
                                 return((cost.before + cost.after)/N)
                               },

                               next.order.opportunities = function(N.next){
                                 err.nahead = make.err.msg("Argument 'N.next' must be greater than 'settings$lead.time'" , err_src = match.call()[[1]])
                                 assert(N.next > settings$lead.time, err.nahead)

                                 if (ctn + settings$lead.time > N.int){return(c())}

                                 period = (ctn + settings$lead.time):(min(N.int, ctn + N.next))
                                 flag   = is.na(settings$order.roster[period])
                                 return (period[! flag])
                               },

                               roster.days  = function(from = NA, until = NA){
                                 if (is.na(from)){from = ctn + settings$lead.time}
                                 if (is.na(until)){until = N.int}

                                 if (from > N.int){return(c())}
                                 if (until > N.int){until = N.int}

                                 assert((from > 0) & (until >= from), "Argument 'until' must be after 'from'", match.call()[[1]])

                                 period = from:until
                                 flag   = (settings$order.roster[period] == 0) | (is.na(settings$order.roster[period]))
                                 return (period[! flag])
                               },

                               free.days  = function(from = NA, until = NA){
                                 if (is.na(from)){from = ctn + settings$lead.time}
                                 if (is.na(until)){until = N.int}

                                 if (from > N.int){return(c())}
                                 if (until > N.int){until = N.int}

                                 assert((from > 0) & (until >= from), "Argument 'until' must be after 'from'", match.call()[[1]])

                                 period = from:until
                                 flag   = is.na(settings$order.roster[period])
                                 return (period[! flag])
                               },

                               fortnight.order.counts = function(period = stn:ctn){
                                 FDY    = fday(time)[period]
                                 t      = tabulate(1 + FDY[which(data$Order[period] > 0)])
                                 fc     = c(t[-1], rep(0, 14 - length(t)), t[1])
                                 names(fc) = fdlabel
                                 return(fc)
                               },

                               ggvis.plot = function(period, figure, width = 760, height = 300){
                                 xvar <- prop("x", as.symbol('time'))
                                 yvar <- prop("y", as.symbol(figure))

                                 xvar_name <- 'Date'
                                 yvar_name <- figure

                                 # p <- ggvis(to.data.frame(period, figure), x = xvar, y = yvar, key := ~ID)

                                 p <- ggvis(to.data.frame(period, figure), x = xvar, y = yvar)
                                 p <- layer_points(p, size.hover := 200) %>% scale_datetime("x", nice = "month")
                                 if (figure %in% c('Balance', 'Demand', 'Hold.Cost', 'Order.Cost', 'Total.Cost')){p <- layer_paths(p)}
                                 p <- p %>% add_axis('x', title = xvar_name)  %>%
                                   add_axis('y', title = yvar_name, title_offset = 70)  %>%
                                   set_options(width = width, height = height)
                                 return(p)
                               },

                               goto.active = function(Time){
                                 # Takes you to the given time.num
                                 # The given time.num must be in the future. (More than the cur time.num)
                                 # This function respects the orders and actual demands set for the dates between curent time and the specified time_num in the future
                                 # in calculating the balance
                                 # Check if the given time.num is greater than the cur
                                 ktn = ctn # keep the current time number in ktn
                                 goto(Time)
                                 if (ctn > etn){ctn <<- etn}
                                 if (ktn >= ctn){
                                   data[(ctn + 1):etn,c('Balance', 'Return', 'Hold.Cost', 'Order.Cost', 'Total.Cost')] <<- as.numeric(NA)
                                 } else {
                                   for (i in (ktn + 1):ctn){update(i)}
                                 }
                                 # forecast[['Demand.Adj']]$train(time[2:ctn], data$Demand[2:ctn], model_type = 'mean')
                                 # todo: write method update() to fasten this process
                               },

                               jump.active = function(N_int = 1){
                                 # jumps to the next <<N.int>> interval
                                 goto.active(ctn + N_int)
                               },

                               #     predict.demand = function(peri){
                               #       peri.ext = (ctn + 1):min(ctn + max(peri) + settings$nDaysAhead, N.int)
                               #       times    = time[peri.ext]
                               #       if (class(forecast[['Demand.Adj']]) == 'NULL'){forecast[['Demand.Adj']] <<- TSP.MODEL(seasonalities = 'moy')}
                               #       f      = forecast[['Demand.Adj']]
                               #       demdat = data$Demand.Adj[sequence(ctn)]
                               #       findex = which(feasible(demdat))
                               #
                               #       assert(length(findex) > 7, "Requires at least seven history values for demand!", err_src = match.call()[[1]])
                               #       cat('Predicting usage from ', as.character(times[1]), ' until ',  as.character(times[length(times)]), ' ... ')
                               #       if (!identical(f$train.time, time[findex])){
                               #         f$train(time = time[findex], data = demdat[findex])
                               #       }
                               #       res = f$predict(times)
                               #       data$forec[peri.ext] <<- res$pred
                               #       data$fserr[peri.ext] <<- settings$serr.gain*res$serr
                               #       data$forec[peri.ext][data$forec[peri.ext] < 0] <<- 0
                               #       cat('Done! \n')
                               #       return(data$forec[ctn + peri])
                               #     },
                               #
                               #     predict.demand.serr = function(peri){
                               #       if (sum(is.na(data$fserr[ctn + peri])) > 0){predict.demand(peri)}
                               #       return(data$fserr[ctn + peri])
                               #     },

                               updateDemandForecast = function(){
                                 updateForecast('Demand.Adj')
                                 res = predictNext(settings$nDaysAhead, 'Demand.Adj', 'arima')
                                 tcx = names(res$pred)
                                 data[tcx, 'forec'] <<- res$pred
                                 data[tcx, 'fserr'] <<- res$serr*settings$serr.gain
                                 data[tcx, 'forec'][data[tcx, 'forec'] < 0] <<- 0
                               },


                               next.order.report.1 = function(N1, N2){
                                 # if you want to do the next order the next N1 days:
                                 # 1- how much should you replensih if the next replenishment is N2 days after this order
                                 # 2- how much is the daily cost until the next replenishment after this order (N1 + N2 days ahead)
                                 FD  = data$forec[(ctn+N1):(ctn+N1+N2-1)]
                                 FE  = data$fserr[(ctn+N1):(ctn+N1+N2-1)]
                                 if (sum(is.na(FD+FE))>0){
                                   updateForecast('Demand.Adj')
                                   res = predictNext(N1 + N2 + settings$nDaysAhead, 'Demand.Adj', 'arima')
                                   tcx = names(res$pred)
                                   data[tcx, 'forec'] <<- res$pred
                                   data[tcx, 'fserr'] <<- res$serr*settings$serr.gain
                                   data[tcx, 'forec'][data[tcx, 'forec'] < 0] <<- 0
                                   FD  = data$forec[(ctn+N1):(ctn+N1+N2-1)]
                                   FE  = data$fserr[(ctn+N1):(ctn+N1+N2-1)]
                                 }
                                 QQ  = sum(FD[1:N2]) + settings$base.stock + 3.0*sqrt(sum(FE[1:N2]^2))
                                 out = list(amount = QQ, cost = (sum((QQ - cumulative(FD[1:N2]))*settings$hc.rate) + settings$order.weight[ctn + N1 + N2])/N2)
                                 return(out)
                               },

                               next.order.report = function(N1, N2){
                                 # if you want to do the next order the next N1 days:
                                 # 1- how much should you replensih if the next replenishment is N2 days after this order
                                 # 2- how much is the daily cost until the next replenishment after this order (N1 + N2 days ahead)
                                 assert(N2 > 0)
                                 rr  = c(ard.ratio[ctn+N1], rep(1.0, N2 - 1), 1.0 - ard.ratio[ctn+N1+N2])
                                 FD  = rr*data$forec[(ctn+N1):(ctn+N1+N2)]
                                 FE  = rr*data$fserr[(ctn+N1):(ctn+N1+N2)]
                                 if (sum(is.na(FD+FE))>0){
                                   updateForecast('Demand.Adj')
                                   res = predictNext(N1 + N2 + settings$nDaysAhead, 'Demand.Adj', 'arima')
                                   tcx = names(res$pred)
                                   data[tcx, 'forec'] <<- res$pred
                                   data[tcx, 'fserr'] <<- res$serr*settings$serr.gain
                                   data[tcx, 'forec'][data[tcx, 'forec'] < 0] <<- 0
                                   FD  = rr*data$forec[(ctn+N1):(ctn+N1+N2)]
                                   FE  = rr*data$fserr[(ctn+N1):(ctn+N1+N2)]
                                 }
                                 QQ  = sum(FD) + settings$base.stock + 3*sqrt(sum(FE^2))
                                 out = list(amount = QQ, cost = (sum((QQ - cumulative(FD))*settings$hc.rate) + settings$order.weight[ctn + N1 + N2])/N2)
                                 return(out)
                               },

                               optimal.order.amount = function(N){
                                 cost.min = Inf
                                 stay     = (ctn + N <= N.int)  # must be True
                                 err.N = make.err.msg("No data for the next 'N' intervals!", err_src = match.call()[[1]])
                                 assert(stay, err.N)
                                 i        = 0
                                 optim.Q  = sqrt(2*mean(settings$order.fee, na.rm = T)*mean(data$Demand, na.rm = T)/settings$hc.rate)
                                 while ((stay) & (ctn + N + i < N.int)){
                                   i   = i + 1
                                   res = next.order.report(N, i)
                                   if (!is.na(settings$order.roster[ctn + N + i])){
                                     # cost = (i*res$cost + settings$order.weight[ctn + N + i])/(i + 1)
                                     if (cost.min > res$cost){
                                       cost.min = res$cost
                                       optim.Q  = res$amount
                                       n.raise  = 0
                                     } else {
                                       n.raise = n.raise + 1
                                       stay = n.raise < 15
                                     }
                                   }
                                 }
                                 return(optim.Q)
                               },

                               minimum.order.amount = function(N){
                                 nin = N.int
                                 if (ctn + N + settings$lead.time > nin){return(settings$order.capacity)}

                                 if (is.na(settings$order.roster[ctn + N])){return(NA)} # Is the next N-th day an order day?
                                 else{
                                   nop = which(!is.na(settings$order.roster[(ctn + N + settings$lead.time):N.int]))[1]
                                   if (is.na(nop)){res = next.order.report(N, N.int - ctn - N + 1)} else {res = next.order.report(N, nop + settings$lead.time - 1)}
                                 }
                                 return (res$amount)
                               },

                               required.order.amount = function(N1, N2){
                                 # If replenish the next N1 days, how much should be ordered so that next order is N2 days after that
                                 if (ctn + N1 + settings$lead.time > N.int){return(50000)}
                                 if (ctn + N1 + N2 > N.int){N2 = N.int - ctn - N1}

                                 # Is the next N1 days an order day?
                                 if (is.na(settings$order.roster[ctn + N1])){return(NA)}
                                 else {res = next.order.report(N1, N2)}
                                 return (res$amount)
                               },

                               run.forecast = function(from = 1, jumper = 10, model_type = 'mean'){
                                 assert(jumper >= 1)
                                 f = forecast[['Demand.Adj']]
                                 f$reset()
                                 i = from
                                 while (i < N.int){
                                   j = i + jumper
                                   if (j > N.int){j = N.int}
                                   f$train(time = time[1:i], data = data$Demand.Adj[1:i], model_type = model_type)
                                   res = f$predict(time[(i + 1):j], model_type = model_type)
                                   i   = j
                                 }
                               },

                               last.order = function(global = F, submitted_only = F){
                                 "Returns the time number of the last time in which the store was serviced (replenished)"
                                 if (global){prd = sequence(N.int)} else {prd = sequence(ctn)}
                                 fbl = feasible(data$Order[prd], exclude = 0)
                                 if (submitted_only){fbl = fbl & data$Submitted[prd]}
                                 w   = which(fbl)
                                 if (is.empty(w)) {return(0)} else {return(w[length(w)])}
                               },

                               next.order = function(submitted_only = F){
                                 "Returns the time number of the next time in which the store will be serviced (replenished)"
                                 prd = (ctn + 1):N.int
                                 fbl = feasible(data$Order[prd], exclude = 0)
                                 if (submitted_only){fbl = fbl & data$Submitted[prd]}
                                 w   = which(fbl)
                                 if (is.empty(w)) {return(0)} else {return(ctn + w[1])}
                               },

                               # only_fontial: only for next time interval after lead time
                               recommend.order = function(fixed_roster = FALSE, only_fontial = T){
                                 # SECE: Sequential Expected Cost Evaluation is a new method for optimal replenishment order planning
                                 # for store management. It recommends how much and in how many intervals in the future is optimal to replenish the store
                                 mu  = mov.avr('Demand')
                                 lst = last.order()

                                 alt = settings$lead.time  # actual lead time
                                 while (is.na(settings$order.roster[ctn + alt]) & (ctn + alt < N.int)){alt = alt + 1}

                                 if    (is.na(settings$order.roster[ctn + alt]) & (ctn + alt == N.int)){ # No more order opportunities left
                                   # goto(N.int)
                                   return(NA)
                                 }

                                 j         = alt
                                 cost.min  = Inf
                                 j.min     = 0
                                 stay      = (mu > 0)
                                 n.raise   = 0
                                 while (stay){
                                   cost = cost.piuno(Q = settings$order.capacity, N = j) + settings$es.penalty*(j + ctn > settings$max.off.srv + lst)
                                   if (!is.na(cost)){
                                     if (cost < cost.min){
                                       j.min    = j
                                       cost.min = cost
                                       n.raise  = 0
                                       if (only_fontial & (j.min > alt) & !is.na(settings$order.roster[ctn + 1])){return(NULL)}
                                     } else{
                                       n.raise = n.raise + 1
                                       stay    = n.raise < 15
                                     }
                                   }
                                   j    = j + 1
                                   stay = stay & (j + ctn < N.int) & (cost.min < settings$es.penalty)
                                 }

                                 flag = (mu > 0) & (j.min > 0)
                                 if ((flag) & (n.raise == 0)){
                                   sd    = mov.sd('Demand')
                                   ecinr = j.min*futureCostPI(N = j.min, B = current('Balance'), mu = mu, sigma = sd, P = settings$es.penalty, h = settings$hc.rate, base = settings$base.stock)
                                   if (j.min > 1){
                                     ecir  = (j.min - 1)*futureCostPI(N = j.min - 1 , B = current('Balance'), mu = mu, sigma = sd, settings$es.penalty, settings$hc.rate, base = settings$base.stock)
                                   } else {ecir = 0}
                                   Qs    = optimal.order.amount(j.min)
                                   ecir  = ecir + (Qs - mu)*settings$hc.rate + settings$order.weight[ctn + j.min]
                                   flag  = (ecinr > ecir)
                                 }
                                 # checks future orders
                                 if (alt > 1) {flag = flag & (sum(data$Order[(ctn + 1):(ctn + alt - 1)]) == 0)}
                                 if (flag){
                                   if (fixed_roster){
                                     # This code is written for very special case when ordering is permitted but a certain roster
                                     # in which the order costs are minimum is preferred. While replenishment in other days are permitted,
                                     # the system tries to choose the order amount so that the next replenishment falls on the cheapest
                                     # days or days in the roster

                                     a = roster.days(from = ctn + j.min + settings$lead.time, until = N.int)
                                     if (length(a) == 0){Qss = round(optimal.order.amount(j.min), -1)} else {
                                       counter = 1
                                       Q.opt   = min(optimal.order.amount(j.min), settings$order.capacity)
                                       Qss     = required.order.amount(j.min, min(settings$max.off.srv, a[1] - ctn - j.min))

                                       if (Qss - settings$base.stock - 3*sqrt(sum(data$fserr[(ctn + 1):a[1]]^2)) > settings$order.capacity){
                                         a   = free.days(from = ctn + j.min + settings$lead.time, until = N.int)
                                         if (length(a) == 0){Qss = round(optimal.order.amount(j.min), -1)}
                                         else {Qss = required.order.amount(j.min, min(settings$max.off.srv, a[1] - ctn - j.min))}
                                       }
                                       best.Q  = Qss
                                       while((Qss <= Q.opt) & (counter < length(a))) {
                                         best.Q  = Qss
                                         counter = counter + 1
                                         Qss     = required.order.amount(j.min, min(settings$max.off.srv, a[counter] - ctn - j.min))
                                       }
                                       Qss = best.Q
                                     }
                                   } else {Qss = optimal.order.amount(j.min)}
                                   if ((is.na(Qss)) | (Qss <= 0)){Qss = settings$order.capacity}
                                   return(list(N.next = j.min, time = time[ctn + j.min], amount = Qss))
                                 }
                                 return(NA)
                               },

                               jump.optimal = function(until = NULL, fixed_roster = FALSE, show = TRUE, forecast_demand = F, update_forecast = F){
                                 if (is.null(until)){until = etn}
                                 end.time.num = time.number(until)
                                 reset()

                                 data$Order[(ctn + settings$lead.time):end.time.num] <<- 0

                                 eval = (ctn + 1):N.int
                                 if (forecast_demand & !no.missing(data$forec[eval]) & !no.missing(data$fserr[eval])){

                                   hist = sequence(ctn)
                                   cat('Predicting usage from ', as.character(time[eval[1]]), ' until ',  as.character(time[max(eval)]), ' ... ')
                                   forecast[['Demand.Adj']] <<- TSP.MODEL(seasonalities = c('moy'))
                                   # forecast[['Demand.Adj']] <<- TSP.MODEL(seasonalities = character())
                                   forecast[['Demand.Adj']]$train(time[hist], data$Demand.Adj[hist], model_type = 'arima')
                                   res = forecast[['Demand.Adj']]$predict(time = time[eval], model_type = 'arima')

                                   tc = time2Char(time[eval])

                                   data$forec[eval] <<- forecast[['Demand.Adj']]$pred[tc]
                                   data$fserr[eval] <<- settings$serr.gain*forecast[['Demand.Adj']]$serr[tc]
                                   data$forec[eval][data$forec[eval] < 0] <<- 0
                                   cat('Done ! \n')
                                   update_forecast = F
                                 }

                                 # data$Order[(ctn + 1):end.time.num] <<- 0
                                 while (ctn < end.time.num){
                                   if (show){cat('Int.No:',ctn,'Time:', as.character(now()),'Bal:',current('Balance'),'\n')}
                                   if (!is.na(settings$order.roster[ctn])){
                                     if (update_forecast){updateDemandForecast()}
                                     res = recommend.order(fixed_roster = fixed_roster, only_fontial = T)
                                     if (class(res) == 'list'){set.order(amount = res$amount, N.next = res$N.next)}
                                   }
                                   jump.active()
                                 }
                               },

                               jump.optimal.1 = function(until = NULL, fixed_roster = FALSE, show = TRUE, forecast_demand = F){
                                 if (is.null(until)){until = etn}
                                 end.time.num = time.number(until)
                                 reset()

                                 data$Order[(ctn + settings$lead.time):end.time.num] <<- 0

                                 eval = (ctn + 1):N.int
                                 if (forecast_demand & !no.missing(data$forec[eval]) & !no.missing(data$fserr[eval])){

                                   hist = sequence(ctn)
                                   cat('Predicting usage from ', as.character(time[eval[1]]), ' until ',  as.character(time[max(eval)]), ' ... ')
                                   forecast[['Demand.Adj']] <<- TSP.MODEL(seasonalities = c('moy'))
                                   forecast[['Demand.Adj']]$train(time[hist], data$Demand.Adj[hist], model_type = 'arima')
                                   res = forecast[['Demand.Adj']]$predict(time = time[eval], model_type = 'arima')

                                   tc = time2Char(time[eval])

                                   data$forec[eval] <<- forecast[['Demand.Adj']]$pred[tc]
                                   data$fserr[eval] <<- settings$serr.gain*forecast[['Demand.Adj']]$serr[tc]
                                   data$forec[eval][data$forec[eval] < 0] <<- 0
                                   cat('Done ! \n')
                                 }

                                 # data$Order[(ctn + 1):end.time.num] <<- 0
                                 while (ctn < end.time.num){
                                   if (show){cat('Int.No:',ctn,'Time:', as.character(now()),'Bal:',current('Balance'),'\n')}
                                   res = recommend.order(fixed_roster = fixed_roster, only_fontial = T)
                                   if (class(res) == 'list'){set.order(amount = res$amount, N.next = res$N.next)}
                                   jump.active()
                                 }
                               },

                               close.all.orders = function(){
                                 # Closes all orders
                                 data$Order[(ctn+1):N.int] <<- 0
                               },

                               # Accessor functions:

                               str = function(){
                                 paste("\n",
                                       "Time Number :", ctn, "\n",
                                       "Time        :", now(), "\n",
                                       "Balance     : $", current('Balance'), "\n",
                                       "Total Cost  : $", round(holding.cost() + ordering.cost(),2), "\n",
                                       "Order Amount: $", data$Order[ctn], "\n",
                                       "Return      : $", data$Return[ctn], "\n")
                               },

                               clearFutureForecasts = function(){
                                 if (ctn < N.int){
                                   data$forec[(ctn + 1):N.int] <<- NA
                                   data$fserr[(ctn + 1):N.int] <<- NA
                                 }
                               }

                             ))

# Generic Functions
print.VIRTUAL.STORE = function(obj){
  cat(obj$str())
}


# coercion method:
setAs("VIRTUAL.STORE", "data.frame", function(from){
  return(from$to.data.frame())
})
