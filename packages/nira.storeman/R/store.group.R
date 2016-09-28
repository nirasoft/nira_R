

  # Header
  # Filename:       store.group.R
  # Description:    This file mainly defines a reference class named STORE.GROUP that Contains
  #                 the data of a number of stores and manages them.
  #                 For more information about the class, read the class documentation.
  # Author:         Nima Ramezani Taghiabadi
  # Email:          nima.ramezani@cba.com.au
  # Start Date:     29 January 2016
  # Last Revision:  14 September 2016
  # Version:        2.2.2
#


# Version History:

# Version   Date               Action
# ----------------------------------
# 2.1.1     29 July 2016       Availability data added. Method get.store() modified to accommodate availability as a new data column
# 2.1.2     30 August 2016     Property nDays Ahead added to sg.settings. Compatible to virtual.store version 4.5.0
# 2.1.3     12 September 2016  Argument 'store.ids' added to method run.optimal()
# 2.2.0     13 September 2016  Method get.store() modified: Specific settings are read from 'spec' table and put in store settings
# 2.2.1     13 September 2016  Method save.settings() added. Saves SEG and Base Stock parameters for each store in the list in a given csv file
# 2.2.2     14 September 2016  Argument 'base' added to method gen.report()


library(timeDate)
library(niragen)
library(nira.timser)


defaultSpecColumns = list(ID = "ID", Capacity = 'Capacity', Initial.Balance = "InitialBalance",
                          Order.Fee.InRoster = "Standard", Order.Fee.OffRoster = "Emergency")

# Definition of Store Group Settings:
#' @exportClass SG.SETTINGS
#' @export SG.SETTINGS
SG.SETTINGS <- setRefClass("SG.SETTINGS", fields = list(
  # Store IDs are supposed to be rownames of the spec table
  capacities = 'numeric',
  costs      = 'matrix',
  weights    = 'matrix',
  roster     = 'matrix',
  lead.time  = 'integer',
  max.off.srv = 'integer',
  nDaysAhead  = 'integer',
  hc.rate    = 'numeric',
  base.stock = 'numeric',
  es.penalty = 'numeric',
  serr.gain  = 'numeric',
  top.up     = 'numeric')  , methods = list(

    initialize = function(row_names = c(), col_names = c(), ...){
      callSuper(...)
      if (!is.empty(row_names)){
        rownames(costs)   <<- row_names
        rownames(weights) <<- row_names
        rownames(roster)  <<- row_names
      }
      if (!is.empty(col_names)){
        colnames(costs)   <<- col_names
        colnames(weights) <<- col_names
        colnames(roster)  <<- col_names
        names(capacities) <<- col_names
      }
    },

    ext.vs.settings = function(sid){
      vss = VS.SETTINGS(
        capacity     = capacities[sid],
        order.capacity = capacities[sid],
        order.fee    = costs[,sid],
        order.weight = weights[,sid],
        order.roster = as.logical(roster[,sid]),
        hc.rate      = hc.rate,
        lead.time    = lead.time,
        max.off.srv  = max.off.srv,
        nDaysAhead   = nDaysAhead,
        es.penalty   = es.penalty,
        serr.gain    = serr.gain,
        top.up       = top.up,
        base.stock   = base.stock)
      names(vss$order.roster) <- names(roster[,sid])
      return(vss)
    },

    embedd.vs.settings = function(vss, sid){
      capacities[sid] <<- vss$capacity
      costs[,sid]     <<- vss$order.fee
      weights[,sid]   <<- vss$order.weight
      roster[,sid]    <<- vss$order.roster
      #hc.rate        <<- vss$hc.rate
      #lead.time      <<- vss$lead.time
      #es.penalty     <<- vss$es.penalty
      #top.up         <<- vss$top.up
      #base.stock     <<- vss$base.stock
    },

    read.roster = function(filename, ...){
      RSTR  = read.csv(filename, as.is = T, row.names = 1, check.names = F)
      dates = char2Time(rownames(RSTR))
      rownames(RSTR) <- time2Char(dates)
      verify(as.matrix(RSTR), names_include = names(roster), varname = filename)
      rows = intersect(rownames(RSTR),rownames(roster))
      cols = intersect(colnames(RSTR),colnames(roster))
      roster[rows, cols] <<- as.matrix(RSTR[rows, cols])
      apply.roster(rost = roster, ...)
    },

    write.roster = function(filename, period = sequence(nrow(roster)), store_ids = colnames(roster), update = F){
      r = roster[period, store_ids, drop = F]
      if (update){RSTR  = read.csv(filename, as.is = T, row.names = 1, check.names = F)
      for (sid in store_ids){RSTR[rownames(r), sid] <- r[, sid]}
      write.csv(RSTR, filename)
      } else {write.csv(r, filename)}
    },

    # argument f_rost is a matrix of logicals nrow = N.store, ncol = 14
    # todo: verfications
    apply.roster.fortnightly = function(time, start, end, f_rost, in_roster_fee, off_roster_fee, in_roster_weight, off_roster_weight){
      ns   = nrow(f_rost)
      sids = rownames(f_rost)
      if (length(in_roster_fee) == 1){in_roster_fee = rep(in_roster_fee, ns)} else {assert(length(in_roster_fee) == ns, "The length of argument 'in_roster_fee' does not match 'f_rost'", match.call()[[1]])}
      if (length(off_roster_fee) == 1){off_roster_fee = rep(off_roster_fee, ns)} else {assert(length(off_roster_fee) == ns, "The length of argument 'off_roster_fee' does not match 'f_rost'", match.call()[[1]])}
      if (length(in_roster_weight) == 1){in_roster_weight = rep(in_roster_weight, ns)} else {assert(length(in_roster_weight) == ns, "The length of argument 'in_roster_weight' does not match 'f_rost'", match.call()[[1]])}
      if (length(off_roster_weight) == 1){off_roster_weight = rep(off_roster_weight, ns)} else {assert(length(off_roster_weight) == ns, "The length of argument 'off_roster_weight' does not match 'f_rost'", match.call()[[1]])}
      names(in_roster_fee) = sids
      names(off_roster_fee) = sids
      names(in_roster_weight) = sids
      names(off_roster_weight) = sids

      for (i in sids){
        vss = ext.vs.settings(i)
        vss$apply.roster.fortnightly(time, start = start, end = end, f_rost = f_rost[i,], in_roster_fee = in_roster_fee[i], off_roster_fee = off_roster_fee[i], in_roster_weight = in_roster_weight[i], off_roster_weight = off_roster_weight[i])
        embedd.vs.settings(vss, i)
      }
    } ,

    apply.roster = function(rost, in_roster_fee, off_roster_fee, in_roster_weight, off_roster_weight){
      verify(rost, c('data.frame','matrix', 'numeric', 'integer', 'logical'), dims = dim(costs), varname = 'rost', names_identical = names(costs), rownames_identical = rownames(costs))
      ns   = ncol(rost)
      sids = colnames(rost)
      if (length(in_roster_fee) == 1){in_roster_fee = rep(in_roster_fee, ns)} else {assert(length(in_roster_fee) == ns, "The length of argument 'in_roster_fee' does not match 'f_rost'", match.call()[[1]])}
      if (length(off_roster_fee) == 1){off_roster_fee = rep(off_roster_fee, ns)} else {assert(length(off_roster_fee) == ns, "The length of argument 'off_roster_fee' does not match 'f_rost'", match.call()[[1]])}
      if (length(in_roster_weight) == 1){in_roster_weight = rep(in_roster_weight, ns)} else {assert(length(in_roster_weight) == ns, "The length of argument 'in_roster_weight' does not match 'f_rost'", match.call()[[1]])}
      if (length(off_roster_weight) == 1){off_roster_weight = rep(off_roster_weight, ns)} else {assert(length(off_roster_weight) == ns, "The length of argument 'off_roster_weight' does not match 'f_rost'", match.call()[[1]])}
      names(in_roster_fee) = sids
      names(off_roster_fee) = sids
      names(in_roster_weight) = sids
      names(off_roster_weight) = sids

      for (i in sids){
        vss = ext.vs.settings(i)
        vss$apply.roster(rost = rost[,i], in_roster_fee = in_roster_fee[i], off_roster_fee = off_roster_fee[i], in_roster_weight = in_roster_weight[i], off_roster_weight = off_roster_weight[i])
        embedd.vs.settings(vss, i)
      }
    },

    forbid_weekends = function(time){
      dow = dayOfWeek(time)
      roster[dow %in% c('Sat', 'Sun'),] <<- NA
    },

    free.roster = function(cost = NULL){
      roster[,] <<- T
      if (!is.null(cost)){costs[,]  <<- cost}
    }
  ))

#' @export
build.spec = function(specset, spec_columns){
  verify(spec_columns, c("list", "character"))

  permit = names(defaultSpecColumns) %in% names(spec_columns)
  assert(sum(!permit) == 0,
         "Argument 'spec_columns' must specify all required fields. These columns are missing: "
         %+% paste(names(defaultSpecColumns[!permit]), collapse = " , "), err_src = match.call()[[1]])
  verify(specset, 'data.frame')
  names.specset = names(specset)

  permit = spec_columns %in% names.specset
  assert(sum(!permit) == 0,
         "Argument 'specset' must contain all the fields specified by argument 'spec_columns'. These columns are missing: "
         %+% paste(spec_columns[!permit], collapse = " , "), err_src = match.call()[[1]])

  names(names.specset) <- names.specset
  names.specset[names.specset[as.character(spec_columns)]] <- names(spec_columns)
  names(specset) <- names.specset
  rownames(specset) <- specset$ID
  return(specset)
}


# Creating a STORE.GROUP class
#' A Reference Class representing a group of stores.
#'
#' @field N.store An integer representing the number of stores in the group
# N.store: integer containing the number of stores in the group
# demand: a time series object containing the demands of all the stores in the group
# order: a time series object containing the orders of all the stores in the group
# spec: a data.frame containing the specifications of each store
#'
#' @export STORE.GROUP
#' @exportClass STORE.GROUP
STORE.GROUP <- setRefClass("STORE.GROUP",
                           fields = list(
                             name              = "character",
                             time.zone         = "character",
                             N.store           = "integer",
                             demand            = "TIME.SERIES",
                             order             = "TIME.SERIES",
                             balance           = "TIME.SERIES",
                             order.return      = "TIME.SERIES",
                             holding.cost      = "TIME.SERIES",
                             order.cost        = "TIME.SERIES",
                             total.cost        = "TIME.SERIES",

                             pred              = "TIME.SERIES",
                             error             = "TIME.SERIES",

                             spec              = "data.frame",
                             report            = "data.frame",
                             availability      = "data.frame",
                             selected          = "character",

                             settings          = "SG.SETTINGS",
                             stores            = 'list'
                           ),

                           methods = list(
                             # The constructor can either get a list of virtual stores or
                             # a list containing spec, demand and orders
                             # the vs_list must be a list. Any other type is not accepted.
                             initialize = function(timeset = NULL, demandset = numeric(), orderset = numeric(), specset = data.frame(),
                                                   balances = NULL, returns = NULL, predset = numeric(), errorset = numeric(), time_format = "%d/%m/%Y", vs_list = list(),
                                                   sg_settings = NULL, spec_columns = defaultSpecColumns, time_zone = "GMT", ...){
                               # todo: verifications

                               callSuper(...)

                               if (!is.null(timeset)){
                                 N.store <<- nrow(specset)
                                 N.int = length(timeset)

                                 timeset   = verify(timeset, c('NULL', 'character', 'factor', 'timeDate', 'Date', 'POSIXt', 'POSIXct', 'POSIXlt'))
                                 demandset = verify(demandset, c('numeric', 'data.frame', 'matrix'), dims = c(N.int, N.store))
                                 orderset  = verify(orderset, c('numeric', 'data.frame', 'matrix'), dims = c(N.int, N.store))
                                 balances  = verify(balances, c('NULL', 'numeric', 'data.frame', 'matrix'), dims = c(N.int, N.store))
                                 returns   = verify(returns, c('NULL', 'numeric', 'data.frame', 'matrix'), dims = c(N.int, N.store))

                                 spec    <<- build.spec(specset, spec_columns)
                                 store.ids = rownames(spec)
                                 demandset = try(demandset[, store.ids, drop = F], silent = T)
                                 verify(demandset, err_msg = "Store IDs do not match: demandset and specset!", err_src = match.call()[[1]])
                                 orderset  = try(orderset[, store.ids, drop = F], silent = T)
                                 verify(demandset, err_msg = "Store IDs do not match: orderset and specset!", err_src = match.call()[[1]])

                                 if (!class(balances) == 'NULL'){
                                   balances = try(balances[, store.ids, drop = F], silent = T)
                                   verify(balances, err_msg = "Store IDs do not match: balances and specset!", err_src = match.call()[[1]])
                                 }
                                 if (!class(returns)  == 'NULL'){
                                   returns = try(returns[, store.ids, drop = F], silent = T)
                                   verify(returns, err_msg = "Store IDs do not match: returns and specset!", err_src = match.call()[[1]])
                                 }

                                 # dfempty <- data.frame.na(nrow = length(timeset), ncol = N.store, col_names = spec[, 'ID'])
                                 time.zone <<- time_zone
                                 tc = time2Char(timeset)

                                 demand  <<- TIME.SERIES(timeset = timeset, dataset = demandset[tc, store.ids], format = time_format, name = 'demand', zone = time.zone, center = time.zone)
                                 order   <<- TIME.SERIES(timeset = timeset, dataset = orderset[tc, store.ids], format  = time_format, name = 'order', zone = time.zone, center = time.zone)

                                 pred    <<- TIME.SERIES(timeset = timeset, dataset = predset[tc, intersect(store.ids, names(predset))],  format = time_format, name = 'demand.pred', zone = time.zone, center = time.zone)
                                 error   <<- TIME.SERIES(timeset = timeset, dataset = errorset[tc, intersect(store.ids, names(predset))], format = time_format, name = 'demand.pred.error', zone = time.zone, center = time.zone)

                                 selected <<- rownames(spec)[1]
                                 reset(balances, returns)

                                 if(class(sg_settings) == "SG.SETTINGS"){settings <<- sg_settings}
                                 else {
                                   settings    <<- new('SG.SETTINGS',
                                                       capacities = spec$Capacity,
                                                       costs   = repeat.row(spec$Order.Fee.InRoster, demand$N.int),
                                                       weights = repeat.row(spec$Order.Fee.OffRoster, demand$N.int),
                                                       roster  = matrix(1, demand$N.int, ncol = N.store), hc.rate = 0.0225/365,
                                                       lead.time = as.integer(1), max.off.srv = as.integer(60), nDaysAhead = as.integer(60), es.penalty   = 10000,  top.up  = 0, base.stock = 0, serr.gain = 1.0,
                                                       row_names = rownames(demand$data), col_names = rownames(spec))
                                 }

                                 clear.stores()
                               } # if timeset is not given an empty object is created
                               # todo: add construction from vs_list from previous versions
                             },

                             remove.stores = function(store_ids = sequence(N.store)){
                               verify(store_ids, 'character', varname = 'store_ids')
                               NC = which(rownames(spec) %in% store_ids)
                               if (length(NC) > 0){spec <<- spec[- NC, , drop = F]}
                               demand$remove.figures(store_ids)
                               balance$remove.figures(store_ids)
                               order$remove.figures(store_ids)
                               order.return$remove.figures(store_ids)
                               holding.cost$remove.figures(store_ids)
                               order.cost$remove.figures(store_ids)
                               total.cost$remove.figures(store_ids)
                               pred$remove.figures(store_ids)
                               error$remove.figures(store_ids)

                               NC = which(colnames(settings$roster) %in% store_ids)
                               if (length(NC) > 0){
                                 settings$costs   <<- settings$costs[, - NC, drop = F]
                                 settings$roster  <<- settings$roster[, - NC, drop = F]
                                 settings$weights <<- settings$weights[, - NC, drop = F]
                               }
                               N.store <<- nrow(spec)

                               for (sid in store_ids){stores[[sid]] <<- NULL}
                             },

                             fortnight.order.counts = function(period = order$stn:order$etn){
                               foc = matrix(nrow = 0, ncol = 14)
                               for (sid in colnames(data$demand)){
                                 FDY    = fday(demand$time)[period]
                                 t      = tabulate(1 + FDY[which(order$data[period, sid] > 0)])
                                 foc = rbind(foc, c(t[-1], rep(0, 14 - length(t)), t[1]))
                               }
                               colnames(foc)   = fdlabel
                               rownames(foc)   = rownames(spec)
                               return(foc)
                             },

                             fit.fortnight.roster = function(period = order$stn:order$etn, threshold = 0.7, forbid_weekends = T){
                               roster = matrix(0, nrow = N.store, ncol = 14)
                               if (forbid_weekends){roster[, c(6,7,13,14)] = NA}
                               colnames(roster) = fdlabel
                               rownames(roster) = rownames(spec)

                               foc = fortnight.order.counts(period = period)

                               for (i in 1:N.store){
                                 ttc = tab.top.cumulative(foc[i,], threshold = threshold)
                                 roster[i, ttc] = 1
                               }
                               return(roster)
                             },

                             fill.stores = function(store_ids = rownames(spec)){
                               assert(store_ids, 'character', range = rownames(spec), varname = 'store_ids')
                               for (sid in store_ids){stores[[sid]] <<- get.store(sid)}
                             },

                             get.store = function(store_id, silent = F, ...){
                               verify(store_id, 'character', range = rownames(spec), varname = 'store_id', lengths = 1)

                               if (!silent){cat(store_id,': Convert to VIRTUAL.STORE object ... ')}

                               config = settings$ext.vs.settings(store_id)

                               # Specific Settings:
                               s1 = spec[ATM_id, 'SEG']
                               s2 = spec[ATM_id, 'Base']
                               if (!is.null(s1)) {if (!is.na(s1)) {config$serr.gain   = s1}}
                               if (!is.null(s2)) {if (!is.na(s2)) {config$base.stock  = s2}}

                               peri   = spec[store_id,'IB.Date.Num']:length(demand$time)

                               config$order.fee   = config$order.fee[peri]
                               config$order.weight = config$order.weight[peri]
                               config$order.roster = config$order.roster[peri]

                               vs = VIRTUAL.STORE(demand$time[peri], demand$data[peri, store_id], initial_balance = spec[store_id, 'Initial.Balance'],
                                                  orderset = order$data[peri, store_id], ID = store_id,
                                                  name = as.character(spec[store_id, 'Name']), vs_settings = config, center = time.zone, ...)
                               if (!inherits(pred$data[peri, store_id], 'NULL') & !inherits(error$data[peri,store_id], 'NULL')){
                                 vs$data$forec = pred$data[peri, store_id]
                                 vs$data$forec[vs$data$forec < 0] <- 0
                                 vs$data$fserr = error$data[peri, store_id]*config$serr.gain
                               }
                               # todo: jump to store group ctn (transfer balances, returns and costs to the get.store) before returning the get.store object
                               # Availability:
                               Ak      <- availability[availability$STOREID == store_id,]
                               dates.A <- as.character(Ak$Date)
                               Ak      <- Ak[!duplicated(dates.A),]
                               dates.A <- unique(dates.A)
                               rownames(Ak) <- dates.A
                               vs$feed(Ak[,'Availability', drop = F])

                               if (!silent){cat('Done ! \n')}
                               return(vs)
                             },

                             # Argument: srv_cap is a single numeric or a numeric vector of 14 elements
                             # and contains the daily order replenishment capacities
                             # (Maximum how many stores can be serviced for each fortnight day)
                             optimal.fortnight.roster = function(hist = sequence(length(demand)), srv_cap = NULL, gain = 1.0, feasible_roster = NULL){
                               # The default value for 'hist' will change later
                               # todo: This function has been customized for special needs of CBA ATM cash optimization project,
                               #       and needs to be more generic (i.e. order.cost and hold.cost.rate can be a time series for each store)
                               # todo: support weekly and monthly and annual rosters
                               # todo: support special days in year

                               verify(feasible_roster, c('matrix', 'numeric', 'logical', 'integer', 'data.frame'), rownames_include = rownames(spec), names_identical = fdlabel, varname = 'feasible_roster')
                               if (is.null(feasible_roster)){
                                 feasible_roster = repeat.row(c(1,1,1,1,1,NA,NA,1,1,1,1,1,NA,NA), N.store)
                                 colnames(feasible_roster) <- fdlabel
                                 rownames(feasible_roster) <- rownames(spec)
                               }

                               feasible_roster <- as.matrix(feasible_roster)
                               feasible_roster[feasible_roster == 0]  <- NA

                               oc = colMeans(settings$costs, na.rm = T)
                               mds  = gain*apply(demand$data[hist, ,drop = F], 2, function(x) high.pass.mean(x, threshold = 10, na.rm = TRUE))
                               sds  = apply(demand$data[hist, ,drop = F], 2, function(x) high.pass.sd(x, threshold = 10, na.rm = TRUE))

                               QSS = sqrt(2*oc*mds/settings$hc.rate)
                               idx = QSS > spec$Capacity
                               QSS[idx]  = spec$Capacity[idx]
                               strength  = spec$Capacity/mds
                               freq      = ceiling((14*mds + 2.0*sqrt(14)*sds)/QSS)
                               freq[is.na(freq)] = 1

                               # freq = QSS/mds
                               #      for (i in 1:N.store){
                               #        if (mds[i] > 0){
                               #          if (freq[i] > 14){freq[i] = 1}
                               #          else if (freq[i] > 7) {freq[i] = 2}
                               #          else if (freq[i] > 4) {freq[i] = 3}
                               #          else if (freq[i] > 3) {freq[i] = 4}
                               #          else {freq[i] = 6}
                               #        } else {freq[i] = 0}
                               #      }

                               if (class(srv_cap) == 'NULL'){
                                 srv_cap = as.integer(2 + sum(freq)/8)
                               }
                               if (length(srv_cap) == 1){srv_cap = rep(srv_cap, 14)} else {assert(length(srv_cap) == 14)}

                               from  = '04-Jan-2010'  # It is a Monday.1
                               until = '17-Jan-2010'  # It is a Sunday.2
                               ts    = timeSequence(from = from, to = until, by = 'day')

                               ord    = order(strength)
                               forca  = rep(1, 14)
                               RSTR   = c()
                               DF     = demand$aggregate.seasonal(hist, func = high.pass.mean, seasonality = 'dof') #*****
                               ordids = rownames(spec)[ord]
                               for (i in sequence(length(ord))){
                                 frost = feasible_roster[ordids[i], ] & forca
                                 m     = ord[i]
                                 u     = distribute.seasonality(ts, season.values = DF[,m, drop = F], seasonality = 'dof')
                                 r     = opt.roster(demands = gain*u, freq = freq[m], q = QSS[m], fr = frost)
                                 indx  = (!is.na(r)) & (r == 1)
                                 srv_cap[indx] = srv_cap[indx] - 1
                                 forca[srv_cap < 1] = NA
                                 r[is.na(r) & !is.na(frost)] = 0
                                 RSTR = rbind(RSTR, r)
                               }
                               colnames(RSTR) = fdlabel
                               rownames(RSTR) = ordids
                               rsms = rowSums(RSTR, na.rm = T)
                               if ((sum(freq[ord] - rsms != 0) > 0) | (sum(rsms == 0) > 0)) {cat('Warning: Some stores may have not received adequate service \n')}
                               return(RSTR)
                             },

                             reset = function(balances = NULL, returns = NULL){
                               report       <<- data.frame()

                               dfempty       <- data.frame.na(nrow = length(demand$time), ncol = N.store, col_names = rownames(spec))
                               if (class(balances) == 'NULL'){
                                 balance      <<- TIME.SERIES(timeset = demand$time, dataset = dfempty, name = "Balance", center = time.zone)
                               } else {
                                 balances = verify(balances, 'data.frame')
                                 assert(nrow(balances) == length(demand$time), "Dimension mismatch!")
                                 assert(ncol(balances) == N.store, "Dimension mismatch!")
                                 balance  <<- TIME.SERIES(timeset = demand$time, dataset = balances, name = "Balance", center = time.zone)
                               }

                               if (class(returns) == 'NULL'){
                                 order.return      <<- TIME.SERIES(timeset = demand$time, dataset = dfempty, name = "Return", center = time.zone)
                               } else {
                                 returns = verify(returns, 'data.frame')
                                 assert(nrow(returns) == length(demand$time), "Dimension mismatch!")
                                 assert(ncol(returns) == N.store, "Dimension mismatch!")
                                 order.return  <<- TIME.SERIES(timeset = demand$time, dataset = returns, name = "Return", center = time.zone)
                               }

                               holding.cost <<- TIME.SERIES(timeset = demand$time, dataset = dfempty, name = "Holding Cost", center = time.zone)
                               order.cost   <<- holding.cost$copy()
                               total.cost   <<- holding.cost$copy()
                               order.cost$name   <<- "Service Cost"
                               total.cost$name   <<- "Total Cost"

                               # for (st in rownames(spec)){balance$data[1,st] <<- spec[st, 'Initial.Balance']}
                             },

                             clear.stores = function(){
                               stores <<- list()
                             },

                             run.optimal = function(start = 1, end = length(demand), fixed_roster = T, silent = T){
                               if (class(start) %in% valid.time.classes){
                                 start <- try(as.timeDate(start))
                                 verify(start, lengths = 1, err_src = match.call()[[1]])
                               } else {
                                 verify(start, c('numeric', 'integer'), lengths = 1, range = c(1, length(demand)), err_src = match.call()[[1]])
                                 start = demand$time[start]
                               }

                               if (class(end) %in% valid.time.classes){
                                 end <- try(as.timeDate(end))
                                 verify(end, lengths = 1, err_src = match.call()[[1]])
                               } else {
                                 verify(end, c('numeric', 'integer'), lengths = 1, err_src = match.call()[[1]])
                                 end = demand$time[end]
                               }

                               for (i in rownames(spec)){
                                 vs  = stores[[i]]
                                 if ('VIRTUAL.STORE' %in% class(vs)){
                                   cat('Run optimization for Store: ', vs$ID, ' started .... \n')
                                   vs$goto(start)
                                   if (vs$now() == start){# can't run if creation time is after start time
                                     vs$reset()
                                     vs$jump.optimal(until = end, fixed_roster = fixed_roster, show = !silent)
                                     cat('Run optimization for Store: ', vs$ID, ' finished! \n')
                                   }
                                 }
                               }
                             },

                             gen.report = function(store.ids = NULL, clear = T, base = NULL){
                               store.ids  = verify(store.ids, 'character', range = names(stores), default = names(stores), varname = 'store.ids')
                               base       = verify(base, 'numeric', range = c(0, Inf), default = settings$base.stock, varname = 'base')
                               rstr  = c()
                               if (clear)(reset())
                               for (id in store.ids){
                                 vs  = stores[[id]]

                                 peri  = vs$stn:vs$ctn
                                 timc  = time2Char(vs$time[peri])
                                 rstr  = rbind(rstr, vs$fortnight.order.counts())

                                 report[id, "HOLD.COST"] <<- vs$data$Hold.Cost[vs$ctn]
                                 report[id, "SRV.COST"]    <<- vs$data$Order.Cost[vs$ctn]
                                 report[id, "TOT.COST"]    <<- vs$data$Total.Cost[vs$ctn]
                                 report[id, "FILL.RATE.BASE"] <<- vs$fill.rate(threshold = base)
                                 report[id, "FILL.RATE.ZERO"] <<- vs$fill.rate(threshold = 1)
                                 report[id, "MIN.BALANCE"] <<- min(vs$data$Balance[peri], na.rm = T)
                                 report[id, "PLANNED"]     <<- sum(vs$settings$order.roster[peri], na.rm = T)
                                 report[id, "CANCELLED"]   <<- sum((vs$data$Order[peri] == 0) & (vs$settings$order.roster[peri]), na.rm = T)
                                 report[id, "EMERGENCY"]   <<- sum((vs$data$Order[peri] > 0) & (!vs$settings$order.roster[peri]), na.rm = T)
                                 order$data[timc, id]   <<- vs$data$Order[peri]
                                 balance$data[timc, id] <<- vs$data$Balance[peri]
                                 order.return$data[timc, id]  <<- vs$data$Return[peri]
                                 holding.cost$data[timc, id]  <<- vs$data$Hold.Cost[peri]
                                 order.cost$data[timc, id]    <<- vs$data$Order.Cost[peri]
                                 total.cost$data[timc, id]    <<- vs$data$Total.Cost[peri]
                               }

                               columns = c(colnames(report), fdlabel)
                               report <<- cbind(report, rstr)
                               colnames(report) <<- columns
                             },

                             save.stores = function(path){
                               for (i in names(stores)){
                                 vs  = stores[[i]]
                                 if ('VIRTUAL.STORE' %in% class(vs)){
                                   write.csv(vs$gen.report(vs$stn:vs$ctn), file = paste0(path, '/', vs$ID, '.csv'), row.names = F)
                                 }
                               }
                             },

                             save.report = function(path, ...){
                               write.csv(report, file  = paste0(path, '/', name, '.report', '.csv'), ...)

                               TC = total.cost$data
                               TC = TC[!(rowSums(!is.na(TC)) == 0),]
                               tc = rownames(TC)

                               write.csv(TC, file = paste0(path, '/', name, '.costs', '.csv'), ...)
                               write.csv(sg$demand$data[tc,, drop = F],  file = paste0(path, '/', name, '.demands', '.csv'), ...)
                               write.csv(sg$balance$data[tc,, drop = F],  file = paste0(path, '/', name, '.balances', '.csv'), ...)
                               write.csv(sg$order$data[tc,, drop = F],  file = paste0(path, '/', name, '.orders', '.csv'), ...)
                               write.csv(sg$order.return$data[tc,, drop = F],  file = paste0(path, '/', name, '.returns', '.csv'), ...)
                             },

                             save.settings = function(settings_fn, store_ids = NULL){
                               store.ids = verify(store_ids, 'character', range = names(stores), default = names(stores), varname = 'store_ids')
                               options(warn = -1)
                               sts       = try(read.csv(settings_fn, row.names = 1, as.is = T), silent = T)
                               options(warn = 0)
                               if (!inherits(sts, 'data.frame')){sts = data.frame()}
                               for (i in store.ids){
                                 sts[i, 'SEG']  = stores[[i]]$settings$serr.gain
                                 sts[i, 'Base'] = stores[[i]]$settings$base.stock
                               }
                               write.csv(sts, file = settings_fn)
                             },

                             save.forecasts = function(forecast_fn, forerror_fn, store_ids = NULL, from_stores = T){
                               store.ids = verify(store_ids, 'character', range = names(stores), default = names(stores), varname = 'store_ids')
                               options(warn = -1)
                               frc       = try(read.csv(forecast_fn, row.names = 1, as.is = T, check.names = F), silent = T)
                               err       = try(read.csv(forerror_fn, row.names = 1, as.is = T, check.names = F), silent = T)
                               options(warn = 0)
                               if (!inherits(frc, 'data.frame')){frc = data.frame()}
                               if (!inherits(err, 'data.frame')){err = data.frame()}

                               tcf = rownames(pred$data)
                               tce = rownames(error$data)
                               for (i in store.ids){
                                 if (from_stores){
                                   a = stores[[i]]$data[, 'forec', drop = F]
                                   e = stores[[i]]$data[, 'fserr', drop = F]/stores[[i]]$settings$serr.gain
                                   names(a) <- i
                                   names(e) <- i
                                   a = a[!is.na(a[,i]), ,drop = F]
                                   e = e[!is.na(e[,i]), ,drop = F]
                                   pred$feed(a)
                                   error$feed(e)
                                 }

                                 frc[tcf, i]  = pred$data[tcf, i]
                                 err[tce, i]  = error$data[tce, i]
                               }
                               write.csv(frc, file = forecast_fn)
                               write.csv(err, file = forerror_fn)
                             },

                             read.forecasts = function(forecast_fn, forerror_fn, store_ids = NULL, to_stores = T){
                               store.ids = verify(store_ids, 'character', range = names(stores), default = names(stores), varname = 'store_ids')
                               options(warn = -1)
                               frc       = try(read.csv(forecast_fn, row.names = 1, as.is = T, check.names = F), silent = T)
                               err       = try(read.csv(forerror_fn, row.names = 1, as.is = T, check.names = F), silent = T)
                               options(warn = 0)
                               assert(inherits(frc, 'data.frame'), 'File ' %+% forecast_fn %+% 'not found or not valid!')
                               assert(inherits(err, 'data.frame'), 'File ' %+% forerror_fn %+% 'not found or not valid!')
                               pred$feed(frc)
                               error$feed(err)

                               if (to_stores){
                                 for (i in store.ids){
                                   a = pred$data[, i, drop = F]
                                   e = error$data[, i, drop = F]
                                   names(a) <- 'forec'
                                   names(e) <- 'fserr'

                                   a = a[!is.na(a[,'forec']), ,drop = F]
                                   e = e[!is.na(e[,'fserr']), ,drop = F]

                                   tce <- rownames(stores[[i]]$data) %^% rownames(e)

                                   stores[[i]]$feed(a)
                                   stores[[i]]$feed(e)

                                   stores[[i]]$data[tce, 'fserr'] <<- stores[[i]]$data[tce, 'fserr']*stores[[i]]$settings$serr.gain
                                 }
                               }
                             },

                             run = function(start = 1, end = length(demand), silent = F){
                               if (class(start) %in% valid.time.classes){
                                 start <- try(as.timeDate(start))
                                 verify(start, lengths = 1, err_src = match.call()[[1]])
                               } else {
                                 verify(start, c('numeric', 'integer'), lengths = 1, range = c(1, length(demand)), err_src = match.call()[[1]])
                                 start = demand$time[start]
                               }

                               if (class(end) %in% valid.time.classes){
                                 end <- try(as.timeDate(end))
                                 verify(end, lengths = 1, err_src = match.call()[[1]])
                               } else {
                                 verify(end, c('numeric', 'integer'), lengths = 1, err_src = match.call()[[1]])
                                 end = demand$time[end]
                               }

                               for (i in names(stores)){
                                 vs  = stores[[i]]
                                 if ('VIRTUAL.STORE' %in% class(vs)){
                                   if (!silent){cat('Run simulation for Store: ', vs$ID, '.... ')}
                                   vs$goto(start)
                                   vs$reset()
                                   vs$goto(end)
                                   if (!silent){cat('Done! \n')}
                                 }
                               }
                             },

                             goto = function(...){
                               for (i in names(stores)){
                                 vs  = stores[[i]]
                                 vs$goto(...)
                               }
                             },

                             recommend.orders = function(...){
                               RO = data.frame()
                               for (i in 1:N.store){
                                 vs  = stores[[i]]
                                 if (inherits(vs, 'VIRTUAL.STORE')){
                                   u = vs$recommend.order(...)
                                   if ("list" %in% class(u)){
                                     RO[vs$ID, 'Date']   = time.to.cahr(u$time)
                                     RO[vs$ID, 'Amount'] = u$amount
                                   }
                                 }
                               }
                               return(RO)
                             },

                             extend.stores = function(...){
                               for (i in names(stores)){
                                 vs  = stores[[i]]
                                 m   = vs$N.int
                                 vs$extend(vs_settings = settings$ext.vs.settings(i), ...)
                               }
                             },

                             extend = function(forecast_demand = T, fort_roster = NULL, extend_stores = F, ...){
                               m = demand$N.int
                               demand$extend(...)
                               balance$extend(...)
                               order$extend(...)
                               order.return$extend(...)
                               holding.cost$extend(...)
                               order.cost$extend(...)
                               total.cost$extend(...)
                               pred$extend(...)
                               error$extend(...)
                               settings$roster  <<- mat.extend(settings$roster, demand$N.int)
                               settings$weights <<- mat.extend(settings$weights, demand$N.int)
                               settings$costs   <<- mat.extend(settings$costs, demand$N.int)
                               new.date.labels = rownames(demand$data)[(m + 1):demand$N.int]
                               rownames(settings$roster)  <<- rownames(demand$data)
                               rownames(settings$weights) <<- rownames(demand$data)
                               rownames(settings$costs)   <<- rownames(demand$data)

                               if (extend_stores){extend.stores(forecast_demand = forecast_demand, ...)}

                               if (!is.null(fort_roster)){
                                 settings$apply.fort.roster(demand$time, start = m + 1, end = demand$N.int, f_rost = fort_roster,
                                                            in_roster_fee = spec$Order.Fee.InRoster, off_roster_fee = spec$Order.Fee.OffRoster,
                                                            in_roster_weight = settings$OrderWeightInRoster, off_roster_weight = settings$OrderWeightOffRoster)
                               }
                             },

                             gglvis.motion = function(period = balance$stn:balance$ctn, store_ids = rownames(spec)){
                               assert(length(period) > 1, "Given period must have more than one element!", err_src = match.call()[[1]])
                               V = TIME.SERIES(center = time.zone)
                               for(k in store_ids){
                                 ST = demand[period, k]
                                 names(ST$data) = 'Demand'
                                 ST$append(balance$data[period, k], 'Balance')
                                 ST$append(holding.cost$data[period, k], 'Holding.Cost')
                                 ST$append(order.cost$data[period, k], 'Order.Cost')
                                 ST$append(total.cost$data[period, k], 'Total.Cost')
                                 ST$ID = k
                                 V = V | ST
                               }
                               V$gglvis.motion(xvar = 'Holding.Cost', yvar = 'Order.Cost', sizevar = 'Total.Cost', colorvar = 'Demand')
                             }
                           ))

#' @export
'[.STORE.GROUP' = function(obj, store_ids){
  if (class(store_ids) %in% c('numeric', 'integer')){store_ids = rownames(obj$spec)[store_ids]}
  verify(store_ids, 'character', range = rownames(obj$spec), varname = 'store_ids')

  vsl = list()
  for (sid in store_ids){
    if (sid %in% names(obj$stores)){vsl = c(vsl, obj$stores[[sid]])} else {
      vs  = obj$get.store(sid)
      vsl = c(vsl, vs)
      obj$stores[[vs$ID]] = vs
    }
  }
  if (length(vsl) == 1){vsl = vsl[[1]]}
  return(vsl)
}
