#' nira.storeman: This wonderul package is an ideal tool-box for working with stores.
#'
#' @section Class VIRTUAL.STORE:
#' This R reference class contains multiple fields and methods that help to:
#' 1 - simulate inventory given actual demand and orders
#' 2 - Forecast the demand according to the history data considering trands and seasonalities
#' 3 - Manage and control inventory balance and base stock level
#' 4 - Optimize orders to minimize the overall holding and order cost
#'
#' @section Class STORE.GROUP:
#' This R reference class contains multiple fields and methods for managing a group of virtual stores
#'
#' @docType package
#' @name nira.storeman
#'
#' @import timeDate
#' @import niragen
#' @import nira.timser
#' @importClassesFrom nira.timser TSP.MODEL TIME.SERIES

#' @include inv.tools.R
#' @include virtual.store.R
#' @include opt.rost.plan.R
#' @include store.group.R

# Start Date:     22 December 2015
# Last Revision:  14 September 2016
# Version:        4.4.2

# Version History:

# Version   Date              Action
# ----------------------------------
# 4.3.2     29 July 2016      store.group.R changed to ver 2.1.1
# 4.3.3     02 August 2016    store.group.R changed to ver 2.1.1
# 4.4.0     29 July 2016      virtual.store.R changed to ver 4.4.19
# 4.4.1     02 September 2016 virtusl.store.R changed to ver 4.6.0
# 4.4.2     14 September 2016 store.group.R changed to ver 2.2.2



NULL
#> NULL
