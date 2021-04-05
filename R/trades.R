#' Get recent trades
#'
#' @description Get recent trades from Coinbase
#'
#' @param coin_pair Currency pair: base - quote

#' @return A data.frame of trades
#' @export
get_trades <- function(coin_pair = "BTC-USD") {
  trades <- jsonlite::fromJSON(paste0("https://api.pro.coinbase.com/products/", coin_pair, "/trades"))
  return(trades)
}

#' Get sample of recent trades
#'
#' @description Get recent trades from Coinbase
#'
#' @param n NUmber of samples
#' @param coin_pair Currency pair: base - quote

#' @return A data.frame of trades
#' @export
get_trades_sample <- function(n = 10, coin_pair = "BTC-USD") {
  btc_trades <- list()
  for (i in 1:n) {
    btc_trades[[i]] <- get_trades(coin_pair)
    Sys.sleep(5)
    print(i)
  }
  btc_trades_all <- dplyr::bind_rows(btc_trades)
  return(btc_trades_all)
}

#' Combine samples of trades
#'
#' @description Combine samples of trades
#'
#' @param trades Set of samples of tradea

#' @return A data.frame of trades
#' @export
combine_trades <- function(trades) {
  btc_trades_all <- dplyr::bind_rows(trades)
  btc_trades_all$time <- lubridate::ymd_hms(btc_trades_all$time)
  btc_trades_all$price <- as.numeric(btc_trades_all$price)
  btc_trades_all$size <- as.numeric(btc_trades_all$size)
  btc_trades_all$value <- btc_trades_all$price * btc_trades_all$size
  btc_trades_all <- btc_trades_all[order(btc_trades_all$time), ]
  return(btc_trades_all)
}

#' Plot trades
#'
#' @description Plot set of samples of trades
#'
#' @param x Trades
#' @return None
#' @export
plot_coin <- function(x) {
  opar <- par(mfrow = c(1, 3))
  hist(x$value, breaks = 50)
  plot(
    x$time,
    x$value * ifelse(x$side == "buy", 1, -1)
  )
  plot(x$time, x$price, "type" = "l")
  par(opar)
}

#' Percent buyers
#'
#' @description Percent buyers
#'
#' @param x Trades
#' @return Percent buyers
#' @export
pc_buy <- function(x) {
  return(sum(x$side == "buy") * 100 / 1000)
}
