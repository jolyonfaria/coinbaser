#' Check Coinbase API is working
#' 
#' @description Check Coinbase API is working
#'
#' @param coinbase_url Url for Coinbase Feed API

#' @return Response from Coinbase or an error message
#' @export
check_cb_api <- function(coinbase_url = "https://api.pro.coinbase.com/products") {
  res <- httr::GET(coinbase_url)
  if (res$status_code != 200) {
    stop("Coinbase API is returning an error.")
  } else {
    return(res)
  }
}

