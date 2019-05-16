#' getBalance Get Eth addr balance
#'
#' @param eth_addr
#'
#' @return Eth addr balance
#' @export
#' @import httr
#' @import jsonlite
#'
#'
getBalance = function(eth_addr){
  eth_account_url = paste0("http://api.etherscan.io/api?module=account&action=balance&address=",
                           eth_addr,
                           "&tag=latest&apikey=D1Z8Y7PYHKHS2IWCG98224MXQ9PU78IDKF")
  httr::set_config(config(ssl_verifypeer = 0L))
  balance.raw = as.numeric(fromJSON(eth_account_url)$result)
  balance.real = balance.raw/ 10e+17
  balance.real
}

#' getPriceCny Get latest eth price in CNY
#'
#' @return latest eth price in CNY
#' @export
#' @import jsonlite
#'
#'
getPriceCny = function(){
  eth_price_url = "https://api.coinmarketcap.com/v1/ticker/ethereum/?convert=CNY"
  eth_price = as.numeric(fromJSON(eth_price_url)$price_cny)
  eth_price
}

#' getPriceUsd Get latest eth price in USD
#'
#' @return latest eth price in CNY
#' @export
#' @import jsonlite
#'
#'
getPriceUsd = function(){
  eth_price_url = "https://api.coinmarketcap.com/v1/ticker/ethereum/?convert=USD"
  eth_price = as.numeric(fromJSON(eth_price_url)$price_usd)
  eth_price
}

#' getInfo Get the balance and the real value of a ETH address
#'
#' @param eth_addr the address
#' @param name name of the address
#' @param use_cny True for cny, False for usd
#'
#' @return a one line data.frame
#' @export
#'
#'
getInfo = function(eth_addr, name = "unamed", use_cny = T){
  balance = getBalance(eth_addr)

  if(use_cny){
    price = getPriceCny()
  }else{
    price = getPriceUsd()
  }
  account_value = balance * price

  data.frame(Account = name,
             Volume=round(balance,2),
             Value=round(account_value/10000,2))

}

#' getTrans Get the transaction histroy of an ETH address
#'
#' @param eth_addr the address
#' @param number number of lines
#'
#' @return the data.frame of all transactions
#' @import dplyr
#' @import jsonlite
#' @export
#'
#'
getTrans = function(eth_addr,number = 10){
  eth_transactions_url = paste0(
    "http://api.etherscan.io/api?module=account&action=txlist&address=",
    eth_addr,
    "&startblock=0&endblock=99999999&sort=asc&apikey=D1Z8Y7PYHKHS2IWCG98224MXQ9PU78IDKF"
  )
  data.raw = fromJSON(eth_transactions_url)
  data.raw2 = tail(dplyr::filter(data.raw$result,as.numeric(data.raw$result$value)!=0),number)

  #data.raw2 = (tail(data.raw$result,number))
  df = data.frame(Time = as.character(anytime(as.numeric(data.raw2$timeStamp))),
                  Volume = round(as.numeric(data.raw2$value)/10E+17,4)
  )
  df

}
