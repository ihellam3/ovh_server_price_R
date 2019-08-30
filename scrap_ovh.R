scrap_ovh <- function(url) {
  library("rvest")
  library("data.table")
  
  arr <- read_html(url) %>% html_nodes("td") %>% html_text()
  
  #ToDollar <- function(raw) dollar_format(prefix = "", suffix = " €")(as.numeric(substring(raw, 1, nchar(raw) - 2)))
  ToDollar <- function(raw) as.numeric(substring(raw, 1, nchar(raw) - 2))
  
  return (rbindlist(lapply(0:(length(arr) / 12 - 1), 
                           function(i) 
                             data.frame(DC = arr[12 * i + 1], 
                                        Family = arr[12 * i + 2], 
                                        Name = arr[12 * i + 3], 
                                        API_Name = arr[12 * i + 4], 
                                        CPU_Model = arr[12 * i + 5], 
                                        RAM = arr[12 * i + 6], 
                                        Base_storage = arr[12 * i + 7], 
                                        Public_network = arr[12 * i + 8], 
                                        Private_network = arr[12 * i + 9], 
                                        Monthly_price_EUR = ToDollar(arr[12 * i + 10]), 
                                        Availability = arr[12 * i + 11], 
                                        Buy = arr[12 * i + 12]))))
}

