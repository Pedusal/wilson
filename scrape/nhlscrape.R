library(rvest)
library(httr)
library(XML)

#GETTING THE LINKS
site1 <- 'http://www.eliteprospects.com'
main_url1 <- paste0(site1, '/nhl/stats/2015-2016?nation=fin')
page1 <- httr::GET(main_url1, add_headers('user-agent' = 'r')) ## Kielletty jos user-agent = NULL
page1 <- xml2::read_html(page1)
data1 <- rvest::html_nodes(page1, '')
links1 <- rvest::html_attr(data1, 'href')
 
links1
links
