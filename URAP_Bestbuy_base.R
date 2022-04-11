
library(tidyverse)
library(rvest)
library(RSelenium)

# set up headless server through firefox
rD <- rsDriver(browser="firefox", port=4560L, verbose=F)
remDr <- rD[["client"]]

# navigate to best buy url
ul = "https://www.bestbuy.com/site/reviews/google-nest-learning-smart-wifi-thermostat-stainless-steel/4346501?variant=A"
remDr$navigate("https://www.bestbuy.com")
remDr$navigate("https://www.bestbuy.com/site/reviews/google-nest-learning-smart-wifi-thermostat-stainless-steel/4346501?variant=A")

# html parse
html <- remDr$getPageSource()[[1]]
nest_thermo <- read_html(html)

# find how many page there is 
page.latest <- nest_thermo %>%
  html_element(".ugc-review-container") %>%
  html_text() %>%
  str_replace_all(",","")%>%
  str_extract("[0-9]+ customer reviews") %>%
  str_extract("[0-9]+") %>%
  strtoi()
page.latest <- ceiling(page.latest/20)


#Scrape page info
user.name <- nest_thermo %>%
  html_nodes(css=".body-copy-lg strong") %>%
  html_text() 

user.name <- user.name[c(TRUE, FALSE)]

review.title <- nest_thermo %>%
  html_nodes(css="#reviews-accordion .heading-5.v-fw-medium") %>%
  html_text()

review.date <- nest_thermo %>%
  html_nodes(css=".v-m-right-xxs .submission-date") %>%
  html_attr("title") %>%
  str_extract("[A-z]+ [0-9]+, [0-9]+")%>%
  as.Date("%B %d, %Y")

review.rating <- nest_thermo %>%
  html_nodes(css=".review-rating")%>%
  html_node("p")%>%
  html_text()%>%
  str_extract("[0-5]")%>%
  as.double()

review.content <- nest_thermo %>%
  html_nodes(css=".ugc-review-body .pre-white-space") %>%
  html_text()


#combine into tibble~
article.table <- tibble(user=user.name,
                        title = review.title,
                        date = review.date,
                        rating = review.rating,
                        review = review.content,
                        url = u)    
article.table 

ScrapePage <- function(parsed){
  nest_thermo <- parsed
  
  user.name <- nest_thermo %>%
    html_nodes(css=".body-copy-lg strong") %>%
    html_text()
  
  user.name <- user.name[c(TRUE, FALSE)]
  
  review.title <- nest_thermo %>%
    html_nodes(css="#reviews-accordion .heading-5.v-fw-medium") %>%
    html_text()
  
  review.date <- nest_thermo %>%
    html_nodes(css=".v-m-right-xxs .submission-date") %>%
    html_attr("title") %>%
    str_extract("[A-z]+ [0-9]+, [0-9]+")%>%
    as.Date("%B %d, %Y")
  
  review.rating <- nest_thermo %>%
    html_nodes(css=".review-rating")%>%
    html_node("p")%>%
    html_text()%>%
    str_extract("[0-5]")%>%
    as.double()
  
  review.content <- nest_thermo %>%
    html_nodes(css=".ugc-review-body .pre-white-space") %>%
    html_text()
  
  
  #combine into tibble~
  article.table <- tibble(user=user.name,
                          title = review.title,
                          date = review.date,
                          rating = review.rating,
                          review = review.content,
                          url = u)
  return(article.table)
}

#ScrapeWeb <- function(pages, page.latest){
#  start <- page.latest-pages+1
#  test <- start:page.latest
#  match_key <- tibble(n = test,
#                      key = sample(test,length(test)))
#  lapply(test, function(i){
#    j <- match_key[match_key$n==i,]$key
#    message("Getting page ",i, " of ",length(test), "; Actual: page ",j)
#    Sys.sleep(runif(1, 1.0,2.0))
#    if((i%%3)==0){
#      Sys.sleep(runif(1,1.0,3.0))
#    }
#    ScrapePage(j)
#  })->scraped
#  article.table.all <- scraped%>%
#    bind_rows()
#  message("took a break")
#  return(article.table.all)
#}

ScrapeAll <- function(){
  current_page = 1
  html <- remDr$getPageSource()[[1]]
  nest_thermo <- read_html(html)
  ans <- ScrapePage(nest_thermo)
  while(current_page<=page.latest){
    message(current_page)
    remDr$findElement("css",".next a")$clickElement()
    Sys.sleep(0.5)
    current_page <- current_page + 1
    u = str_c(ul, "&page=", current_page)
    html <- remDr$getPageSource()[[1]]
    temp <- read_html(html)
    temp <- ScrapePage(temp)
    ans <- bind_rows(ans, temp)
  }
  write_csv(ans, "/Users/dylan/OneDrive/Desktop/df_nest_thermo_plug.csv")
  ans
  remDr$quit()
  return(ans)
}

