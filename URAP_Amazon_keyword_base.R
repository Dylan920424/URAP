#= include the library
library(tidyverse)
library(rvest)

#Keywords: Temperature, Thermal, Light, Visual, Comfort, Comfortable, Energy / Energy Saving(s), Too hot / Too cold,

this.keyword <- c("temperature","thermal","light","visual", "comfort", "comfortable", "energy", "energy+saving","hot", "cold")
this.service <- "amazon"
this.product <- c("EmersonSmartThermostat","AmazonSmartPlug","KasaSmartPlug")
product.code <- c("B01NB1OB0I","B089DR29T6", "B07B8W2KHZ")

#=see this link for documentation: http://rvest.tidyverse.org/
#=open a browser and link to the page=
thermostat.url <- str_c("https://www.amazon.com/product-reviews/", product.code[1])
thermostat.session <- session(str_c(thermostat.url,"/ref=cm_cr_arp_d_paging_btm_next_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1&filterByKeyword=", this.keyword[1]))
thermostat <- thermostat.session

#= Scrape one specific review with link u
ScrapArticle <- function(u){
  
  #= jumps to link u
  temp.html <- thermostat %>%
    session_jump_to(u)
  
  user.name <- temp.html %>%
    html_nodes(css=".a-profile-name") %>%
    html_text()%>%
    unique()
  
  review.title <- temp.html %>%
    html_nodes(css=".a-text-bold span") %>%
    html_text()
  
  review.title = review.title[review.title!=""]
  
  review.date <- temp.html %>%
    html_nodes(css=".review-date") %>%
    html_text() %>%
    str_extract("[A-z]+ [0-9]+, [0-9]+")%>%
    as.Date("%B %d, %Y")
  
  review.rating <- temp.html %>%
    html_nodes(css=".review-rating")%>%
    html_node("span")%>%
    html_text()%>%
    str_extract("[0-5].[0-9]")%>%
    as.double()
  
  review.content <- temp.html %>%
    html_nodes(css=".review-text-content span") %>%
    html_text()
  
  review.content = review.content[review.content!=""]
  
  if(length(review.content)==0){
    review.content = ""
  }
  
  
  #combine into tibble
  article.table <- tibble(user=user.name,
                          title = review.title,
                          date = review.date,
                          rating = review.rating,
                          review = review.content,
                          url = u)
  return(article.table)
}

#= Scrape one page
ScrapePage <- function(pageNumber,key){
  link <- str_c(thermostat.url, "/ref=cm_cr_arp_d_paging_btm_next_", pageNumber, "?ie=UTF8&reviewerType=all_reviews&pageNumber=", pageNumber, "&filterByKeyword=", key)
  links.reviews <- thermostat %>%
    session_jump_to(link) %>% # move session to the most recent page
    html_nodes(css="#cm_cr-review_list .celwidget .a-row:nth-child(2)") %>%
    html_nodes("a") %>% # extract article <a>
    html_attr("href") %>% # extract article <a> `href` attributes
    unique() %>%
    str_c("https://amazon.com",.)
  scraped <- links.reviews %>%
    map(ScrapArticle)%>%
    bind_rows()
  return(scraped)
}

#= Code that scrapes pages number of page starting from page.latest
ScrapeWeb <- function(pages, page.latest,key){
  start <- page.latest-pages+1
  test <- start:page.latest
  match_key <- tibble(n = test,
                      key = sample(test,length(test)))
  lapply(test, function(i){
    j <- match_key[match_key$n==i,]$key
    message("Getting page ",i, " of ",length(test), "; Actual: page ",j)
    Sys.sleep(runif(1, 1.0,2.0))
    if((i%%3)==0){
      Sys.sleep(runif(1,0.0,1.5))
    }
    ScrapePage(j,key)
  })->scraped
  article.table.all <- scraped%>%
    bind_rows()
  Sys.sleep(runif(1, 0.0,1.5))
  message("took a break")
  return(article.table.all)
}

#= Code that scrapes the whole website
ScrapeAll <- function(key,itr){
  #= find the last page number
  tryCatch(
    expr = {
      thermostat.url <- str_c("https://www.amazon.com/product-reviews/", product.code[itr])
      thermostat.session <- session(str_c(thermostat.url,"/ref=cm_cr_arp_d_paging_btm_next_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1&filterByKeyword=", key))
      thermostat <- thermostat.session
      page.latest <- thermostat %>%
        html_element("#filter-info-section .a-size-base+ .a-size-base") %>%
        html_text() %>%
        str_replace_all(",","")%>%
        str_extract("[0-9]+ global reviews\n") %>%
        str_extract("[0-9]+") %>%
        strtoi()
      page.latest <- ceiling(page.latest/10)
      if (page.latest>500){
        page.latest=500
      }
      ans <- ScrapeWeb(page.latest%%10, page.latest,key)
      page.latest <- page.latest - page.latest%%10
      while(page.latest>0){
        temp <- ScrapeWeb(10,page.latest,key)
        message(nrow(temp), "rows")
        ans <- bind_rows(ans, temp)
        page.latest <- page.latest-10
      }
      ans <- ans%>%
        bind_cols(keyword= key,.)%>%
        bind_cols(service = this.service,.)%>%
        bind_cols(product = this.product[itr],.)
      write_csv(ans, str_c("/Users/dylan/OneDrive/Desktop/URAP/csv/keywords/",this.product[itr],".",this.service,"'(",key,").csv"))
      return(ans)
    },
    error = function(e) {
      message(e);
    }
  )
}

for (j in c(2,3)) {
  lapply(this.keyword, function(i) {
    ScrapeAll(i,j)
    message(i, " ",this.product[j])
  }) -> ans.test
  Sys.sleep(60)
}



