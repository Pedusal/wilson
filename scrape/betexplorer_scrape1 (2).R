
### FIRST DOWNLOAD PhantomJS ###
### AND REMEMBER PUT THE EXE FILE INTO THE WORING DIRECTORY ###
setwd("/Users/jetroanttonen/R/java_scrape")

library(rvest)
library(httr)
library(XML)

#GETTING THE LINKS
site <- 'http://www.betexplorer.com'
main_url <- paste0(site, '/hockey/usa/nhl-2017-2018/results/?stage=vgbaVqys&month=all')
page <- httr::GET(main_url, add_headers('user-agent' = 'r')) ## Kielletty jos user-agent = NULL
page <- xml2::read_html(page)
data <- rvest::html_nodes(page, '.in-match')
links <- rvest::html_attr(data, 'href')

#EXTRACTING THE TEAMS
t1 <- html_nodes(page, '.h-text-left')
t1 <- html_children(t1)
t1 <- html_nodes(t1, 'span')
t1 <- as.character(t1)
for(i in 1:length(t1)) {
  t1[i] <- sub("<span>", "", t1[i])
  t1[i] <- sub("</span>", "", t1[i])
  t1[i] <- sub("<strong>", "", t1[i])
  t1[i] <- sub("</strong>", "", t1[i])
}
t1 <- matrix(t1, ncol = 2, byrow = T)
colnames(t1) <- c("home_team", "away_team")

#EXTRACTING THE SCORES
s1 <- html_nodes(page, '.h-text-center')
s1 <- html_children(s1)
s1 <- html_text(s1)
ot <- rep(0, length(s1))
home_score <- rep(NA, length(s1))
away_score <- rep(NA, length(s1))
for(i in 1:length(s1)) {
  if(nchar(s1[i]) > 4) {
    ot[i] <- 1
    s1[i] <- substring(s1[i],1,4)
  }
  home_score[i] <- as.numeric(strsplit(s1[i], ":")[[1]][1])
  away_score[i] <- as.numeric(strsplit(s1[i], ":")[[1]][2])
  if(ot[i] == 1) {
    home_score[i] <- min(home_score[i], away_score[i])
    away_score[i] <- min(home_score[i], away_score[i])
  }
}
s1 <- cbind(home_score, away_score, ot)

#EXTRACTING THE DATES
p1 <- html_nodes(page, '.h-text-no-wrap')
p1 <- as.character(p1)
for(i in 1:length(p1)) {
  p1[i] <- sub('<td class="h-text-right h-text-no-wrap">', "", p1[i])
  p1[i] <- sub("</td>", "", p1[i])
  p1[i] <- gsub("\\.", "/", p1[i])
  p1[i] <- as.numeric(as.Date(p1[i], "%d/%m/%Y"))
}

#MERGING THE MATCH DATA
match_mat <- cbind(p1, t1, s1, links)
colnames(match_mat) <- c("eurodate", "home_team", "away_team", "home_score", "away_score", "OT", "link")
for(i in 1:dim(match_mat)[2]) {
  match_mat[,i] <- rev(match_mat[,i])
}

#CREATE THE scrape.js FILE (not necessary if created already)
writeLines("var url = 'http://www.betexplorer.com/hockey/usa/nhl-2017-2018/boston-bruins-florida-panthers/hd0qk8ZF/#ha';
var page = new WebPage();
page.settings.userAgent = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36';
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}
", con = "scrape.js")

#USE THE FOLLOWING FUNCTION FOR SCRAPING (this function could be silenced)
js_scrape <- function(url = "http://www.betexplorer.com/hockey/usa/nhl-2017-2018/boston-bruins-florida-panthers/hd0qk8ZF/#ha", 
                      js_path = "scrape.js", 
                      phantompath = paste0(getwd(),"/phantomjs")
                      ) {
  
  # this section will replace the url in scrape.js to whatever you want 
  lines <- readLines(js_path)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, js_path)
  
  command = paste(phantompath, js_path, sep = " ")
  system(command)
  
}

#A LIST OF BOOKIES: (work in progress)
bookielist <- c("bet365",
                "Unibet",
                "Betfair",
                "Betsson",
                "Betway",
                "bwin",
                "ComeOn",
                "10Bet",
                "Interwetten",
                "William Hill",
                "888sport",
                "188BET",
                "Betfair Exchange",
                "Pinnacle")
row_names <- c()
for(i in 1:length(bookielist)) {
  row_names <- c(row_names, paste0(bookielist[i],"_home"), paste0(bookielist[i],"_away"))
}
row_names <- c(row_names, "average_home", "average_away", "high_home", "high_away", "highbookie_home", "highbookie_away")

#THE SCRAPE
how_many <- dim(match_mat)[1]
final_mat <- matrix(NA, ncol = length(row_names), nrow = how_many)
colnames(final_mat) <- row_names

start_time <- Sys.time()

for(link_index in 1:how_many) {
  
  try(
    {
    match_url <- paste0(site, match_mat[link_index,"link"], '#ha')
    js_scrape(url = match_url) #Prints unnecessary stuff
    
    page <- xml2::read_html('1.html') #The html file downloaded by phantom is simply called '1.html'
    data <- rvest::html_nodes(page, 'td')
    text <- rvest::html_text(data) #The odds are to be found in this vector of character strings called 'text'
    
    mat <- matrix(NA, ncol = length(bookielist) + 3, nrow = 2)
    colnames(mat) <- c(bookielist, "average", "high", "highbookie")
    rownames(mat) <- c("home", "away")
    
    for(bookie in 1:length(bookielist)) {
      bookie_name <- bookielist[bookie]
      home_odd <- NA
      away_odd <- NA
      for(i in 1:length(text)) {
        if(text[i] == bookie_name) {
          index <- i
          break
        }
      }
      home_odd <- as.numeric(sub("\n","",text[index + 3]))
      away_odd <- as.numeric(sub("\n","",text[index + 4]))
      
      column <- which(bookielist == bookie_name)
      mat[1, column] <- home_odd
      mat[2, column] <- away_odd
    }
    for(i in 1:length(text)) {
      if(text[i] == "Average odds (exchanges not included in calculation)") {
        index <- i
        break
      }
    }
    
    #Average odds
    home_ave <- as.numeric(sub("\n","",text[index + 2]))
    away_ave <- as.numeric(sub("\n","",text[index + 3]))
    mat[1, (dim(mat)[2]-2)] <- home_ave
    mat[2, (dim(mat)[2]-2)] <- away_ave
    
    #Highest odds
    mat[1, (dim(mat)[2]-1)] <- max(na.omit(mat[1,]))
    mat[2, (dim(mat)[2]-1)] <- max(na.omit(mat[2,]))
    
    #Bookies that offered the highest odds
    mat[1, (dim(mat)[2])] <- colnames(mat)[which(mat[1,] == max(na.omit(mat[1,])))[1]] #Ugly/Lazy
    mat[2, (dim(mat)[2])] <- colnames(mat)[which(mat[2,] == max(na.omit(mat[2,])))[1]] #Ugly/Lazy
    
    row <- c(mat)
    
    final_mat[link_index,] <- row
    }
  )
  
  elapsed <- Sys.time() - start_time
  multiplier <- (how_many-link_index)/link_index
  left <- round(unclass(elapsed*multiplier))
  print(paste0(link_index, " / ", how_many))
  print(paste0("Time remaining: ", left, " ", attr(left, "units")))
}

final <- data.frame(cbind(match_mat, final_mat), stringsAsFactors = F)
#saveRDS(final, "hascrape1718.rds")



