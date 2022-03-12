


# Improve on the layout and data of this site:
# https://nytbee.com/
# Can also go back and scrape some data: https://nytbee.com/Bee_20180729.html
# (Doesn't go all the way back to 9 May 2018, 29 July 2018 is the earliest solution set)

# Some answers checking, different way of laying out solutions here: 
# https://www.nytimes.com/2022/03/09/crosswords/spelling-bee-forum.html
# Can obviously go back in time and scrape a database of results too
# Forum only started some time in 2021?


## [Online / digital] Spelling bee strarted on 9 May 2018





##-----  2. Packages  -----

library(tidyverse)
library(lubridate)
library(rvest)




##-----  3. NYT Bee Site  -----

puzzle_date <- "20180729"

url <- str_c("https://nytbee.com/Bee_", puzzle_date, ".html")

#~ Check the structure of the URL is consistent with later years
  #~ It's not, there's been 3-4 different structures

#@---  29-July-2018 to  website structute:

answer_list <- read_html(url) %>% 
  html_elements(xpath = "//*[@id='answer-list']/ul") %>%
  html_text2() %>%
  str_remove(pattern = "\r") %>%
  str_split(pattern = "\n") %>%
  as.data.frame()
  
colnames(answer_list) <- "answers"


#! At some point the design of the website changed, used top/bottom block instead of left/right

##---  2018? something to 2019 something website structure


##--- 2019 something to 2022 onwards website structure

## Improved function by not using data.frame:
answer_list <- read_html("https://nytbee.com/Bee_20210729.html") %>% 
  html_elements(xpath = "//*[@id='main-answer-list']") %>%
  html_text2() %>%
  str_trim() %>%
  str_squish() %>%
  str_split(pattern = " ")

answer_list <- answer_list[[1]]




##---  Find the 7 Spelling Bee letters

all_spb_letters <- toString(answer_list) %>%
  str_extract_all("[a-z]") 

all_spb_letters <- toString(all_spb_letters[[1]]) %>% str_remove_all(", ") 

all_spb_letters <- unique(str_split(all_spb_letters, "")[[1]])


##---  Find the centre letter

test_split <- map(answer_list, function(x) str_split(x, pattern = "")[[1]])

centre_spb_letter <- Reduce(intersect, test_split)


  



#~ Looks like the letters are uploaded as a picture
#~ Determine the letters by 
  # a) finding the unique letter in each word
  # b) collapse to a string and find unique letters

# Stats to calculate:
  # Max score (1 pt for 4 letter word, 1 pt for each letter > length 4)
  # The rankings (assume they are percentages?)
  # Number of pangrams
  # Words starting with each letter etc
  # Total no. of answers



##---  Create a scraping function which can be applied to range of dates, execute the scrape
  #~ Include logic for determining which left/top division etc. to apply

