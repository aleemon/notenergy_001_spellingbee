##-----  1. Overview  -----

# Look at NYT's Spelling Bee puzzle and determine the possible answers

# Look for words that are not considered answers by NYT (typically rude, but also English spellings?)
# Also look, for fun, at words that don't include the centre letter


# Words must be minimum 4 letters long



## App Idea

  # wrap this into a front end (Shiny app?) that goes and finds all acceptable words?
  # Make it fun and enter the letters in a hexagon? (will require advanced CSS)
  # Show three levels of hints:
    # Number of words, by letter count
    # Show the number of pangrams
    # Number of words by starting letters
    # Full word list

## If it takes forever to compute, might need ot pre-compute?


# Ability to show historical data? Like store previously days etc.


#~ Testing on 9-Mar-22 4 letter words indicates that a lot of the Scrabble words are 



##-----  2. Packages  -----

library(tidyverse)
library(words)
#library(combinat) # Not using this package
library(gtools)
library(stringi)



##-----  3. Define Permutation and Search Function  -----

spelling_bee <- function(n, spb_letters, spb_centre) {

  system.time({  
  ## Get all permutations, with repeats for n letter words
  perms <- permutations(
    7, # 7 input letters
    n, # Seeking words of length n
    spb_letters, # Input of the Spelling Bee letters
    set = FALSE, # Don't remove repeats from source vector, probably not needed
    repeats.allowed = TRUE # Yes, definitely want repeats of letters
  ) %>%
    as.data.frame() %>% # Convert to data frame
    unite(col = 'merged', sep = "") %>% # Concatenate the columns to form words
    pull() # Convert to atomic vector
  }) 
  
  
  ## Drop rows which don't feature the centre letter
  perms <- perms[str_detect(perms, spb_centre)]
  
  
  #~ Drop any words where a single letter appears more than twice consecutively
    #~ Three consectuive letters might exist, if a hyphen is not accounted for, or
    #~ Onomaetopoes (bzzz, aaargh, etc.) But these are unlikely to be actual words
    #~ Validate this theory by searching the words database for three consecutive letters
  
  # perms <- perms[str_detect(perms, pattern = str_c(spb_centre, "{3,}"), negate = TRUE)] 
  # perms <- perms[str_detect(perms, pattern = str_c(spb_letters[1], "{3,}"), negate = TRUE)]
  # perms <- perms[str_detect(perms, pattern = str_c(spb_letters[2], "{3,}"), negate = TRUE)]
  # perms <- perms[str_detect(perms, pattern = str_c(spb_letters[3], "{3,}"), negate = TRUE)]
  # perms <- perms[str_detect(perms, pattern = str_c(spb_letters[4], "{3,}"), negate = TRUE)]
  # perms <- perms[str_detect(perms, pattern = str_c(spb_letters[5], "{3,}"), negate = TRUE)]
  # perms <- perms[str_detect(perms, pattern = str_c(spb_letters[6], "{3,}"), negate = TRUE)]
  
  ## Concatenate the entire list words of the relevent length and search for combos which definitely never occur
    # This creates a much more condensed list to match against
    #~ This line of code should actually cover the ones above searching for three letter combos!
  perms <-  perms[stri_detect_fixed(str_flatten(filter(words, word_length == n)$word), perms)]
  
  
  
  
  word_list <- filter(words, word_length == n)$word
  
  allowable_words <- list()
  
  for (i in seq_along(perms)) {
    
    allowable_words[[i]] <- word_list[stri_detect_fixed(word_list, perms[i])]
    
  }
  
  
  
  
  
 
  ## Check all permutations against the valid words list
  # acceptable_words <- list()
  # 
  # for (i in seq_along(perms)) {
  #   
  #   
  #   ## Filter Scrabble words by desired length to trim the search time
  #   word_list <- filter(words, word_length == n)$word
  #   
  #   ## Determine if they're in the Scrabble words, if so extract them
  # 
  #   #~ This should speed things up a bit, hopefully?
  #   acceptable_words[[i]] <- stri_detect_fixed(perms[i], word_list, max_count = 1)
  #   
  #   
  #   #if(any(str_detect(perms[i], scrabble_words))) {
  #   # if(any(stri_detect_fixed(perms[i], scrabble_words, max_count = 1))) {
  #   #   
  #   #   acceptable_words[[i]] <- perms[i]
  #   #   
  #   # }
  #   
  # }
  
  
  ## Drop the empty elements of the list
  #allowable_words <- allowable_words[-which(sapply(allowable_words, is.null))]
  allowable_words <- allowable_words[which(sapply(allowable_words, length) != 0)]
  
  
  ## Convert to data frame
  allowable_words  <- as.data.frame(do.call(rbind, allowable_words))
  
  
  return(allowable_words)
  
}



##-----  4. Source Words  -----

## Load the list of Scrabble-approved words
data("words")


## Drop any words < 4 letters long
words <- filter(words, word_length > 4)



##-----  5. Spelling Bee Letters  ----- 

## Define the letters for today's puzzle
spb_090322 <- c('a','i', 't', 'r', 'c', 'o', 'm') 
spb_090322_centre <- 'm'

# When/if creating an interface for solving, enter the letters individually



##-----  5. Find the Permutations  -----

#~ Longest word in English is 45 letters long
  #! Realistically, how many iterations should I bother running through, probably 12 or 14


answers <- map(c(4:12), function(n) {spelling_bee(n, spb_090322, spb_090322_centre)})

## Combine answers into a data frame









## Ideas to speed up
  # Remove 'words' which have 3 or more of the same letter repeated consecutively
  # Remove 'words which have a high degree of repeat of the same letter (regardless of consecutive-ness)

# Test where the bottlenecks are - in the permutation generation phase, or the matching phase?
  #~ The bottleneck is in the matching words against the scrabble list
  #~ But the permutation calculation also blows out badly with longer words


#~ Trick is going to be to cut down the number of permutations to match


# As suggested on SO - concat the entire allowable words list, and match weird combinations against that?
  # Then these could be excluded

system.time({
  perms_test <-  perms[stri_detect_fixed(str_flatten(filter(words, word_length == 6)$word), perms)]
})





# Introduce DT?
  # https://stackoverflow.com/questions/63120555/how-to-optimize-string-detection-for-speed


## This SO thread has some interesting approaches:
  # https://stackoverflow.com/questions/54717711/efficient-pattern-detecting-in-vector-of-strings
  # It suggests stringi package is faster than the stringr package?
  # Also suggests that base grepl() is faster than str_detect()


## Some analysis of bigram frequency in English:
  # https://www.petercollingridge.co.uk/blog/language/analysing-english/bigrams/




## Package 'ri' might be able to provide some assistance for breaking down the permutation problem?



##--- Testing

## n = 4
  # Permutation = 0 seconds
  # Matching = 53 seconds




system.time({
  answers <- spelling_bee(7, spb_090322, spb_090322_centre)
})

## n = 6
  # Permutation = .21 seconds
  # No. of perms = 117,649
  # Matching = takes way too long (several minutes at least)
  # After dropping 3 letter combos = 109,116 long. Still too long
  # Whole function with stringi took 6.5 minutes to resolve the answers - clearly the problem is increasing exponentially
  # Simpler function without if loop cut down to just under 5 minutes
  # Reduced permutations (no gobbledegook combos) - under 14 seconds. 


## n = 7
  # Takes 3.4 minutes


## n = 8
  # 7^8 = 5.7 million permutations, which is rather hefty
  # Computation time is looooooong


## n = 12
  # Permutation failed with a 2.7 GB vector (!!)
  # Permutations = 7^12 = 13,841,287,201 (which is should be able to handle, it's just being a lil' bitch)
  # It's gonna struggle with permutations above 12 letter words







