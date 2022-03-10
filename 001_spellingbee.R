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
    # Number of words by starting letters
    # Full word list

## If it takes forever to compute, might need ot pre-compute?




##-----  2. Packages  -----

library(tidyverse)
library(words)
#library(combinat) # Not using this package
library(gtools)



##-----  3. Define Permutation and Search Function  -----

spelling_bee <- function(n, spb_letters, spb_centre) {
  
  ## Get all permutations, with repeats for n letter words
  perms <- permutations(
    7, # 7 input letters
    n, # Seeking words of length num
    spb_letters, # Input of the Spelling Bee letters
    set = FALSE, # Don't remove repeats from source vector, probably not needed
    repeats.allowed = TRUE # Yes, definitely want repeats of letters
  ) %>%
    as.data.frame() %>% # Convert to data frame
    unite(col = 'merged', sep = "") %>% # Concatenate the columns to form words
    pull() # Convert to atomic vector
  
  
  
  ## Drop rows which don't feature the centre letter
  perms <- perms[str_detect(perms, spb_centre)]
  
  
  
  
  ## Check all permutations against the valid words list
  acceptable_words <- list()
  
  for (i in seq_along(perms)) {
    
    
    ## Filter Scrabble words by desired length to trim the search time
    scrabble_words <- filter(words, word_length == n)$word
    
    ## Determine if they're in the Scrabble words, if so extract them
    if(any(str_detect(perms[i], scrabble_words))) {
      
      acceptable_words[[i]] <- perms[i]
      
    }
    
  }
  
  ## Drop the empty elements of the list
  acceptable_words <- acceptable_words[-which(sapply(acceptable_words, is.null))]
  
  ## Convert to data frame
  acceptable_words  <- as.data.frame(do.call(rbind, acceptable_words))
  
  
  return(acceptable_words)
  
}



##-----  4. Source Words  -----

## Load the list of Scrabble-approved words
data("words")



##-----  5. Spelling Bee Letters  ----- 

## Define the letters for today's puzzle
spb_090322 <- c('a','i', 't', 'r', 'c', 'o', 'm') 
spb_090322_centre <- 'm'

# When/if creating an interface for solving, enter the letters individually



##-----  5. Find the Permutations  -----

#~ Longest word in English is 45 letters long
  #! Realistically, how many iterations should I bother running through?


answers <- map(c(4:12), function(n) {spelling_bee(n, spb_090322, spb_090322_centre)})

## Combine answers into a data frame



## Ideas to speed up
  # Remove 'words' which have 3 or more of the same letter repeated consecutively
  # Remove 'words which have a high degree of repeat of the same letter (regardless of consecutive-ness)

# Test where the bottlenecks are - in the permutation generation phase, or the matching phase?





