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
#library(iterpc) # 'arrangements' is a more up-to-date package
library(arrangements)


## Might need to look at alternative word databases

##-----  3. Define Permutation and Search Function  -----

spelling_bee <- function(n, spb_letters, spb_centre) {

  system.time({  
  ## Get all permutations, with repeats for n letter words
  perms <- gtools::permutations(
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
words <- filter(words, word_length >= 4)



##-----  5. Spelling Bee Letters  ----- 

## Define the letters for today's puzzle
spb_090322 <- c('a','i', 't', 'r', 'c', 'o', 'm') 
spb_090322_centre <- 'm'

# When/if creating an interface for solving, enter the letters individually



##-----  5. Find the Permutations  -----

#~ Longest word in English is 45 letters long, and there's some 21 letter words
  #! Realistically, how many iterations should I bother running through, probably 12 or 14

##---  9 March 2022

answers_090322 <- map(c(4:7), function(n) {spelling_bee(n, spb_090322, spb_090322_centre)}) 

## Combine answers into a data frame
#answers_df <- do.call(rbind, answers)
answers_090322_vec <- unlist(answers)


##---  10 March 2022

answers_100322 <- map(c(4:7), function(n) {spelling_bee(n, spb_100322, spb_100322_centre)}) 
answers_100322_vec <- unlist(answers_100322)


##-----  6. Work to improve speed


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




## Package 'iterpc' might be able to help break the problem into chunks?


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


## Compare the performance of 'permutations' to 'arrangements'

# Should be much faster, as benchmarked here:
  # https://randy3k.github.io/arrangements/articles/benchmark.html


# https://cran.r-project.org/web/packages/arrangements/arrangements.pdf
# https://stackoverflow.com/questions/69910536/why-does-gtoolscombinations-and-permutations-not-work-with-a-vector-containing

# https://gist.github.com/randy3k/bd5730a6d70101c7471f4ae6f453862e


perms <- gtools::permutations(
  7, # 7 input letters
  n, # Seeking words of length n
  spb_letters, # Input of the Spelling Bee letters
  set = FALSE, # Don't remove repeats from source vector, probably not needed
  repeats.allowed = TRUE # Yes, definitely want repeats of letters
) %>%
  as.data.frame() %>% # Convert to data frame
  unite(col = 'merged', sep = "") %>% # Concatenate the columns to form words
  pull() # Convert to atomic vector



# I understand how to use this package, from SO explanation
  # https://stackoverflow.com/questions/22569176/how-to-generate-permutations-or-combinations-of-object-in-r

#~ This is waaaaay more efficient than the gtools::permutation() function
  # Can handle up to n = 9
arrangs <- arrangements::permutations(
  x = spb_100322,
  k = 9,
  replace = TRUE
) %>%
  as.data.frame() %>% # Convert to data frame
  unite(col = 'merged', sep = "") %>% # Concatenate the columns to form words
  pull() # Convert to atomic vector

arrangs <- arrangs[str_detect(arrangs, spb_100322_centre)]
arrangs <- arrangs[stri_detect_fixed(str_flatten(filter(words, word_length == 7)$word), arrangs)]

## Now the subsetting/matching is the bottleneck - look to maybe chunk this approach?



##-----  6. Nic's approach  -----

# Muuuuch simpler and faster approach.

spb_letters <- spb_100322
spb_centre <- spb_100322_centre


spelling_bee_nic <- function(spb_letters, spb_centre) {
  
  not_the_letters <- setdiff(letters, spb_letters)
  
  allowed_words <- filter(words, str_detect(words$word, spb_centre)) # Filter words with the centre letter
  
  
  for (i in seq_along(not_the_letters)) {
    
    allowed_words <- filter(allowed_words, str_detect(allowed_words$word, not_the_letters[i], negate = TRUE))
    
  }

  
  return(allowed_words)
  
}

answers_100322 <- spelling_bee_nic(spb_100322, spb_100322_centre)



# Now produce hints, check for pangrams etc.

pangrams <- str_detect(answers_100322$word, every(spb_letters))


every(spb_letters, answers_100322$word)



