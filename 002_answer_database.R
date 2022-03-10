##-----  1. Overview  -----

# Provide two things in this script:
  # 1. A list of the historical puzzle questions
  # 2. A list of the historical answer sets


## Use these answer sets to compare against the Scrabble dictionary
  # There seem to be a lot of words not allowed by Spelling Bee
  # This might be more prevalent for the shorter words?


# Look online for historical answers too?


## Export data to a database


## Keep two lists:
# Found answers (i.e. Scrabble answers)
# Spelling Bee answers (the actual answers)
# Cross compare the list over time to filter it down?



##-----  2. Packages  -----

library(tidyverse)



##-----  3. Data  -----

##--- 9 March 2022

spb_090322 <- c('a','i', 't', 'r', 'c', 'o', 'm') 
spb_090322_centre <- 'm'

spb_090322_answers <- c(
  'aromatic', 'ammo', 'armor', 'aroma', 'atom', 'atomic',
  'cami', 'camo', 'carom', 'coatroom', 'coma', 'comic', 'comma', 'commit', 'cram',
  'imam', 'imitator',
  'macro', 'maim', 'mama', 'mamma', 'maraca', 'marm', 'marmot', 'mart',
  'mica', 'micro', 'mimic', 'mirror', 'mitt', 'moat', 'momma', 'moor', 'moot',
  'moratoria', 'morocco', 'mortar', 'motor', 'motorcar', 'motto',
  'omit', 'roam', 'romcom', 'room', 'tamari', 'tarmac',
  'tatami', 'tomato', 'tomcat', 'tomtit', 'tram', 'tramcar', 'trim'
)



##--- 10 March 2022

spb_100322 <- c('g', 'l', 'v', 'a', 'o', 'e', 't') 
spb_100322_centre <- 't'

spb_100322_answers <- c()


