#Day 1
source("comms.R")
day <- 1
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText  %>% cat

# Now we need to get the answer:

depths <- rawInputDay1 %>%
  str_split('\n') %>%
  first %>%
  as.integer()

tbDepths <- tibble(depth=depths) %>% 
  drop_na() %>% 
  mutate(change=c(NA,diff(depth)),
         isIncreased=change>0)

numIncreased <- tbDepths %>% 
  filter(isIncreased) %>% 
  nrow()

answer <- numIncreased

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

# Part 2
numIncreased3 <- tbDepths %>% 
  mutate(slidingSum=c(NA,rollsum(depths, k=3))) %>%
  mutate(change2=c(NA,diff(slidingSum)),
         isIncreased2=change2>0) %>% 
  drop_na() %>% 
  filter(isIncreased2) %>% 
  nrow()

answer <- numIncreased3
answer %>% 
  as.character() %>% 
  writeClipboard()
