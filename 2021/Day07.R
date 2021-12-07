#Day 7
source("comms.R")
day <- 7
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText %>%
  str_sub(str_locate(problemText, '--- Day ')[1,1])  %>%
  cat

#### PART 1  ####
# Now we need to get the answer:

inp <- rawInputDay %>% 
  str_trim() %>% 
  str_split(',') %>%
  first %>% 
  as.integer()

cost <- Vectorize(function(dx) {
  sum(abs(inp-dx))
})

xx <- min(inp):max(inp)
costs <- cost(xx)
position <- tibble(pos=xx, cost=costs) %>% arrange(cost) %>% pull(pos) %>% first
fuel <- cost(position)
answer <- fuel


# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

#### PART 2  ####


# Finally save to clipboard and open browser
cost <- Vectorize(function(dx) {
  N <- abs(inp-dx)
  Cost <- N*(N+1)/2  # Sum(N)=N(N+1)/2
  sum(Cost)
})

xx <- min(inp):max(inp)
costs <- cost(xx)
position <- tibble(pos=xx, cost=costs) %>% arrange(cost) %>% pull(pos) %>% first
fuel <- cost(position)
answer <- fuel


# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()
