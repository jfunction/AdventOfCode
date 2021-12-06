#Day 6
source("comms.R")
day <- 6
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

evolve <- function(tbFish) {
  newFish <- tbFish %>%
    filter(inp==0) %>% 
    pull(n) %>% sum()
  tbFish <- tbFish %>% 
    mutate(inp=inp-1)%>% 
    mutate(inp=ifelse(inp==-1,6,inp))
  if(newFish!=0) {
    tbFish <- tbFish %>% 
      add_row(inp=8, n=newFish)
  }
  tbFish %>% 
    group_by(inp) %>% 
    summarise(n=sum(n), .groups = 'drop')
}

tbFish <- tibble(inp=inp) %>% 
  count(inp)

for (day in 1:80) {
  tbFish <- evolve(tbFish)
}
answer <- tbFish %>% 
  summarise(n=sum(n)) %>% 
  pull(n)

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

#### PART 2  ####

tbFish <- tibble(inp=inp) %>% 
  count(inp)

for (day in 1:256) {
  tbFish <- evolve(tbFish)
}
answer <- tbFish %>% 
  summarise(n=sum(n)) %>% 
  pull(n)

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

