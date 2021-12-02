#Day 2
source("comms.R")
day <- 2
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText  %>% cat

#### PART 1  ####
# Now we need to get the answer:

moves <- rawInputDay %>%
  str_trim() %>% 
  str_split('\n') %>%
  first

tbMoves <- tibble(moves=moves %>% sapply(function(x){str_trim(x)%>%str_split(' ')}, USE.NAMES = F)) %>% 
  mutate(direction=moves %>% sapply(first), amount=moves %>% sapply(last) %>% as.integer) %>% 
  select(direction, amount)

lsMoves <- tbMoves %>% group_by(direction) %>% 
  summarise(amount=sum(amount)) %>% 
  pull(amount, name=direction)

newLocation <- list(x=unname(lsMoves['forward']), y=unname(lsMoves['down']-lsMoves['up']))
answer <- newLocation$x*newLocation$y

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

#### PART 2  ####
tbMoves %>% 
  mutate(deltaAim=ifelse(direction=='forward',0,
                         ifelse(direction=='down', amount, -amount))) %>% 
  mutate(aim=cumsum(deltaAim)) %>% 
  mutate(deltaH=ifelse(direction=='forward',amount,0),
         deltaD=ifelse(direction=='forward',aim*amount,0)) %>% 
  mutate(x=cumsum(deltaH), y=cumsum(deltaD)) %>% 
  mutate(answer=x*y) %>% 
  tail(1) %>% 
  pull(answer) -> answer
# answer of 350855196 means you incremented deltaAim by 1 not by amount
# Because probably you didn't read the question carefully
# Because maybe you had covid at the time

#
answer %>% 
  as.character() %>% 
  writeClipboard()
