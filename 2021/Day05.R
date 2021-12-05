#Day 5
source("comms.R")
day <- 5
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText %>%
  str_sub(str_locate(problemText, '--- Day ')[1,1])  %>%
  cat

#### PART 1  ####
# Now we need to get the answer:

inp <- rawInputDay %>% 
  str_trim() %>% 
  str_split('\n') %>%
  first

tbLines <- tibble(line=inp) %>% 
  rowwise() %>% 
  mutate(f1=line %>% str_split(' -> ') %>% first %>% first,
         f2=line %>% str_split(' -> ') %>% first %>% last,
         x1=f1 %>% str_split(',') %>% first %>% first %>% as.integer(),
         y1=f1 %>% str_split(',') %>% first %>% last %>% as.integer(),
         x2=f2 %>% str_split(',') %>% first %>% first %>% as.integer(),
         y2=f2 %>% str_split(',') %>% first %>% last %>% as.integer()) %>% 
  arrange(x1,y1,x2,y2) %>% 
  mutate(isVertical=x1==x2,
         isHorizontal=y1==y2,
         isMeshy=isVertical|isHorizontal) %>%
  ungroup()


tbLines %>%
  filter(isMeshy) %>% 
  rowid_to_column() %>% 
  group_by(rowid) %>% 
  group_modify(function(df1,df2) {
    result <- with(as.list(df1), {
      xx = seq(x1, x2)
      yy = seq(y1, y2)
      return(tibble(x=xx,y=yy))
    })
    result
  }) %>% 
  bind_rows %>% 
  ungroup %>% 
  mutate(value=1) %>% 
  group_by(x,y) %>% 
  summarise(value=sum(value), .groups = 'drop') %>% 
  filter(value!=1) %>% 
  nrow() -> answer


# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

#### PART 2  ####

tbLines %>% 
  rowid_to_column() %>% 
  group_by(rowid) %>% 
  group_modify(function(df1,df2) {
    result <- with(as.list(df1), {
      xx = seq(x1, x2)
      yy = seq(y1, y2)
      return(tibble(x=xx,y=yy))
    })
    result
  }) %>% 
  bind_rows %>% 
  ungroup %>% 
  mutate(value=1) %>% 
  group_by(x,y) %>% 
  summarise(value=sum(value), .groups = 'drop') %>% 
  filter(value!=1) %>% 
  nrow() -> answer


answer %>% 
  as.character() %>% 
  writeClipboard()
