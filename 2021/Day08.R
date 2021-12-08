#Day 8
source("comms.R")
day <- 8
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText %>%
  str_sub(str_locate(problemText, '--- Day ')[1,1])  %>%
  cat

#### PART 1  ####
# Now we need to get the answer:
cat(rawInputDay)
inp <- rawInputDay %>% 
  str_trim() %>% 
  str_split('\n') %>%
  first %>% 
  str_split('[ ]*[|][ ]*') %>% sapply(as.list)

tbSegments <- tibble(Segment=inp[1,] %>% as.character(), Output=inp[2,] %>% as.character())

sortChar <- function(x) {
  x %>% str_split('') %>% first %>% str_sort() %>% paste0(collapse = '')
}
sortChar <- Vectorize(sortChar, USE.NAMES = F)

withoutCharsFrom <- function(x, y) {
  # given x, remove any chars seen in y and return the result
  x %>% 
    str_replace_all(glue('[{y}]+'),'')
}
withoutCharsFrom <- Vectorize(withoutCharsFrom, USE.NAMES = F)

processSegment <- function(Segment, Output) {
  Segment <- Segment %>% str_split(' ') %>% first %>% sortChar
  Output <- Output %>% str_split(' ') %>% first %>% sortChar
  one <- Segment[str_length(Segment)==2]
  four <- Segment[str_length(Segment)==4]
  seven <- Segment[str_length(Segment)==3]
  eight <- Segment[str_length(Segment)==7]
  a <- seven %>% withoutCharsFrom(one)
  bd <- four %>% withoutCharsFrom(one)
  fiveLetterWords <- Segment[str_length(Segment)==5]  # 2,3 or 5
  # in our bd var, it is either 'bd' or 'db'. Is it 'bd'?
  firstIsB <- ((fiveLetterWords %>% str_detect(str_sub(bd, 1,1)) %>% sum()) == 1)
  if (firstIsB) {b<-bd %>% str_sub(1,1);d<-bd %>% str_sub(2,2)} 
  else {d<-bd %>% str_sub(1,1);b<-bd %>% str_sub(2,2)}
  five <- fiveLetterWords[str_detect(fiveLetterWords,b)]
  c <- one %>% withoutCharsFrom(five)
  f <- one %>% withoutCharsFrom(c)
  g <- five %>% withoutCharsFrom(paste0(a,b,d,f))
  e <- eight %>% withoutCharsFrom(paste0(a,b,c,d,f,g))
  zero <- eight %>% withoutCharsFrom(d)
  two <- eight %>% withoutCharsFrom(paste0(b,f))
  three <- eight %>% withoutCharsFrom(paste0(b,e))
  six <- eight %>% withoutCharsFrom(c)
  nine <- eight %>% withoutCharsFrom(e)
  # browser()
  Output %>% # order of replacement matters, longest first
    str_replace(eight,'8') %>% 
    str_replace(nine, '9') %>% 
    str_replace(zero, '0') %>% 
    str_replace(six,  '6') %>% 
    str_replace(five, '5') %>% 
    str_replace(three,'3') %>% 
    str_replace(two,  '2') %>% 
    str_replace(four, '4') %>% 
    str_replace(seven,'7') %>% 
    str_replace(one,  '1') %>% 
    paste0(collapse = '')
}

with(tbSegments %>% slice(3) %>% as.list, {
  processSegment(Segment,Output)
})

tbAnswer <- tbSegments %>%
  rowwise() %>% 
  mutate(Result=processSegment(Segment,Output)) %>% 
  mutate(Answer = str_count(Result,'[1478]')) %>% 
  ungroup()

answer <- tbAnswer %>% 
  summarise(Answer=sum(answer)) %>% 
  pull(Answer)


# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

browseURL(glue("https://adventofcode.com/2021/day/{day}"))

#### PART 2  ####

answer <- tbAnswer %>% 
  summarise(Answer=sum(Result %>% as.numeric)) %>% 
  pull(Answer)

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

browseURL(glue("https://adventofcode.com/2021/day/{day}#part2"))
