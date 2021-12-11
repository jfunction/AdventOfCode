#Day 9
source("comms.R")
day <- 9
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

browseURL(glue("https://adventofcode.com/2021/day/{day}"))
problemText %>%
  str_sub(str_locate(problemText, '--- Day ')[1,1])  %>%
  cat

#### PART 1  ####
# Now we need to get the answer:
cat(rawInputDay)
inp <- rawInputDay %>% 
  str_trim() %>% 
  str_split('\n') %>%
  first %>% lapply(function(x)x %>% str_split('') %>% first %>% as.numeric)

inp <- do.call(cbind, inp)
HEIGHT <- dim(inp)[1]
WIDTH <- dim(inp)[2]
above <- function(i,j, default=Inf) {if(i>1){return(inp[i-1,j])}else{return(default)}}
below <- function(i,j, default=Inf) {if(i<HEIGHT){return(inp[i+1,j])}else{return(default)}}
leftOf <- function(i,j, default=Inf) {if(j>1){return(inp[i,j-1])}else{return(default)}}
rightOf <- function(i,j, default=Inf) {if(j<WIDTH){return(inp[i,j+1])}else{return(default)}}
lowerThan <- function(i,j,regionFunc){return(inp[i,j]<regionFunc(i,j))}
isLocallyLowest <- function(i,j) {
  all(lowerThan(i,j,above),lowerThan(i,j,below),lowerThan(i,j,leftOf),lowerThan(i,j,rightOf))
}

risk <- 0
for(i in 1:HEIGHT){
  for(j in 1:WIDTH){
    if(isLocallyLowest(i,j)){
      risk <- risk + 1+inp[i,j]
    }
  }
}
answer <- risk

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

browseURL(glue("https://adventofcode.com/2021/day/{day}"))

#### PART 2  ####

# Can probably be phrased as graph theory problem with connected components
# Define directed graph
# Lets try this actually

# Vertices
tbGraph <- complete(tibble(i=1,j=1), i=1:100,j=1:100) %>%
  rowwise() %>%
  mutate(value=inp[i,j]) %>%
  ungroup() %>%
  rowid_to_column() %>% 
  filter(value!=9) # nines don't go anywhere so are useless for this problem

# Inefficiently construct edges for each vertex
tbEdges <- tibble(i1=numeric(0), j1=numeric(0), i2=numeric(0), j2=numeric(0))
for(i in 1:HEIGHT){
  for(j in 1:WIDTH){
    edges <- tibble(i1=numeric(0), j1=numeric(0), i2=numeric(0), j2=numeric(0))
    if (above(i,j) < inp[i,j]) {edges <- edges %>% add_row(i1=i,j1=j,i2=i-1,j2=j)}
    if (below(i,j) < inp[i,j]) {edges <- edges %>% add_row(i1=i,j1=j,i2=i+1,j2=j)}
    if (leftOf(i,j) < inp[i,j]) {edges <- edges %>% add_row(i1=i,j1=j,i2=i,j2=j-1)}
    if (rightOf(i,j) < inp[i,j]) {edges <- edges %>% add_row(i1=i,j1=j,i2=i,j2=j+1)}
    if (inp[i,j]!=9)
      tbEdges <- tbEdges %>% bind_rows(edges) %>% drop_na()
  }
}
# Check we did it ok
tbEdges

tbEdgesAll <- tbEdges %>%
  left_join(tbGraph %>% rename(srcID=rowid), by=c(i1='i',j1='j')) %>%
  right_join(tbGraph %>% rename(dstID=rowid), by=c(i2='i',j2='j')) %>% 
  select(srcID, dstID) %>% 
  drop_na()

# Make a graph where edges are flows
graph_from_data_frame(tbEdgesAll, vertices=tbGraph) %>% 
  igraph::decompose(mode='weak',min.vertices = 1) %>% 
  lapply(function(g) {
    i<-g %>% vertex_attr('i')
    j<-g %>% vertex_attr('j')
    id <- g %>% vertex_attr('name') %>% as.numeric() %>% min()
    vals <- g %>% vertex_attr('value') %>% as.numeric()
    size <- g %>% V %>% length
    tibble(i=i, j=j, minID=id, size=size, vals=vals)}) %>% 
  bind_rows -> tbColoured

# Was playing with colouring schemes for plotting
randNums <- seq(1,max(tbColoured$minID)) %>% sample(length(.))

# Make a pretty plot cos yolo
tbColoured %>% 
  mutate(ID=randNums[minID]) %>% 
  # slice_max(order_by=size, n=300) %>% 
  ggplot(.) +
    aes(x = j, y = i, fill = vals) +
    coord_flip() +
    geom_tile(size = .5) +
    # scale_color_distiller(palette = "Spectral", direction = 1) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    theme_dark() +
    labs(fill="Height", y='', x='', title="--- Day 9: Smoke Basin ---")

# Ok enough mucking around let's get the answer
tbColoured %>% 
  arrange(desc(size)) %>%
  # I wasn't thinking clearly so just fiddled with n until I got 3 components
  slice_max(order_by=size, n=300) %>% 
  pull(size) %>% 
  unique() %>% 
  prod() -> answer

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

browseURL(glue("https://adventofcode.com/2021/day/{day}#part2"))
