#Day 4
source("comms.R")
day <- 4
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText  %>% cat

#### PART 1  ####
# Now we need to get the answer:

inp <- rawInputDay %>% 
  str_trim() %>% 
  str_split('\n\n') %>%
  first %>% 
  str_split('\n')

draws <- inp %>% head(1) %>% 
  str_split(',') %>% first %>% as.integer()

boards <- inp %>% tail(-1) %>% 
  lapply(function(x){
    board <- x %>% str_trim() %>% str_split('[ ]+') %>% sapply(as.integer)
  })

boardsBinary <- lapply(1:100,function(i){matrix(rep(FALSE,5*5),nrow=5)})
isSolved <- function(boardBinary) {
  rowOrColWin <- sapply(1:5, function(i){
    all(boardBinary[i,]) || # i'th row
      all(boardBinary[,i]) # i'th col
  }) %>% any
  diagWin <- all(diag(boardBinary)) || all(diag(boardBinary[5:1,]))
  if(rowOrColWin || diagWin) return(TRUE)
  return(FALSE)
}

updatedBoardsBinary <- function(boardsBinary, di) {
  # Given index of draw return the updated board using global (parent environment) variables for the boards/draws
  for (bi in seq_along(boardsBinary)) {
    boardsBinary[[bi]] <- boardsBinary[[bi]] | (boards[[bi]]==draws[di])
  }
  boardsBinary
}

bb <- boardsBinary
di <- 0
while(!any(lapply(bb, isSolved))) {
  di <- di + 1
  bb <- updatedBoardsBinary(bb, di)
}

solved <- which(sapply(bb, isSolved))
board <- boards[[solved]]
whereUnmarked <- bb[[solved]]==FALSE
bs <- sum(board[whereUnmarked])
draw <- draws[[di]]
answer <- bs*draw

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

#### PART 2  ####

bb <- boardsBinary
di <- 0
winOrder <- rep(-1, length(bb))
winCnt <- 1
while(!all(lapply(bb, isSolved))) {
  di <- di + 1
  draw <- draws[[di]]
  bb <- updatedBoardsBinary(bb, di)
  solved <- which(sapply(bb, isSolved))
  for (i in seq_along(solved)) {
    if(winOrder[[solved[[i]]]] < 0){
      winOrder[[solved[[i]]]] <- winCnt
      winCnt <- winCnt+1
    }
  }
}

worstBoardIndex <- which.max(winOrder)
board <- boards[[worstBoardIndex]]
whereUnmarked <- bb[[worstBoardIndex]]==FALSE
bs <- sum(board[whereUnmarked])
answer <- bs*draw


answer %>% 
  as.character() %>% 
  writeClipboard()
