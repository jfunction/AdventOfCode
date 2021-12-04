#Day 3
source("comms.R")
day <- 3
problemText <- getProblemDay(day)
rawInputDay <- getInputDay(day)

problemText  %>% cat

#### PART 1  ####
# Now we need to get the answer:

# Helper function to turn binary representation [1 0 1 1] into decimal 11
b2d <- function(b) {
  sum(b * 2^seq(length(b)-1,0,-1))
}

d2b <- function(d, bits=32) {
  rev(as.integer(intToBits(d))) %>% 
    tail(bits)
}

inMat <- rawInputDay %>%
  str_trim() %>% 
  str_split('\n') %>%
  first %>%
  sapply(function(x){strsplit(x,'')} %>% first, USE.NAMES = F) %>% 
  as.integer() %>% 
  matrix(nrow = str_length('101100100111')) %>% 
  t

bitLen <- dim(inMat)[2]

gam <- inMat %>% 
  colMeans() %>% 
  round() %>% 
  b2d()

eps <- bitwXor(2^bitLen-1, gam)

answer <- gam*eps
# 3862206 Too Low

# Finally save to clipboard and open browser
answer %>% 
  as.character() %>% 
  writeClipboard()

#### PART 2  ####
.round <- function(x) {
  # In R, rounding breaks ties by going to nearest even number
  # eg, round(0.5) gives 0
  # This is not desirable for our problem
  intPart <- floor(x)
  fracPart <- x-intPart
  if (fracPart>=0.5) return(intPart+1)
  return(intPart)
}

findThing <- function(mat, col=1, tiesGoTo=1) {
  mcb <- mat[,col] %>% mean() %>% .round()
  # search most common byte (rounded up) if ties go up or least common byte (rounded down) otherwise
  bit <- ifelse(tiesGoTo==1,mcb,1-mcb) 
  print(glue("{col} {bit}"))
  # if (mat[,col] %>% mean() == 0.5) browser()
  newMat <- mat[mat[,col]==bit,]
  # if (col>7) browser()
  if (!is.matrix(newMat)) return(newMat)  # single row
  if (length(newMat)==0) return(mat)      # empty matrix
  return(findThing(newMat, col=col+1, tiesGoTo=tiesGoTo))
}

ogr <- findThing(inMat) %>% b2d
csr <- findThing(inMat, tiesGoTo = 0) %>% b2d

answer <- ogr*csr
# 3992285 too high, covid brain. 
# 3990690 also too high, after is.na check
# I now realise we aren't keeping the global list for each search but filtering at each step
# Thus "most common" and "least common" may change each step.
# 2540096 is too low. This was because I didn't take care of the round() function. See .round() function above.

answer %>% 
  as.character() %>% 
  writeClipboard()

