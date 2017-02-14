#### SOC 533 HW 5 --------------------------------------------------------------

#### Preamble ------------------------------------------------------------------
library(dplyr)

setwd("H:/SOC 533/HW5/")

#### Parse HMD/HFD data into `mort` and `fert` objects -------------------------

readLines("SWEbirthsTR.txt", n=10)
fert <- read.table("SWEbirthsTR.txt", skip=2, header=T)

readLines("fltcoh_5x5.txt", n=10)
mort <- read.table("fltcoh_5x5.txt", skip=2, header=T)


glimpse(fert)
glimpse(mort)

#View(fert)
#View(mort)

#### filter to relevant cohort data --------------------------------------------

fert <- fert %>%
  filter(Year>=1900, Cohort==1900)

mort <- mort %>%
  filter(Year=="1900-1904")



#### Compute 5-yr female-only fertility rates ----------------------------------

#total number of daughters within step/total number of person years lived

#first, apply prop. female births to total number of births

fert5 <- data.frame(
  age = mort$Age,
  totbirths = rep(0, nrow(mort))
)

#first age step with >0 total births

fert5$totbirths[4] <- sum(fert$Total[1:3])

#the rest

for(i in 1:length(fert5$totbirths)){
  j <- seq(0, 65, 9)
  fert5$totbirths[i+4] <- sum(fert$Total[(i+4+j[i]):(i+13+j[i])])
}

#View(fert5)

#### Join `fert5` and `mort` ---------------------------------------------------

sweden <- inner_join(mort, fert5, by=c("Age" = "age"))


#### Create Leslie matrix ------------------------------------------------------

#View(sweden)
fab <- 0.4886
A <- matrix(0, 23, 23)
rownames(A) <- fert5$age[2:24]
colnames(A) <- fert5$age[2:24]

rownames(A)[1] <- "0-4"
colnames(A)[1] <- "0-4"

#fertility first row

sweden <- sweden %>%
  mutate(nFx = totbirths/Lx)

A1 <- NULL
for(i in 1:23){
  A1[i] <- fab*(sum(sweden$Lx[1:i])/(2*sweden$lx[1]))*(sweden$nFx[i]+sweden$nFx[i+1]*sweden$Lx[i+1]/sweden$Lx[i])
}

A[1,] <- A1

#survivorship subdiag
survivor <- function(x){
  l <- NULL
  for(i in 1:length(x)-1){
    l[i] <- x[i+1]/x[i]
  }
  return(l)
}

sweden$Lx[2] <- sum(sweden$Lx[1:2])

l <- survivor(sweden$Lx)

A[row(A)-1 == col(A)] <- l[2:23]

#### Save Leslie matrix to storage ---------------------------------------------

write.csv(A, "Sweden 1900 leslie matrix.csv")
