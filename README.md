# Sweden Leslie Matrix for 1900 Cohort

This repository includes a leslie matrix computed from the Human Mortality Database and Human Fertility Database, along with a script showing the process used to produce this tool for population projection.


### Parsing the raw data

The first step was to read the headers from the raw text files obtained from HMD/HFD. Once R's base function `read.table` parses the files, we have objects `mort` and `fert` to munge towards the desired estimates of fertility rates and survivorship.

```r
readLines("SWEbirthsTR.txt", n=10)
fert <- read.table("SWEbirthsTR.txt", skip=2, header=T)

readLines("fltcoh_5x5.txt", n=10)
mort <- read.table("fltcoh_5x5.txt", skip=2, header=T)
```


### Filtering to the 1900 cohort

Dplyr `Filter` functions on the `fert` and `mort` objects selects the rows from the long form data that we are interested in.

```r
fert <- fert %>%
  filter(Year>=1900, Cohort==1900)

mort <- mort %>%
  filter(Year=="1900-1904")
```

###  Computing 5-yr female-only fertility rates

A new dataframe `fert5` takes the column of age ranges from the mortality life table, and then adds a blank column of zeroes for each row. Since the age groups are not completely aligned between the data, the next lines sum the youngest ages where births were documented (12-14) to fill the 10-14 element for A_1, followed by a loop that takes sums the number of births within the age ranges of the mortality data.

```r
#total number of daughters within step/total number of person years lived

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
```


### Joining the data

An inner join using dplyr's `inner_join` function creates a table of mortality and fertility variables for the 1900 cohort from Sweden

```r
sweden <- inner_join(mort, fert5, by=c("Age" = "age"))
```

### Constructing the Leslie Matrix

An empty 23 x 23 matrix is created first. The two components of the leslie matrix, the fertlity first row and the survivorship subdiagonal, each had a specific process. The first was a loop that took the total person years lived, the survivorship, and the fertility rates by age step to compute each element of the first row. Once the object `A1` is created, we overwrite the first row of our working object `A` using an index on the first row. I used a `survivor`function I wrote previously, along with an indexing appropriate for the subdiagonal to replace the non-structural zeroes in the matrix with the survivorship subdiagonal. The corresponding code for this portion is as follows:

```r
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
```
