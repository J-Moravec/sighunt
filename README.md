## SigHunt: Tool for horizontal gene transfer detection.
This is reimplementation of *SigHunt: horizontal gene transfer finder optimized for eukaryotic genomes.*

Original SigHunt had poor implementation and was burden with many problems, using it was hell.
There lies a new implementation, that should get rid of all these problems, making analysing signatures and subsequent
verification of putative islands much easier.

As is standard lately among R packages, new SigHunt will utilize:

* testthat -- for proper code testing
* roxygen2 -- for automatic generation of documentation
* Rcpp -- for C++ binding where more power is needed

### Install

get the latest package

```sh
git clone https://github.com/J-Moravec/sighunt
```

start `R` and install package

```sh
install.packages("sighunt", repos = NULL, type="source")
```

### Basic usage

load fasta

```R
library(sighunt)
myseq <- read_fasta('my_sequence.fasta')
```

compute signature of the first sequence in the loaded fasta file with defined window size and the sliding window step

```R
mysignature <- get_signature(myseq[[1]], window = 5000, step = 1000)
```

get DIAS using one of the methods: `global_density`, `sliding_density` or `eye_of_storm`

```R
mydias <- global_density(mysignature)
```

plot it to find a good cutoff

```R
plot(mydias)
mycutoff <- 8
```

get positions of candidate regions (i.e. those with dias greater than chosen cutoff)

```R
mycandidates <- mydias[mydias > mycutoff]
head(mycandidates)
```

### Example of parsing a multifasta

the code is modular, therefore you can use comon R tools for parsing multifasta. However, it is always better to try one (or several) contigs to optimize parameters. Then you can put it together in a function a use `lapply` to get a list of candidate position (every entry in the list will be a vector of dias scores greater than the cutoff).

```R
library(sighunt)

# function that executes my analysis on one sequence
get_candidate <- function(sequence){
  signature <- get_signature(sequence, window = 5000, step = 1000)
  dias <- global_density(signature)
  dias <- dias[dias > 8]
  return(dias)
}

myseq <- read_fasta('my_sequence.fasta')
list_of_candidates <- lapply(myseq, get_candidate)
```

