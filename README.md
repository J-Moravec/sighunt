## SigHunt: Tool for horizontal gene transfer detection.
This is reimplementation of *SigHunt: horizontal gene transfer finder optimized for eukaryotic genomes.*

Original SigHunt had poor implementation and was burden with many problems, using it was hell.
There lies a new implementation, that should get rid of all these problems, making analysing signatures and subsequent
verification of putative islands much easier.

As is standard lately among R packages, new SigHunt will utilize:
* testhat -- for proper code testing
* roxygen2 -- for automatic generation of documentation
* Rcpp -- for C++ binding where more power is needed

and many others.
