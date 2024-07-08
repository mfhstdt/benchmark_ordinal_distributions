# Benchmark collection of 172 distributions of ordinal data
 
This benchmark collection includes 172 simulated distributions of ordinal data. 
This collection is inspired by benchmarks introduced by Haslbeck et al. (2022) and Marron & Wand
(1992) and is meant as a benchmark to validate and test statistical methods, such as modality detection methods. 

The ordinal distributions in our benchmark were created by categorizing simulated data into either 5 or 10 categories (with the exception of the claw distribution). Each distribution type was simulated for sample sizes N = 50, N = 500, N = 5000, and N = 50,000. Simulated distributions were either unimodal, bimodal, trimodal, or had five modes. The underlying data generating process was either Gaussian or non-Gaussian, with non-Gaussian distributions being (mixtures of) beta distributions, or sampled from
probabilities of specific cases which we found crucial for evaluating modality detection methods. Additionally, the benchmark entails uniform distributions as a special form of unimodals, and the claw distribution which is a mixture of five Gaussians. 

## Raw data
The distributions can be accessed as an RData file (`benchmark_distributions.RData`) or as a csv file in the zip folder `benchmark_distributions.zip`. 

## Replicate the data base
All simulated distributions are fully replicable, The corresponding R code is stored in `create_benchmark.R`. 

## Visualization 
Plots and names of all distributions in the benchmark can be found in `benchmark_distributions.pdf`

## References 
Haslbeck, J., Vermunt, J. K., & Waldorp, L. (2022). The impact of ordinal scales on
gaussian mixture recovery. *Behavior Research Methods, 55* (4), 2143–2156. https://doi.org/10.3758/s13428-022-01883-8

Marron, J., & Wand, M. (1992). Exact mean integrated squared error. *The Annals of Statistics, 20* (2), 712–736. https://doi.org/10.1214/aos/1176348653
