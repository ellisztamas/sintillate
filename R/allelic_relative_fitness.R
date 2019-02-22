#' Relative fitness of a pair of alleles
#'
#' Efficient calculation of the relative fitness of reference alleles from
#' arrays of genotype and phenotype data.
#'
#' A set of individuals can been genotyped for a panel of (biallelic) genetic
#' markers, and scored for one or more fitness values (for example, multiple
#' components of fitness from a single experiment, or overall fitness in
#' multiple years).
#' \code{allelic_relative_fitness} calculates the relative fitness of a reference
#' allele at each marker using matrix operations, meaning large arrays of
#' genotype and phenotype data can be used efficiently.
#'
#' If alleles are labelled A and B, and genotypes AA, AB, and BB have mean
#' fitnesses \eqn{w_{AA}}, \eqn{w_{AB}} and \eqn{w_{BB}} respectively, then
#' the absolute fitnesses of alleles A and B are:
#'
#' \deqn{w_A = w_{AA} + w_{AB}/2}
#' \deqn{w_A = w_{BB} + w_{AB}/2}
#'
#' The relative fitness of reference allele B is then \eqn{w_B/w_A}.
#'
#' @param geno Array of marker data. Rows should index individuals/lines, and
#' columns should index markers. Specify allele labels with \code{alleles}.
#' @param pheno Array of phenotype data. Rows should index individuals/lines, and
#' columns should index phenotypes.
#' @param alleles Vector of three elements indicating the labels used for the
#' two homozygotes and heterozygotes. If allele B is the reference allele,
#' elements should index genotypes AA, AB, and BB in that order. Labels may be
#' numeric or character values. If data do not contain heterozygotes, an
#' arbitrary label is still required here. Data that do not match any of these
#' labels will be treated as NA.
#'
#' @return A dataframe with a row for each marker, and a column for each
#' phenotype.
#'
#' @export
allelic_relative_fitness <- function(geno, pheno, alleles=c(0,1,2)){
  .Deprecated("allelic_effect_sizes", msg = "allelic_relative_fitness was deprecated in favour of allelic_effect_sizes at sintillate versions 0.2.1.")
  # positions of homozygotes and heterozygotes
  maskAA <- (geno == alleles[1])
  maskAB <- (geno == alleles[2])
  maskBB <- (geno == alleles[3])
  # If there are NAs, set these to zero
  maskAA[is.na(maskAA)] <- 0
  maskAB[is.na(maskAB)] <- 0
  maskBB[is.na(maskBB)] <- 0
  pheno[is.na(pheno)]   <- 0
  # Create sparse arrays indicating fitness of individual markers
  sumAA <- t(maskAA) %*% as.matrix(pheno)
  sumAB <- t(maskAB) %*% as.matrix(pheno)
  sumBB <- t(maskBB) %*% as.matrix(pheno)
  # Relative fitness of each allele
  fitA <- (sumAA + (0.5*sumAB)) / colSums(maskAA + maskAB, na.rm = TRUE)
  fitB <- (sumBB + (0.5*sumAB)) / colSums(maskBB + maskAB, na.rm = TRUE)

  # # Check for odd patterns.
  if(any(fitA == 0)) warning("The allele labelled '", alleles[1],"' has zero fitness at one or more loci.")
  if(any(fitB == 0)) warning("The allele labelled '", alleles[3],"' has zero fitness at one or more loci.")
  if(any(fitA <  0)) warning("The allele labelled '", alleles[1],"' has negative fitness at one or more loci!")
  if(any(fitB <  0)) warning("The allele labelled '", alleles[3],"' has negative fitness at one or more loci!")

  relfit <- fitB / fitA
  return(as.data.frame(relfit))
}
