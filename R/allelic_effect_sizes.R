#' Effect sizes for biallic markers
#'
#' Efficient calculation of the additive or multiplicative differences in
#' phenotypic effects of pairs of alleles from arrays of genotype and phenotype
#' data.
#'
#' A set of individuals can been genotyped for a panel of (biallelic) genetic
#' markers, and scored for one or more traits.
#' \code{allelic_effect_sizes} calculates the effect on each phenotype of a
#' reference allele at each marker using matrix operations, meaning large arrays
#' of genotype and phenotype data can be used efficiently.
#'
#' If alleles at a locus are labelled A and B, then genotypes AA, AB, and BB have mean
#' phenotypes \eqn{z_{AA}}, \eqn{z_{AB}} and \eqn{z_{BB}} respectively. then
#' the absolute trait values of alleles A and B are:
#'
#' \deqn{z_A = z_{AA} + z_{AB}/2}
#' \deqn{z_B = z_{BB} + z_{AB}/2}
#'
#' Based on \eqn{z_A} and \eqn{z_B}, the effect size for this locus in tow ways.
#' In most cases, it will be appropriate to calculate the additive difference
#' \eqn{z_B - z_A}. Alternatively, especially when the trait in question is
#' fitness, it is appropriate to calculate the relative effect size
#' \eqn{z_B / z_A}, which puts effect sizes on the scale of selection.
#'
#' If effect sizes are being estimated in order to estimate \eqn{\tau}, it is
#' usually wise to scale phenotypes by the standard deviation and centre around
#' the mean. This is especially true for additive effect sizes.
#' \code{allelic_effect_sizes} does this by default, but this can be turned off
#' with the \code{scale} argument.
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
#' @param method String idicating the method used to estimate effect sizes. If
#' `difference` is given, additive effect sizes are calculated. If `ratio` is
#' given, relative effect sizes are calculated. Defaults to `difference`.
#' @param scale If \code{TRUE}, scale columns in \code{pheno} by their standard
#' deviation and centre around the mean. Defaults to \code{TRUE}.
#'
#' @return A dataframe of allelic effect sizes  with a row for each marker, and
#' a column for each phenotype.
#'
#' @author Tom Ellis
#' @export
allelic_effect_sizes <- function(geno, pheno, alleles=c(0,1,2), method="difference", scale=TRUE){
  if(scale){
    for(c in 1:ncol(pheno)) pheno[,c] <- scale(pheno[,c])
  }

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
  # Absolute trait value of each allele
  alleleA <- (sumAA + (0.5*sumAB)) / colSums(maskAA + maskAB, na.rm = TRUE)
  alleleB <- (sumBB + (0.5*sumAB)) / colSums(maskBB + maskAB, na.rm = TRUE)

  # # Check for odd patterns.
  if(method == "ratio"){
  if(any(alleleA <  0)) warning("The allele labelled '", alleles[1],"' has negative trait values at one or more loci.
If the trait is fitness and you have not centred phenotype data, this may be invalid.")
  if(any(alleleB <  0)) warning("The allele labelled '", alleles[3],"' has negative trait values at one or more loci.
If the trait is fitness and you have not centred phenotype data, this may be invalid.")
  }

  if(method == "difference"){
    effect_sizes <- alleleB - alleleA
  } else if (method == "ratio") {
    effect_sizes <- alleleB / alleleA
  } else {
    stop("Method to estimate effect size should be either `difference` or `ratio`.")
  }

  return(as.data.frame(effect_sizes))
}
