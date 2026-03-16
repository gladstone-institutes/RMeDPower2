##' Pilot plate assay data
##'
##' Pilot dataset from plate-based assays used in the RMeDPower2
##' documentation and examples. The data represent repeated
##' measurements across plates and experimental units and are
##' intended for illustrating experimental design specification,
##' model diagnostics, and power calculations.
##'
##' @format A data frame with observations from a pilot plate
##'   assay. Column names and structure are documented in the
##'   package vignette and example code.
##'
##' @seealso \code{\link{plate_assay_pilot_data_wo_repeats}},
##'   \code{\link{plate_assay_full_data}}
##' @docType data
##' @keywords datasets
##' @name plate_assay_pilot_data
##' @usage data(plate_assay_pilot_data)
NULL

##' Pilot plate assay data without repeated measurements
##'
##' Version of \code{\link{plate_assay_pilot_data}} where repeated
##' measurements have been removed, suitable for power analyses
##' that assume a single observation per experimental unit at each
##' time point or condition.
##'
##' @format A data frame containing the pilot plate assay data
##'   without repeated measurements. See the vignette for details
##'   on columns and preprocessing.
##'
##' @seealso \code{\link{plate_assay_pilot_data}},
##'   \code{\link{plate_assay_full_data}}
##' @docType data
##' @keywords datasets
##' @name plate_assay_pilot_data_wo_repeats
##' @usage data(plate_assay_pilot_data_wo_repeats)
NULL

##' Full plate assay dataset
##'
##' Full plate assay dataset corresponding to the pilot data but
##' including the complete experimental run. This dataset is used
##' in examples demonstrating power calculations under more
##' realistic sample sizes and hierarchies.
##'
##' @format A data frame containing the full plate assay data.
##'   Column definitions follow those of
##'   \code{\link{plate_assay_pilot_data}}.
##'
##' @seealso \code{\link{plate_assay_pilot_data}},
##'   \code{\link{plate_assay_pilot_data_wo_repeats}}
##' @docType data
##' @keywords datasets
##' @name plate_assay_full_data
##' @usage data(plate_assay_full_data)
NULL

##' Single-nucleus RNA-seq cluster-level count data
##'
##' Example dataset containing cluster-level count summaries from
##' a single-nucleus RNA-seq experiment. The data are intended to
##' illustrate how RMeDPower2 can be applied to hierarchical
##' omics experiments with counts aggregated at the cluster
##' level.
##'
##' @format A data frame of cluster-level counts and associated
##'   annotations.
##'
##' @seealso \code{\link{snRNAseq_gene_count_data}}
##' @docType data
##' @keywords datasets
##' @name snRNAseq_cluster_count_data
##' @usage data(snRNAseq_cluster_count_data)
NULL

##' Single-nucleus RNA-seq gene-level count data
##'
##' Example dataset containing gene-level count summaries from a
##' single-nucleus RNA-seq experiment. This dataset can be used
##' to demonstrate power calculations for differential
##' expression-type analyses across experimental conditions.
##'
##' @format A data frame of gene-level counts and associated
##'   annotations.
##'
##' @seealso \code{\link{snRNAseq_cluster_count_data}}
##' @docType data
##' @keywords datasets
##' @name snRNAseq_gene_count_data
##' @usage data(snRNAseq_gene_count_data)
NULL

##' Mouse behavior data from a Morris Water Maze assay
##'
##' Example behavioral dataset containing measurements from a
##' mouse Morris Water Maze (MWM) assay. The data represent
##' repeated measures across trials and subjects and are suitable
##' for illustrating repeated measures power analysis.
##'
##' @format A data frame of behavioral measurements with
##'   information on mouse, trial, and experimental condition.
##'
##' @docType data
##' @keywords datasets
##' @name mouse_behavior_MWM_assay_data
##' @usage data(mouse_behavior_MWM_assay_data)
NULL

##' Mouse brain electrophysiology data
##'
##' Example dataset containing electrophysiological measurements
##' from mouse brain recordings. The data are used to demonstrate
##' power analyses in experiments with repeated measurements and
##' complex correlation structures.
##'
##' @format A data frame of electrophysiological measurements and
##'   associated experimental annotations.
##'
##' @docType data
##' @keywords datasets
##' @name mouse_brain_electro_physiology_data
##' @usage data(mouse_brain_electro_physiology_data)
NULL

