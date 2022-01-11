library(bnlearn)
library(dagitty)
library(icesTAF)
library(pcalg)
source("utils.r")
source("data_processing_for_structure_learning.r")



mk_result_dir <- function(result_dir, extension) {
    result_dir <- paste0(result_dir, "/", extension)
    mkdir(result_dir)
    return(result_dir)
}

fit_network <- function(fit_fn, fit_type, data, result_dir) {
    fit <- fit_fn(data)
    # g <- as.dagitty(fit)

    result_dir <- mk_result_dir(result_dir, fit_type)
    plot_path <- paste0(result_dir, "/network.png")
    print_path <- paste0(result_dir, "/network_info.txt")
    scores_path <- paste0(result_dir, "/network_scores.txt")

    # bic_score <- score(fit, data, type = "bic")
    # aic_score <- score(fit, data, type = "aic")
    # bde_score <- score(fit, data, type = "bde")

    # scores_str <- paste0(
    #     "BIC: ", bic_score, "\n",
    #     "AIC: ", aic_score, "\n",
    #     "BDE: ", bde_score
    # )

    # write_to_file(scores_path, scores_str)
    print_to_file(print_path, fit)

    png(plot_path)
    plot(fit)
    dev.off()
}

fit_network_pc_alg <- function(data, result_dir) {
    labels <- colnames(data)

    suff_stat <- list(dm=data, nlev=nlevels_df(data), adaptDF = FALSE)

    # check whether to use binary or discrete test
    max_unique_vals <- 0
    for (col in colnames(suff_stat$dm)) {
        unique_vals <- unique(suff_stat$dm[, col])
        n_uniques <- length(unique_vals)
        if (n_uniques > max_unique_vals) {
            max_unique_vals <- n_uniques
        }
    }


    result <- pc(suff_stat, indepTest=binCItest, alpha=0.05, labels=labels)

    result_dir <- mk_result_dir(result_dir, "pc_alg")
    plot_path <- paste0(result_dir, "/network.png")
    print_path <- paste0(result_dir, "/network_info.txt")

    print_to_file(print_path, result)

    png(plot_path)
    plot(result)
    dev.off()
}

fit_networks <- function(data_file, result_dir) {
    data <- load_stroke_data(data_file, n_bins_for_numeric=2)

    # try the constraint based methods pc.stable, gs, iamb, fast.iamb, inter.iamb, iamb.fdr, mmpc, si.hiton.pc, hpc
    fit_network(pc.stable, "pc.stable", data, result_dir)
    fit_network(gs, "gs", data, result_dir)
    fit_network(iamb, "iamb", data, result_dir)
    fit_network(fast.iamb, "fast.iamb", data, result_dir)
    fit_network(inter.iamb, "inter.iamb", data, result_dir)
    fit_network(iamb.fdr, "iamb.fdr", data, result_dir)
    fit_network(mmpc, "mmpc", data, result_dir)
    fit_network(si.hiton.pc, "si.hiton.pc", data, result_dir)
    fit_network(hpc, "hpc", data, result_dir)

    # try the score-based methods hc, tabu
    fit_network(hc, "hc", data, result_dir)
    fit_network(tabu, "tabu", data, result_dir)

    # try logical discovery based methods aracne, chow.liu
    fit_network(aracne, "aracne", data, result_dir)
    fit_network(chow.liu, "chow.liu", data, result_dir)

    data <- load_stroke_data(data_file, n_bins_for_numeric=2, make_ints=TRUE)

    
    # try pcalg
    fit_network_pc_alg(data, result_dir)
}


data_file <- "data/healthcare-dataset-stroke-data.csv"

fit_networks(data_file, "out/structure_learning/results")








