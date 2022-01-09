library(bnlearn)
library(dagitty)
library(icesTAF)
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

    write_to_file(scores_path, scores_str)
    print_to_file(print_path, fit)

    png(plot_path)
    plot(fit)
    dev.off()
}

fit_networks <- function(data, result_dir) {

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

}


data_file <- "data/healthcare-dataset-stroke-data.csv"
data <- load_stroke_data(data_file)
fit_networks(data, "out/structure_learning/results")





