library(bnlearn)
library(dagitty)
library(icesTAF)
library(pcalg)
library(sets)
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

    print_to_file(print_path, fit)

    png(plot_path)
    plot(fit)
    dev.off()
}

get_arc_info <- function(arcs, n_nodes) {
    encountered <- set()


    n_arcs <- nrow(arcs)
    n_directed_arcs <- n_arcs

    for (i in 1:nrow(arcs)) {
        arc <- arcs[i,]
        from <- arc["from"]
        to <- arc["to"]

        arc_str <- paste0(from, "->", to)
        rev_arc_str <- paste0(to, "->", from)

        if (set_contains_element(encountered, rev_arc_str)) {
            n_directed_arcs <- n_directed_arcs - 2
            n_arcs <- n_arcs - 1
        }

        encountered <- encountered + set(arc_str)
    }

    # print(paste(n_arcs, n_directed_arcs))
    return(list(n_arcs = n_arcs, n_directed_arcs = n_directed_arcs, avg_degree = n_arcs / n_nodes))
}

fit_network_pc_alg <- function(data, base_result_dir) {
    # print(data)
    agg_result_dir <- mk_result_dir(base_result_dir, "/pc_alg")

    labels <- colnames(data)

    # cols_to_drop <- c("Age")
    # data <- data[,!colnames(data) %in% cols_to_drop]

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

    if (max_unique_vals > 2) {
        ci_test <- disCItest
    } else {
        ci_test <- binCItest
    }

    alphas <- c(0.01, 0.025, 0.05, 0.07, 0.10, 0.15)
    n_arcs <- c()
    n_directed_arcs <- c()
    avg_degrees <- c()
    

    for (alpha in alphas) {

        fit <- pc(suff_stat, indepTest=ci_test, alpha=alpha, labels=labels)

        if (!isValidGraph(as(fit, "amat"))) {
            fit <- pc(suff_stat, indepTest=ci_test, alpha=alpha, labels=labels, u2pd="retry")
        }

        result_dir <- mk_result_dir(agg_result_dir, paste0("/alpha_", alpha))
        plot_path <- paste0(result_dir, "/network.png")
        print_path <- paste0(result_dir, "/network_info.txt")

        print_to_file(print_path, fit)

        png(plot_path)
        plot(fit)
        dev.off()

        fit_bn <- as.bn(fit)
        arc_info <- get_arc_info(fit_bn$arcs, length(labels))

        n_arcs <- c(n_arcs, arc_info$n_arcs)
        n_directed_arcs <- c(n_directed_arcs, arc_info$n_directed_arcs)
        avg_degrees <- c(avg_degrees, arc_info$avg_degree)
    }

    
    results <- data.frame(
        alphas = alphas,
        n_arcs = n_arcs,
        n_directed_arcs = n_directed_arcs,
        avg_degrees = avg_degrees
    )

    write.csv(results, file=paste0(agg_result_dir, "/results.csv"))
    # g <- fit@graph
    # get_arc_info(g@edgeData)
}



fit_netwok_tabu <- function(data, result_dir_base) {
    agg_result_dir <- mk_result_dir(result_dir_base, "/tabu")

    scores_to_try <- c("bic", "aic", "bde", "loglik", "k2")
    tabu_parms_to_try <- c(1, 5, 10, 20, 50, 100)

    scores <- c()
    tabu_params <- c()
    n_arcs <- c()
    n_directed_arcs <- c()
    avg_degrees <- c()

    for (score in scores_to_try) {

        for (tabu_param in tabu_parms_to_try) {
            fit <- tabu(data, score=score, tabu=tabu_param)
            # 
            # g <- as.dagitty(fit)
            result_dir <- mk_result_dir(agg_result_dir, paste0("/score_", score, "_", tabu_param))
            plot_path <- paste0(result_dir, "/network.png")
            print_path <- paste0(result_dir, "/network_info.txt")

            print_to_file(print_path, fit)

            png(plot_path)
            plot(fit)
            dev.off()

            arc_info <- get_arc_info(fit$arcs, ncol(data))

            scores <- c(scores, score)
            tabu_params <- c(tabu_params, tabu_param)
            n_arcs <- c(n_arcs, arc_info$n_arcs)
            n_directed_arcs <- c(n_directed_arcs, arc_info$n_directed_arcs)
            avg_degrees <- c(avg_degrees, arc_info$avg_degree)
        }
    }

    results <- data.frame(
        scores = scores,
        tabu_params = tabu_params,
        n_arcs = n_arcs,
        n_directed_arcs = n_directed_arcs,
        avg_degrees = avg_degrees
    )

    write.csv(results, file=paste0(agg_result_dir, "/results.csv"))
}

fit_networks <- function(data_file, result_dir_base) {

    for (n_bins in c(2, 4, 8, 16, 32)) {
        result_dir <- paste0(result_dir_base, "_n_bins_", n_bins)
        data <- load_stroke_data(data_file, n_bins_for_numeric=n_bins)


        # try the constraint based methods pc.stable, gs, iamb, fast.iamb, inter.iamb, iamb.fdr, mmpc, si.hiton.pc, hpc
        # fit_network(pc.stable, "pc.stable", data, result_dir)
        # fit_network(gs, "gs", data, result_dir)
        # fit_network(iamb, "iamb", data, result_dir)
        # fit_network(fast.iamb, "fast.iamb", data, result_dir)
        # fit_network(inter.iamb, "inter.iamb", data, result_dir)
        # fit_network(iamb.fdr, "iamb.fdr", data, result_dir)
        # fit_network(mmpc, "mmpc", data, result_dir)
        # fit_network(si.hiton.pc, "si.hiton.pc", data, result_dir)
        # fit_network(hpc, "hpc", data, result_dir)

        # try the score-based methods hc, tabu
        # fit_network(hc, "hc", data, result_dir)
        # fit_network(tabu, "tabu", data, result_dir)

        # try logical discovery based methods aracne, chow.liu
        # fit_network(aracne, "aracne", data, result_dir)
        # fit_network(chow.liu, "chow.liu", data, result_dir)

        
        fit_netwok_tabu(data, result_dir)

        data <- load_stroke_data(data_file, n_bins_for_numeric=n_bins, make_ints=TRUE)
        fit_network_pc_alg(data, result_dir)
    }
}


data_file <- "data/healthcare-dataset-stroke-data.csv"
fit_networks(data_file, "out/structure_learning/results")











