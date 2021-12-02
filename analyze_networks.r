library(dagitty)
library(ranger)
library(lavaan)

source("utils.r")
source("data_processing.r")

plot_graph <- function(base_path, graph) {
    png(paste0(base_path, "/graph.png"))
    plot(graph)
    dev.off()
}

binary_conditional_probability <- function(data, effect_var, cause_var, cause_state, cond_vars, cond_states) {
    rem_data <- data[data[cause_var] == cause_state,]

    for (i in seq_len(length(cond_vars))) {
        rem_data <- rem_data[rem_data[cond_vars[i]] == as.logical(cond_states[i]),]
    }

    if (nrow(rem_data) == 0) {
        return(0)
    }

    result <- nrow(rem_data[rem_data[,effect_var] == TRUE,]) / nrow(rem_data)
    return(result)
}

joined_conditional_probability  <- function(data, vars, states) {
    rem_data <- data

    for (i in seq_len(length(vars))) {
        rem_data <- rem_data[rem_data[vars[i]] == as.logical(states[i]),]
    }

    result <- nrow(rem_data) / nrow(data)
    return(result)
}

covariate_adjustment_sum_term <- function(data, effect_var, cause_var, cause_state, adj_set, cond_states) {
    conditional <- binary_conditional_probability(data, effect_var, cause_var, cause_state, adj_set, cond_states)
    normalizer <- joined_conditional_probability(data, adj_set, cond_states)
    result <- conditional * normalizer
    return(result)
}

do_covariate_adjustment_single_state <- function(data, effect_var, cause_var, cause_state, adj_set) {


    n_combinations <- 2^length(adj_set)

    cond_states <- matrix(0, nrow=n_combinations, ncol=length(adj_set))

    for (i in seq_len(n_combinations)) {
        cond_states[i,] <- as.logical(as.numeric(intToBits(i-1)))[seq_len(length(adj_set))]
    }

    sum <- 0.0
    for (i in 1:n_combinations) {
        sum <- sum + covariate_adjustment_sum_term(data, effect_var, cause_var, cause_state, adj_set, cond_states[i,])
    }

    return(sum)
}

do_covariate_adjustment <- function(data, effect_var, cause_var, adj_set) {
    result_true <- do_covariate_adjustment_single_state(data, effect_var, cause_var, TRUE, adj_set)
    result_false <- do_covariate_adjustment_single_state(data, effect_var, cause_var, FALSE, adj_set)

    return(list(true=result_true, false=result_false))
}

analyze_covariate_adjustment <- function(base_path, graph, data, effect_var) {
    file_ <- paste0(base_path, "/covariate_adjustment.txt")
    write_to_file(file_, "")
    for (p in dagitty::parents(graph, effect_var)) {
        adj_sets = dagitty::adjustmentSets(graph, p, effect_var)

        for (adj_set in adj_sets) {
            adj_set <- unlist(adj_set)
            result <- do_covariate_adjustment(data, effect_var, p, adj_set)
            adj_set_str <- paste(adj_set, collapse=", ")
            write_to_file(file_, paste0(p, " -> ", effect_var, ": adjusted for {", adj_set_str, "}: true: ", result$true, ", false: ", result$false, collapse=", "), append = TRUE)
        }

        write_to_file(file_, paste0("Adjustment sets for ", p, " -> ", effect_var, ": "), append = TRUE)
        print_to_file(file_, adj_sets, append=TRUE)
        write_to_file(file_, "=========================", append=TRUE)
    }
}


analyze_conditional_indeps <- function(base_path, graph, data, all_pairs = FALSE) {

    if (all_pairs) {
        type <- "all.pairs"
    } else {
        type <- "missing.edge"
    }

    cond_indeps <- dagitty::impliedConditionalIndependencies(graph, type = type)

    print(paste0("Amount of conditional independencies to be analyzed: ", length(cond_indeps)))

    file_ <- paste0(base_path, "/cond_indeps.txt")
    write_to_file(file_, "")

    for(c in cond_indeps) {
        formula <- paste0(c$X, ' ~ ', c$Y)
        formula_rev <- paste0(c$Y, ' ~ ', c$X)
        
        for (given in c$Z) {
            formula <- paste0(formula, ' + ', given)
            formula_rev <- paste0(formula_rev, ' + ', given)
        }
        
        model <- ranger::ranger(formula, data=data)
        model_rev <- ranger::ranger(formula_rev, data=data)

        resids <- data[, c$X] - predict(model, data)$predictions
        resids_rev <- data[, c$Y] - predict(model_rev, data)$predictions

        print_to_file(file_, c, append = TRUE)
        write_to_file(file_,paste0("Score: ", cor(resids, resids_rev)), append = TRUE)
        write_to_file(file_,"=========================", append = TRUE)
    }
}

perform_local_tests <- function(base_path, graph, data, type = "cis") {
    file_ <- paste0(base_path, "/local_tests.txt")
    results <- dagitty::localTests(x=graph, data=data, type=type, R=100)
    print_to_file(file_, results)
}

analyze_edge_coeffs <- function(base_path, graph, data) {
    plot_path <- paste0(base_path, "/edge_coeffs.png")
    fit_summary_path <- paste0(base_path, "/edge_coeffs_fit_summary.txt")
    fitted_graph_path <- paste0(base_path, "/edge_coeffs.txt")

    fit <- lavaan::sem(toString(graph, "lavaan"), data=data)
    print_to_file(fit_summary_path, summary(fit))

    fitted_graph <- dagitty::lavaanToGraph(fit, digits=2)
    print_to_file(fitted_graph_path, fitted_graph)

    dagitty::coordinates(fitted_graph) <- dagitty::coordinates(graph)
    png(plot_path)
    plot(fitted_graph, show.coefficients=TRUE)
    dev.off()
}


anaylyze_graph <- function(name, graph, data_file) {
    data <- load_stroke_data(data_file)
    base_path <- paste0("out/", name, "/numerical")
    dir.create(base_path, showWarnings=FALSE, recursive=TRUE)
    plot_graph(base_path, graph)
    analyze_conditional_indeps(base_path, graph, data)
    perform_local_tests(base_path, graph, data)
    analyze_edge_coeffs(base_path, graph, data)

    data <- load_stroke_data(data_file, binarize = TRUE)
    base_path <- paste0("out/", name, "/binary")
    dir.create(base_path, showWarnings=FALSE, recursive=TRUE)
    plot_graph(base_path, graph)
    analyze_conditional_indeps(base_path, graph, data)
    perform_local_tests(base_path, graph, data, type="cis.chisq")
    analyze_covariate_adjustment(base_path, graph, data, "Stroke")
    analyze_edge_coeffs(base_path, graph, data)
}

data_file <- "data/healthcare-dataset-stroke-data.csv"

for (file in list.files("networks")) {
    graph <- dagitty(read_to_string(paste0("networks/", file)))
    graph <- dagitty::graphLayout(graph)
    anaylyze_graph(file, graph, data_file)
}

warnings()