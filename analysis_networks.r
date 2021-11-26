library(dagitty)
library(ranger)
library(lavaan)
# library(bayesian)

source("utils.r")
source("data_processing.r")

plot_graph <- function(base_path, graph) {
    png(paste0(base_path, "/graph.png"))
    plot(graph)
    dev.off()
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

        # print(predict(model, data)$predictions)
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

    # print(varTable(fit))
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
    analyze_edge_coeffs(base_path, graph, data)
}

data_file <- "data/healthcare-dataset-stroke-data.csv"

for (file in list.files("networks")) {
    graph <- dagitty(read_to_string(paste0("networks/", file)))
    graph <- dagitty::graphLayout(graph)
    anaylyze_graph(file, graph, data_file)
}

warnings()