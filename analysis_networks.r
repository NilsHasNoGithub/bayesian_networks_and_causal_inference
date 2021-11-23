library(dagitty)
library(ranger)
# library(bayesian)

source("utils.r")
source("data_processing.r")

plot_graph <- function(name, graph) {
    png(paste0("out/", name, "/graph.png"))
    plot(graph)
    dev.off()
}

analyze_conditional_indeps <- function(name, graph, data) {
    cond_indeps <- impliedConditionalIndependencies(graph, type = "all.pairs")

    print(paste0("Amount of conditional independencies: ", length(cond_indeps)))

    file_ <- paste0("out/", name, "/cond_indeps.txt")
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

anaylyze_graph <- function(name, graph, data) {
    plot_graph(name, graph)
    analyze_conditional_indeps(name, graph, data)
}

data <- load_stroke_data("data/healthcare-dataset-stroke-data.csv")

for (file in list.files("networks")) {
    dir.create(paste0("out/", file), showWarnings = FALSE, recursive = TRUE)
    graph <- dagitty(read_to_string(paste0("networks/", file)))
    anaylyze_graph(file, graph, data)
}


