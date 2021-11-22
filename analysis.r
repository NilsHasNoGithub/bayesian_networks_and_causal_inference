library(dagitty)
# library(bayesian)

source("utils.r")

plot_graph <- function(name, graph) {
    png(paste0("out/", name, "/graph.png"))
    plot(graph)
    dev.off()
}

analyze_conditional_indeps <- function(name, graph) {
    cond_indeps <- impliedConditionalIndependencies(graph)
    print_to_file(cond_indeps, paste0("out/", name, "/cond_indeps.txt"))
}

anaylyze_graph <- function(name, graph) {
    plot_graph(name, graph)
    analyze_conditional_indeps(name, graph)
}

for (file in list.files("networks")) {
    dir.create(paste0("out/", file), showWarnings = FALSE)
    graph <- dagitty(read_to_string(paste0("networks/", file)))
    anaylyze_graph(file, graph)
}


