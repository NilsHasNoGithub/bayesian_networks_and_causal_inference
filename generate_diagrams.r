library(dagitty)

source("utils.r")


for (file in list.files("networks")) {
    graph <- dagitty(read_to_string(paste0("networks/", file)))
    png(paste0("out/", file, ".png"))
    plot(graph)
    dev.off()
}


