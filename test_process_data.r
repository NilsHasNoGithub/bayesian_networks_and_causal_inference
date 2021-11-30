source("data_processing.r")

data <- load_stroke_data("data/healthcare-dataset-stroke-data.csv", binarize = TRUE)

# print the columns of data
print(colnames(data))
print(head(data))