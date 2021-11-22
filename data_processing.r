

load_stroke_data <- function(data_file) {
    data <- read.csv(data_file)
    # data <- data[, 2:NCOL(data)]

    # drop all rows in data which contain N/A
    data <- data[complete.cases(data),]

    # remove all the N/A in bmi entries
    data <- data[data["bmi"] != "N/A",]

    # remove all entries of which smoking status is unknown
    data <- data[data["smoking_status"] != "Unknown",]

    # binary variable columns: 
    # gender, hypertension, heart_disease, ever_married, Residence_type

    data$gender <- as.integer(data["gender"] == "Male")
    data$ever_married <- as.integer(data["ever_married"] == "Yes")

    # Convert Residence_type column into binary variables
    data$Residence_type <- as.integer(data["Residence_type"] == "Urban")

    # Replace smoking_status with numerical values
    data[data["smoking_status"]=="never smoked", "smoking_status"] <- 1
    data[data["smoking_status"]=="formerly smoked", "smoking_status"] <- 2
    data[data["smoking_status"]=="smokes", "smoking_status"] <- 3

    # Private, Self-employed, Govt_job, children, Never_worked
    data$work_type_private <- data["work_type"] == "Private"
    data$work_type_self_employed <- data["work_type"] == "Self-emplyed"
    data$work_type_govt_job <- data["work_type"] == "Govt_job"
    data$work_type_never_worked <- data["work_type"] == "Never_worked"
    data$work_type_children <- data["work_type"] == "children"

    # add column to data with name


    
    print(unique(data$work_type))
    print("-------------")
    return(data)
}