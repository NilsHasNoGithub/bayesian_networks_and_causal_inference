

load_stroke_data <- function(data_file) {
    data <- read.csv(data_file)
    # data <- data[, 2:NCOL(data)]

    
    # drop column 'id'
    data <- data[, -c(1)]

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
    data[data["smoking_status"]=="never smoked", "smoking_status"] <- 1.0
    data[data["smoking_status"]=="formerly smoked", "smoking_status"] <- 2.0
    data[data["smoking_status"]=="smokes", "smoking_status"] <- 3.0


    # Private, Self-employed, Govt_job, children, Never_worked
    data$work_type_private <- data["work_type"] == "Private"

    data$bmi <- sapply(data$bmi, as.numeric)
    data$smoking_status <- sapply(data$smoking_status, as.numeric)
    
    result <- data.frame(
        Age = unlist(data["age"]),
        Hypertension = unlist(data["hypertension"]),
        HeartDisease = unlist(data["heart_disease"]),
        Gender = unlist(data["gender"]),
        BMI = unlist(data["bmi"]),
        GlucoseLevel = unlist(data["avg_glucose_level"]),
        Work = unlist(data["work_type_private"]),
        Residence = unlist(data["Residence_type"]),
        Married = unlist(data["ever_married"]),
        Smoking = unlist(data["smoking_status"]),
        Stroke = unlist(data["stroke"])
    )

    # reset index of result
    result <- result[order(result$Age),]
    return(result)
}