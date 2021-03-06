source("utils.r")


load_stroke_data <- function(data_file, standardize=TRUE, binarize=FALSE, print_info=FALSE) {
    data <- read.csv(data_file)
    
    # drop column 'id'
    data <- data[, -c(1)]

    if (print_info) {
        print("Data before preprocessing:")
        print(paste("Number of rows: ", nrow(data)))
        print(paste("Number of columns: ", ncol(data)))
    }

    # drop all rows in data which contain N/A
    data <- data[complete.cases(data),]

    # remove all the N/A in bmi entries
    data <- data[data["bmi"] != "N/A",]

    # remove the one case in which gender is not male or female
    data <- data[(data["gender"] == "Male") || (data["gender"] == "Female")]

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
    data$age <- sapply(data$age, as.numeric)
    
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

    result$Age <- sapply(result$Age, as.numeric)
    result$Hypertension <- sapply(result$Hypertension, as.logical)
    result$HeartDisease <- sapply(result$HeartDisease, as.logical)
    result$Gender <- sapply(result$Gender, as.logical)
    result$BMI <- sapply(result$BMI, as.numeric)
    result$GlucoseLevel <- sapply(result$GlucoseLevel, as.numeric)
    result$Work <- sapply(result$Work, as.logical)
    result$Residence <- sapply(result$Residence, as.logical)
    result$Married <- sapply(result$Married, as.logical)
    result$Smoking <- sapply(result$Smoking, as.numeric)
    result$Stroke <- sapply(result$Stroke, as.logical)

    if (standardize || binarize) {
        result$Age <- standardize_0_1(result$Age)
        result$BMI <- standardize_0_1(result$BMI)
        result$GlucoseLevel <- standardize_0_1(result$GlucoseLevel)
        result$Smoking <- standardize_0_1(result$Smoking)
    }

    if (binarize) {
        result$Age <- result["Age"] >= 0.5
        result$BMI <- result["BMI"] >= 0.5
        result$GlucoseLevel <- result["GlucoseLevel"] >= 0.5
        result$Smoking <- result["Smoking"] < 0.5
    }

    if (print_info) {
        print("Data after preprocessing:")
        print(paste("Number of rows: ", nrow(result)))
        print(paste("Number of columns: ", ncol(result)))
    }

    # reset index of result
    result <- result[order(result$Age),]
    return(result)
}

