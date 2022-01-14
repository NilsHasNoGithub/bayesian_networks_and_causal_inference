source("utils.r")

make_bins <- function(values, n_bins) {
    max_ <- max(values)
    min_ <- min(values)

    frac <- 1.0 / n_bins
    result <- rep(NULL, length(values))

    for (i in 0:(n_bins-1)) {
        bin_start <- min_ + (i * frac) * (max_ - min_) 
        bin_end <- min_ + ((i+1) * frac) * (max_ - min_)

        result[values >= bin_start & values <= bin_end] <- i
    }

    return(sapply(result, as.integer))
}

load_stroke_data <- function(data_file, n_bins_for_numeric=NULL, print_info=FALSE, make_ints=FALSE) {
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
    result$Hypertension <- sapply(result$Hypertension, as.factor)
    result$HeartDisease <- sapply(result$HeartDisease, as.factor)
    result$Gender <- sapply(result$Gender, as.factor)
    result$BMI <- sapply(result$BMI, as.numeric)
    result$GlucoseLevel <- sapply(result$GlucoseLevel, as.numeric)
    result$Work <- sapply(result$Work, as.factor)
    result$Residence <- sapply(result$Residence, as.factor)
    result$Married <- sapply(result$Married, as.factor)
    result$Smoking <- sapply(result$Smoking, as.numeric)
    result$Stroke <- sapply(result$Stroke, as.factor)
    
    
    # if (standardize) {
    #     result$Age <- standardize_0_1(result$Age)
    #     result$BMI <- standardize_0_1(result$BMI)
    #     result$GlucoseLevel <- standardize_0_1(result$GlucoseLevel)
    #     result$Smoking <- standardize_0_1(result$Smoking)
    # }


    n_bins <- n_bins_for_numeric
    
    if (!is.null(n_bins)) {
        result$Age <- make_bins(result$Age, n_bins)
        result$BMI <- make_bins(result$BMI, n_bins)
        result$GlucoseLevel <- make_bins(result$GlucoseLevel, n_bins)
        result$Smoking <- make_bins(result$Smoking, n_bins)
    }

    if (print_info) {
        print("Data after preprocessing:")
        print(paste("Number of rows: ", nrow(result)))
        print(paste("Number of columns: ", ncol(result)))
    }

    if (make_ints) {
        # result$Age <- sapply(result$Age, as.integer)
        result$Hypertension <- sapply(result$Hypertension, as.integer)
        result$HeartDisease <- sapply(result$HeartDisease, as.integer)
        result$Gender <- sapply(result$Gender, as.integer)
        # result$BMI <- sapply(result$BMI, as.integer)
        # result$GlucoseLevel <- sapply(result$GlucoseLevel, as.integer)
        result$Work <- sapply(result$Work, as.integer)
        result$Residence <- sapply(result$Residence, as.integer)
        result$Married <- sapply(result$Married, as.integer)
        # result$Smoking <- sapply(result$Smoking, as.integer)
        result$Stroke <- sapply(result$Stroke, as.integer)
        result[,] <- result[,] - 1
        
        
        result$Age <- result$Age + 1
        result$BMI <- result$BMI + 1
        result$GlucoseLevel <- result$GlucoseLevel + 1
        result$Smoking <- result$Smoking + 1

    } else {
        result$Age <- sapply(result$Age, as.factor)
        result$BMI <- sapply(result$BMI, as.factor)
        result$GlucoseLevel <- sapply(result$GlucoseLevel, as.factor)
        result$Smoking <- sapply(result$Smoking, as.factor)
        
    }

    # reset index of result
    result <- result[order(result$Age),]
    return(result)
}

