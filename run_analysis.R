run_analysis <- function(directorio = "~/Dataset") {
        ## The script need the packages dplyr and the reshape2, so you must install it
        ## You need to put like argument, by default i put the directory "~/Dataset"
        library(dplyr)
        library(reshape2)
        actividades <- read.table(paste(directorio, "/activity_labels.txt",sep=""),stringsAsFactors = FALSE)
        variables <- read.table(paste(directorio, "/features.txt",sep=""),stringsAsFactors = FALSE)
        x_test <- read.table(paste(directorio, "/test/X_test.txt",sep=""),stringsAsFactors = FALSE)
        personas_test <- read.table(paste(directorio, "/test/subject_test.txt",sep=""),stringsAsFactors = FALSE)
        actividades_test <- read.table(paste(directorio, "/test/y_test.txt",sep=""),stringsAsFactors = FALSE)
        x_train <- read.table(paste(directorio, "/train/X_train.txt",sep=""),stringsAsFactors = FALSE)
        personas_train <- read.table(paste(directorio, "/train/subject_train.txt",sep=""),stringsAsFactors = FALSE)
        actividades_train <- read.table(paste(directorio, "/train/y_train.txt",sep=""),stringsAsFactors = FALSE)
  
        ## Colocamos los id de las personas en la respectiva columna para poder agrupar (posiblemente no se ocupe)
        x_test[,562] <- personas_test[,1]
        x_train[,562] <- personas_train[,1]
        x_test[,563] <- actividades_test[,1]
        x_train[,563] <- actividades_train[,1]
  
        ## Agregamos la columna en la tabla de variables
        variables[562,1] <- 562
        variables[562,2] <- "Subject"
        variables[563,1] <- 563
        variables[563,2] <- "Activity"
  
        ##Merges the training and the test sets to create one data set.
        ## We don't need the "by" argument, because there is not repeated subjects in both tables
        principal <- merge(x_test,x_train, all = TRUE)
        principal <- tbl_df(principal)
        ##ponemos los nombres de las variables en el data frame, sin embargo debido a que existen nombres duplicados utilizamos make names para corregir el detalle
        names(principal) <- variables[,2]
        nombres_validos <- make.names(names=names(principal), unique=TRUE, allow_ = TRUE)
        names(principal) <- nombres_validos
        ##Extracts only the measurements on the mean and standard deviation for each measurement. 
        resultado <- select(principal, contains(".mean."), contains (".std."), Subject, Activity)
        ##Uses descriptive activity names to name the activities in the data set
        resultado <- merge(resultado,actividades, by.x= "Activity", by.y="V1", all = TRUE)
        colnames(resultado)[69] <- "Activity_Desc"
        ##Appropriately labels the data set with descriptive variable names. 
        ##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        final_tidy <- melt(resultado, id = c("Activity","Activity_Desc","Subject"))
        final_tidy <- group_by(final_tidy, Activity, Activity_Desc, Subject, variable) 
        final_tidy <- summarise(final_tidy, Average = mean(value))
        write.table(final_tidy, "ultimo.txt", row.name=FALSE)
        final_tidy
} 
