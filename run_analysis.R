run_analysis <- function(){
        
        library(dplyr)
        
        ## Retrieve measurement data        
        data_x_test <- read.table("X_test.txt")         
        data_x_train <- read.table("X_train.txt") 
        
        ## Combines data into a single set
        data_full <- rbind(data_x_test, data_x_train)
        
        ## map in column names from features.txt
        data_head <- read.table("features.txt")       
        colnames(data_full) <- data_head$V2 
        
        ## extract mean and std
        df_mean <- data_full[, grep("mean()",names(data_full), fixed = TRUE)]
        df_std <- data_full[, grep("std()",names(data_full), fixed = TRUE)]
        
        ## Retrieve activity and subject and combine into a single set
        data_y_test <- read.table("y_test.txt") 
        data_y_train <- read.table("y_train.txt")
        data_sub_test <- read.table("subject_test.txt") 
        data_sub_train <- read.table("subject_train.txt")        
        
        data_act <- rbind(data_y_test, data_y_train)
        data_sub <- rbind(data_sub_test, data_sub_train)
        
        ## combine activity, sub with mean and std
        data_com <- cbind(data_act, data_sub, df_mean,df_std)
        colnames(data_com)[1] <- 'activity id'
        colnames(data_com)[2] <- 'subject'
        
        ## Retrieve activity labels
        data_labels <- read.table("activity_labels.txt")
        
        ## combine labels with mean and std
        data_com_labels <- merge(data_labels,data_com,by.x = "V1", by.y = "activity id")
        colnames(data_com_labels)[colnames(data_com_labels) == 'V1'] <- 'activity id'
        colnames(data_com_labels)[colnames(data_com_labels) == 'V2'] <- 'activity'
        
        ## create a dataset with average of each variable for each activity and each subject
        df <- tbl_df(data_com_labels)
        df %>%
                group_by(activity, subject) %>%
                summarise_each(funs(mean), -c(1,2,3)) %>%
                
        write.table(file = "course_proj.txt",row.name = FALSE)
        
}
