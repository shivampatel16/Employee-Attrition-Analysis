### binomial_attrition_dataset[,c('Non-Travel','Travel_Frequently','Travel_Rarely','HR','Research & Development','Sales','Human Resources','Life Sciences','Marketing','Medical','Other','Technical Degree','Healthcare Representative','Human Resources','Laboratory Technician','Manager','Manufacturing Director','Research Director','Research Scientist','Sales Executive','Sales Representative','Divorced','Married','Single')] = "Yes"

####### Logistic Regression ###########

	original_attrition_dataset = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
	binomial_attrition_dataset = original_attrition_dataset
	columnnames = c("BusinessTravel","Department","EducationField","JobRole","MaritalStatus")
	columnnames = as.character(columnnames)
	for(j in 1:length(columnnames)){
		columnname = columnnames[j]
		binomial_attrition_dataset[,columnname] = as.character(binomial_attrition_dataset[,columnname])
		columnvalues = levels(original_attrition_dataset[,columnname])
		columnvalues = as.character(columnvalues)
		for(i in 1:length(columnvalues)){
		  current_value = columnvalues[i]
		  current_array = binomial_attrition_dataset[,columnname]
		  current_array[current_array == current_value] = "Yes"
		  current_array[current_array != "Yes"] = "No"
		  binomial_attrition_dataset[,paste(columnname,current_value,sep = "_")] = current_array
		  binomial_attrition_dataset[,paste(columnname,current_value,sep = "_")] = as.factor(binomial_attrition_dataset[,paste(columnname,current_value,sep = "_")])
		  binomial_attrition_dataset[,paste(columnname,current_value,sep = "_")] = as.numeric(binomial_attrition_dataset[,paste(columnname,current_value,sep = "_")]) - 1
		}
		binomial_attrition_dataset[,columnname] = NULL
	}
	binomial_attrition_dataset$EmployeeCount = NULL
 	binomial_attrition_dataset$Over18 = NULL
 	binomial_attrition_dataset$StandardHours = NULL
	binomial_attrition_dataset$EmployeeNumber = NULL
	logistic_regression_model <- glm(Attrition~., data=binomial_attrition_dataset, family=binomial)
	logistic_regression_probabilities = predict(logistic_regression_model,type = "resp")
	logistic_regression_predicted_classification = logistic_regression_probabilities > 0.5
	logistic_regression_predicted_classification = as.numeric(logistic_regression_predicted_classification)
 	columnames = colnames(binomial_attrition_dataset)
	for(i in 1:length(colnames(binomial_attrition_dataset))){
	  if(as.character(class(binomial_attrition_dataset[,columnames[i]])) == "factor"){
		binomial_attrition_dataset[,columnames[i]] = as.character(binomial_attrition_dataset[,columnames[i]])
	  }
 }
	binomial_attrition_dataset$Attrition = binomial_attrition_dataset$Attrition == "Yes"
	binomial_attrition_dataset$Attrition = as.numeric(binomial_attrition_dataset$Attrition)
	actual_classification_of_training_data = binomial_attrition_dataset$Attrition
	subtraction_logistic = actual_classification_of_training_data - logistic_regression_predicted_classification
	final_results = subtraction_logistic == 0
	logistic_regression_accuracy = sum(final_results) / length(final_results)
	logistic_regression_accuracy
	
########## K means Clustering ###########
	clustering_attrition_dataset = binomial_attrition_dataset
	clustering_attrition_dataset$Attrition = NULL

	#clustering (in R) demands all the parameters to be of numeric type
	clustering_attrition_dataset$Gender = clustering_attrition_dataset$Gender == "Male"
	clustering_attrition_dataset$Gender = as.numeric(clustering_attrition_dataset$Gender)
	clustering_attrition_dataset$OverTime = clustering_attrition_dataset$OverTime == "Yes"
	clustering_attrition_dataset$OverTime = as.numeric(clustering_attrition_dataset$OverTime)
	set.seed(1)
	kmeans_model <- kmeans(clustering_attrition_dataset, 2, nstart=20)
	kmeans_clustering_predicted_classification = kmeans_model$cluster - 1
	subtraction_kmeans = actual_classification_of_training_data - kmeans_clustering_predicted_classification
	final_results = subtraction_kmeans == 0
	kmeans_accuracy = sum(final_results) / length(final_results)
	kmeans_accuracy

########## Hierarchical Clustering ##########

	hc.average = hclust(dist(clustering_attrition_dataset),method="average")
	cluster.average = cutree(hc.average, 2)
	cluster.average = cluster.average - 1
	subtraction_hierarchical = actual_classification_of_training_data - cluster.average
	final_results = subtraction_hierarchical == 0
	hierarchical_clustering_accuracy = sum(final_results) / length(final_results)
	hierarchical_clustering_accuracy

########## SVM ####################

	binomial_attrition_dataset$Gender = binomial_attrition_dataset$Gender == "Male"
	binomial_attrition_dataset$Gender = as.numeric(binomial_attrition_dataset$Gender)
	binomial_attrition_dataset$OverTime = binomial_attrition_dataset$OverTime == "Yes"
	binomial_attrition_dataset$OverTime = as.numeric(binomial_attrition_dataset$OverTime)
	library(e1071)
	svm_model = svm(Attrition~., data=binomial_attrition_dataset, cross = 10,type="C-classification")
	summary(svm_model)
	svm_predicted_classification = as.numeric(predict(svm_model,newdata = binomial_attrition_dataset)) - 1
	subtraction_svm = actual_classification_of_training_data - svm_predicted_classification
	final_results = subtraction_svm == 0
	svm_accuracy = sum(final_results) / length(final_results)
	svm_accuracy



	

























