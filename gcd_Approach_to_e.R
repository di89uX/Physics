data <- read.csv("charge.csv")

#Extract charge values
charges <- data$Qc

#Normalize the charge values
normalized_charges <- charges*1e19

#Sort the  charges in ascending order
normalized_charges <- sort(normalized_charges)

#Initialize a vector to store all modulus results
mod_results <- c()

#Compute the modulus for each pair of charges
for (i in 1:(length(normalized_charges)-1)) {
  for(j in (i+1):length(normalized_charges)){
    mod_ij <- normalized_charges[j]%% normalized_charges[i]
    mod_results <- c(mod_results, mod_ij)
  }
  
}

#Remove very small modulus values that could be noise
significant_mods <- mod_results[mod_results>1] #Threshold

#Find the smallest significant modulus
estimated_e <- min(significant_mods)
print(estimated_e)

estimated_e_real <- estimated_e/1e19
cat("Estimated elementary charge using modulus approach is:", estimated_e_real, "C\n")
