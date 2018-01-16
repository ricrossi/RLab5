library(dplyr)

input <- read.delim(file = "data/input.txt", header = T, sep = "\t", dec = ".")

coeff <- read.delim(file = "data/lookup_table.txt", header = T, sep = "\t", dec = ".")


bmi <- function(w, h){
  w / (h^2)
}

if_else(input$gender == "m", 
        
        coeff_ob <- coeff %>%
          filter(disease == "ob") %>% 
          select(coeff, male), 
        
        coeff_ob <- coeff %>%
          filter(disease == "ob") %>% 
          select(coeff, female))

if_else(bmi < 25, 
        
        RAob <- coeff_ob %>%
          filter(coeff_ob$coeff == "RAob_25", as.numeric(coeff_ob$male)), 
        
        RAob <- coeff_ob %>%
          filter(coeff_ob$coeff == "RAob_25", as.numeric(coeff_ob$female)))

rownames(coeff_ob) <- coeff_ob$coeff
coeff_ob$coeff <- NULL


ifelse(test, if T, if F)


coeff_ob <- coeff %>%
  filter(disease == "ob")

coeff_ob <- filter(coeff, disease == "ob")
coeff_ob <- select(coeff_ob, coeff, female)
