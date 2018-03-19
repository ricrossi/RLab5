library(dplyr)

# read in the individual values and parameters
# (if the input mask organizes the input values in a dataframe)
input <- read.delim(file = "data/input.txt", header = T, sep = "\t", dec = ".")

# read the fixed coefficients
coeff <- read.delim(file = "data/lookup_table.txt", header = T, sep = "\t", dec = ".")
# transpose coeff data to better access values
rownames(coeff_cvd) <- coeff_cvd$coeff
coeff_cvd$coeff <- NULL
coeff_cvd <- t(coeff_cvd)

### function

riskcvd <- function(input, coeff) {
  
  # select cvd specific coeffs
  coeff_cvd <- coeff %>%
    dplyr::filter(disease == "cvd") %>% 
    select(coeff, male, female)
  rownames(coeff_cvd) <- coeff_cvd$coeff
  coeff_cvd$coeff <- NULL
  coeff_cvd <- as.numeric(as.matrix(coeff_cvd))

  # calculations for MALE
  # calculate the params*coefficients summary
  sum_m <- as.numeric(coeff_cvd[1])*input$age + 
    as.numeric(coeff_cvd[2])*input$bpmax + 
    as.numeric(coeff_cvd[3])*input$tc + 
    as.numeric(coeff_cvd[4])*input$hdl + 
    as.numeric(coeff_cvd[5])*input$smoke + 
    as.numeric(coeff_cvd[6])*input$t2d + 
    as.numeric(coeff_cvd[7])*input$hypdrug
  # calculate the absolute risk RA
  RA_m <- 1 - (as.numeric(coeff_cvd[8])^(exp(sum_m - as.numeric(coeff_cvd[9]))))
  # calculate the relative risk RR
  RR_m <- RA/0.08
  
  # calculations for FEMALE
  # calculate the params*coefficients summary
  sum_f <- as.numeric(coeff_cvd[10])*input$age + 
    as.numeric(coeff_cvd[11])*input$bpmax + 
    as.numeric(coeff_cvd[12])*input$tc + 
    as.numeric(coeff_cvd[13])*input$hdl + 
    as.numeric(coeff_cvd[14])*input$smoke + 
    as.numeric(coeff_cvd[15])*input$t2d + 
    as.numeric(coeff_cvd[16])*input$hypdrug
  # calculate the absolute risk RA
  RA_f <- 1 - (as.numeric(coeff_cvd[8])^(exp(sum_m - as.numeric(coeff_cvd[9]))))
  # calculate the relative risk RR
  RR_f <- RA/0.03
  
  # return RR according to gender in input
  RR <- if_else(input$gender == "m", RR_m, RR_f)
  
  return(RR)
  
}






