monthlySSS <- function(income) {
    return(income*0.045)
}

monthlyPHILHEALTH <- function(income) {
    return(income*0.015) #change after rechecking calculation in website
}

monthlyIncomeTax <- function(incomeDeducted) {
    if(incomeDeducted <= 20833.00) {
        return(0.00)
    } else if(incomeDeducted > 20833.00 & incomeDeducted <= 33332.00) {
        return((incomeDeducted-20833.00)*0.2)
    } else if(incomeDeducted >= 33333.00 & incomeDeducted <= 66666.00) {
        return(2500.00+((incomeDeducted-33333)*0.25))
    } else if(incomeDeducted >= 66667.00 & incomeDeducted <= 166666.00) {
        return(10833.33+((incomeDeducted-66667)*0.3))
    } else if(incomeDeducted >= 166667.00 & incomeDeducted <= 666666.00) {
        return(40833.33+((incomeDeducted-166667)*0.32))
    }else if(incomeDeducted >= 666667.00) {
        return(200833.33+((incomeDeducted-666667)*0.35))
    }
}

input <- readline(prompt = "Enter a value: ")
income <- tryCatch(as.numeric(input), error = function(e) NA)

SSS_contribution <- monthlySSS(income)
PAGIBIG_contribution <- 100
PHILHEALTH_contribution <- monthlyPHILHEALTH(income)
total_contribution <- SSS_contribution+PAGIBIG_contribution+PHILHEALTH_contribution
incomeDeducted <- income-total_contribution
income_tax <- monthlyIncomeTax(incomeDeducted)
net_salary <- income-total_contribution-income_tax


cat("Monthly income: ", sprintf("%.2f", income), "\n")
cat("SSS contribution: ", sprintf("%.2f", SSS_contribution), "\n")
cat("PhilHealth contribution: ", sprintf("%.2f", PHILHEALTH_contribution), "\n")
cat("PAGIBIG contribution: ", sprintf("%.2f", PAGIBIG_contribution), "\n")
cat("Total contribution: ", sprintf("%.2f", total_contribution), "\n")
cat("Income deducted: ", sprintf("%.2f", incomeDeducted), "\n")
cat("Monthly income tax: ", sprintf("%.2f", income_tax), "\n")
cat("Net salary: ", sprintf("%.2f", net_salary), "\n")