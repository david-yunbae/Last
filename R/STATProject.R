#The plan is to separate the test process by small steps hypothesis, assumption, actual test, result, decision and conclusion. This is because the assignment prompt suggested to use a wrapper function. I would like to demonstrate the use of the wrapper function.
#The hypothesis function.
Hypothesis <- function(test){
  if(test=="lm"){
    out <- "H0 : B = 0 against H1 : B != 0"
  } else if(test=="ttest"){
    out <-"H0 : mu1 = mu2 against H1 : mu1 != mu2"
  } else if(test=="chitest"){
    out <-"H0 : the two variables are independent against each other. H1 : not H0"
  }
  out
}

#The design is to input the data file and the type of test. The output is the list of assumptions and the graphs for the specified test.
Assumption <- function(dataf, test){
  dat <- readr::read_csv(dataf,show_col_types = FALSE)

  #Prepare assumption for lm
  D <- lm(height ~ weight,dat)
  a <- ggplot2::ggplot(dat,aes(x=height,y=weight))+ geom_point() + stat_smooth(method="lm", col="red") +ggtitle("I) Y vs X")

  b <- ggplot2::ggplot(dat)+geom_point(mapping=aes(x=D$fitted.values ,y=D$residuals)) + geom_hline(yintercept=0,lwd=2)+ggtitle("II) Residual plot")+ylab("Residuals")+xlab("Fitted values")

  c <- ggplot2::ggplot(dat)+geom_histogram(mapping=aes(x=D$residuals),bins=40) +ggtitle("III) Distribution is normal")+xlab("Residuals")

  #Prepare assumption for ttest
  d <- ggplot2::ggplot(dat, aes(sample=height, group=gender, colour=gender))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")

  e <- dat %>% group_by(gender) %>% dplyr::summarise(n=n(),mu=mean(height),sd=sd(height))

  #Preapre assumption for chitest
  datm <- dat %>% filter(gender=="Male") %>% select(phys)
  datf <- dat %>% filter(gender=="Female") %>% select(phys)

  datmn <- datm %>% filter(phys=="None") %>% count()
  datmm <- datm %>% filter(phys=="Moderate") %>% count()
  datmi <- datm %>% filter(phys=="Intense") %>% count()

  datfn <- datf %>% filter(phys=="None") %>% count()
  datfm <- datf %>% filter(phys=="Moderate") %>% count()
  datfi <- datf %>% filter(phys=="Intense") %>% count()

  table <- dplyr::tibble(Male=c(datmn[[1]],datmm[[1]],datmi[[1]]),Female=c(datfn[[1]],datfm[[1]],datfi[[1]]))


  if (test=="lm"){
    out <-"Assumptions: \nI Linearity \nII Constant variance \nIII Residuals normally distributed"
    print((a+b)/c)
  } else if (test=="ttest"){
    out <-c("Assumptions: \nI Normality \nII Equal variance (slarger/ssmaller < 2) - Actual value: ", max(e$sd)/min(e$sd))
    print(d)
  } else if (test=="chitest"){
    out <-"Assumptions: \nI Normal approximation: All entries must be at least 5."
    print(table)
  } else if (test=="all"){
    out <-"Assumptions: \nI Linearity \nII Constant variance \nIII Residuals normally distributed"
    print((a+b)/c)
    out <-c("Assumptions: \nI Normality \nII Equal variance (slarger/ssmaller < 2) - Actual value: ", max(e$sd)/min(e$sd))
    print(d)
    out <-"Assumptions: \nI Normal approximation: All entries must be at least 5."
    print(table)
  }
  out
}

#This does not directly output to the user, but this generates a list of results to hand over to another function that would later convert the information and display to the user appropriately.
Tester <- function(dataf,test){
  dat <- readr::read_csv(dataf,show_col_types = FALSE)

  if(test=="lm"){
    B <- lm(height~weight,dat)$coefficients[[2]]
    df <- lm(height~weight,dat)$df.residual
    p <- broom::glance(lm(height~weight,dat))$p.value[[1]]
    SE <- coef(summary(lm(height~weight,dat)))[,"Std. Error"][[2]]
    t <- summary(lm(height~weight,dat))[[4]][[6]]

    out <- list(type="lm",param=dataf,beta=B,CI=t*SE,t_value=t,degree_of_freedom=df,p_value=p)
    out$CI <- list(min=B-t*SE,max=B+t*SE)
  } else if(test=="ttest"){
    datm <- dat %>% filter(gender=="Male") %>% select(height)
    datf <- dat %>% filter(gender=="Female") %>% select(height)

    test <- t.test(datm,datf,var.equal=TRUE)
    out <- list(type="ttest",param=dataf,CI=t,t_value=test[[1]][[1]],degree_of_freedom=test[[2]][[1]],p_value=test[[3]][[1]])
    out$CI <- list(min=broom::glance(test)$conf.low,max=broom::glance(test)$conf.high)
  } else if(test=="chitest"){
    datm <- dat %>% filter(gender=="Male") %>% select(phys)
    datf <- dat %>% filter(gender=="Female") %>% select(phys)

    datmn <- datm %>% filter(phys=="None") %>% count()
    datmm <- datm %>% filter(phys=="Moderate") %>% count()
    datmi <- datm %>% filter(phys=="Intense") %>% count()

    datfn <- datf %>% filter(phys=="None") %>% count()
    datfm <- datf %>% filter(phys=="Moderate") %>% count()
    datfi <- datf %>% filter(phys=="Intense") %>% count()

    table <- dplyr::tibble(Male=c(datmn[[1]],datmm[[1]],datmi[[1]]),Female=c(datfn[[1]],datfm[[1]],datfi[[1]]))

    test <- chisq.test(table,correct=FALSE)
    out <- list(type="chitest",param=dataf,t_value=test[[1]][[1]],degree_of_freedom=test[[2]][[1]],p_value=test[[3]][[1]])
  }

  out
}

#x = Tester function
Result <- function(x){
  if(x$type=="lm"){
    out <- c("I beta: ", x$beta, "\nII CI: (", x$CI$min, ",", x$CI$max, ")\nIII degree of freedom: ", x$degree_of_freedom, "\nIV critical value : ", x$t_value, "\nV p value: ", x$p_value, "\n \n")
  } else if(x$type=="ttest"){
    out <- c("I CI: (", x$CI$min, ",", x$CI$max, ")\nII degree of freedom: ", x$degree_of_freedom, "\nIII critical value : ", x$t_value, "\nIV p value: ", x$p_value, "\n \n")
  } else if(x$type=="chitest"){
    out <- c("I degree of freedom: ", x$degree_of_freedom, "\nII critical value : ", x$t_value, "\nIII p value: ", x$p_value, "\n \n")
  }
  out
}

#x = Tester function
Decision <- function(x){
  if (x$p_value < 0.05){
    out <- c("REJECT: p-value = ", x$p_value, " < 0.05")
  } else {
    out <- c("DO NOT REJECT: p-value = ", x$p_value, " > 0.05")
  }
  out
}

#x = Tester function
Conclusion <- function(x){
  if (x$type=="lm"){
    if (x$p_value <0.05){
      out <- "There is a relationship between height and weight: As the P-value is very small, we have very strong evidence to reject H0. I.E. very strong evidence that the slope parameter is significant and there is a relationship between the height and weight of the sample population."
    } else if(x$p_value >0.05){
      out <- "There isn't any relationship between height and weight: As the P-value is large, we have no evidence to reject H0. I.E. no evidence that the slope parameter is significant and there isn't any relationship between the height and weight of the sample population."
    }
  } else if(x$type=="ttest"){
    if (x$p_value <0.05){
      out <- "The mean height of male and female are NOT the same: As the P-value is very small, we have very strong evidence to reject H0. I.E. very strong evidence that the mean height of male is not the same as the mean height of female."
    } else if(x$p_value >0.05){
      out <- "The mean height of male and female are the same: As the P-value is large, we have no evidence to reject H0. I.E. no evidence that the mean height of male is not the same as the mean height of female."
    }
  } else if(x$type=="chitest"){
    if (x$p_value <0.05){
      out <- "Gender affects the amount of physical activity: As the P-value is very small, we have very strong evidence to reject H0. I.E. very strong evidence that the two variables are dependent against each other. Gender affects the physical activity."
    } else if(x$p_value >0.05){
      out <- "Gender does NOT affect the amount of physical activity: As the P-value is large, we have no evidence to reject H0. I.E. no evidence that the two variables are dependent against each other. The two variables are independent against each other and there is no association between gender and the amount of physical acitivity."
    }
  }
}

#The wrapper function. is.list returns TRUE.
#' Generate output of a statistical test
#'
#' @param dataf file name; This is the file name of the data being tested (including .csv).
#' @param test text string; This is one of the test that should be implemented, such as lm, ttest or chitest.
#'
#' @return A list of test output information, yet less user-friendly as this is not expected to be viewed by general users. However, the default print function would print the user-friendly version of the output.
#' @export
#'
#' @examples
StatTest <- function(dataf,test){
  output <- list(Hypothesis=Hypothesis(test),Assumption=Assumption(dataf,test),Decision=Decision(Tester(dataf,test)),Conclusion=Conclusion(Tester(dataf,test)),TestData=Tester(dataf,test),Result=Result(Tester(dataf,test)))
  class(output) <- "myr"

  output
}

#x= StatTest wrapper funciton
#' The addition to the generic function print().
#'
#' @param x function name; The function StatTest is expected to be the only input.
#'
#' @return The output of the test that is readable by the general users.
#' @export
#'
#' @examples
print.myr <- function(x){
  cat("STAGE I>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Hypothesis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n",x$Hypothesis, "\n \n")
  cat("STAGE II>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Assumptions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> \n \n",x$Assumption,"\n \n")
  cat("STAGE III>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Results >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n",x$Result,"\n \n")
  cat("STAGE IV>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Decision >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n",x$Decision,"\n \n")
  cat("STAGE V>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Conclusion >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n \n",x$Conclusion,"\n \n")
}
