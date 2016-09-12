

 demo = read.csv("predict_analysis.csv")


head(demo)
  num_visits location total_minutes_on_demo user_age demo_source data_rows_input operating_system users_in_network outreach_emails_sent support_tickets_filed converted
1          6   Denver             186.61749       48    referral            7166              OSX               27                    7                     1     False
2          5   Austin             108.25493       41       email           11950            Other               22                    5                     0      True
3          6  Phoenix              77.80533       43  conference            9700          Windows               20                    8                     1     False
4          6   Albany             126.41979       50       email           11378          Windows               16                    6                     0     False
5          5    Tulsa             192.56441       56       email            3401              OSX               11                    8                     2     False
6          8  Phoenix             112.12647       43     website            9803          Windows               29                    7                     3      True


str(demo)
'data.frame':   10000 obs. of  11 variables:
 $ num_visits           : int  6 5 6 6 5 8 7 8 5 8 ...
 $ location             : Factor w/ 18 levels "Albany","Atlanta",..: 7 3 14 1 18 14 7 9 8 12 ...
 $ total_minutes_on_demo: num  186.6 108.3 77.8 126.4 192.6 ...
 $ user_age             : int  48 41 43 50 56 43 41 36 47 45 ...
 $ demo_source          : Factor w/ 4 levels "conference","email",..: 3 2 1 2 2 4 2 1 1 2 ...
 $ data_rows_input      : int  7166 11950 9700 11378 3401 9803 11656 12375 6797 6397 ...
 $ operating_system     : Factor w/ 3 levels "OSX","Other",..: 1 2 3 3 1 3 1 3 3 1 ...
 $ users_in_network     : int  27 22 20 16 11 29 20 34 23 28 ...
 $ outreach_emails_sent : int  7 5 8 6 8 7 6 7 4 5 ...
 $ support_tickets_filed: int  1 0 1 0 2 3 0 1 1 0 ...
 $ converted  



demo_email = subset(demo, demo_source == "email")
head(demo_email)

demo_website = subset(demo, demo_source == "website")
head(demo_website)

email_Purchased = subset(demo_email,converted == "TRUE")
head(email_Purchased)

website_Purchased = subset(demo_website,converted == "TRUE")
head(website_Purchased)

table(demo_email$converted)

False  True 
 1773   693 

table(demo_website$converted)

False  True 
 1612   879 

# baseline model accuracy

1773/(1773+693)
[1] 0.7189781
> 1612/(1612+879)
[1] 0.6471297

# 2) Descriptive

summary(demo_email$total_minutes_on_demo)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.46  126.70  149.50  148.60  170.70  277.20 

  summary(demo_email$user_age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   25.0    42.0    45.0    45.1    48.0    65.0 

summary(demo_website$total_minutes_on_demo)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  27.97  126.00  146.40  148.10  167.40  284.30

summary(demo_website$user_age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   40.00   44.00   44.06   48.00   59.00  

jpeg('boxplot1.jpg')
 boxplot(email_Purchased$total_minutes_on_demo, website_Purchased$total_minutes_on_demo, col = (c("red", "blue")), main = "Time Spent on Email vs Website Demosource- Purchased", ylab = "Time Spent in Minutes", xlab = "Email Demo Source                                                  Website Demo Source")

jpeg('boxplot1.jpg')
boxplot(email_Purchased$user_age, website_Purchased$user_age, col = (c("gold", "green")), main = "User Age who Puchased software-DemoSource_Email & Website", ylab = "User Age", xlab = "Email Demo Source                                                  Website Demo Source")


#t-test


T1= t.test(email_Purchased$total_minutes_on_demo, website_Purchased$total_minutes_on_demo)
T1

        Welch Two Sample t-test

data:  email_Purchased$total_minutes_on_demo and website_Purchased$total_minutes_on_demo
t = -3.0539, df = 1414.3, p-value = 0.002301
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -10.038365  -2.186099
sample estimates:
mean of x mean of y 
 151.6688  157.7810 

  T1$p.value
[1] 0.002301062
 T1$conf.int
[1] -10.038365  -2.186099
attr(,"conf.level")
[1] 0.95


T2= t.test(email_Purchased$user_age, website_Purchased$user_age)
T2
        Welch Two Sample t-test

data:  email_Purchased$user_age and website_Purchased$user_age
t = -2.3483, df = 1567, p-value = 0.01898
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.97059710 -0.08711803
sample estimates:
mean of x mean of y 
 44.50072  45.02958 

T2$p.value
[1] 0.01898235

T2$conf.int
[1] -0.97059710 -0.08711803
attr(,"conf.level")
[1] 0.95


 T1 =replicate(1000, t.test(email_Purchased$user_age, website_Purchased$user_age)$p.value)
jpeg('densityplot_t1.jpg')
plot(density(T1), col = "red")


 T2 =replicate(1000, t.test(email_Purchased$user_age, website_Purchased$user_age)$p.value)
jpeg('densityplot_t2.jpg')
plot(density(T2), col = "red")


#Logistic regression
#load package ca tools
set.seed(100)
split = sample.split(demo_email$converted, SplitRatio = 0.75)
emailTrain = subset(demo_email, split== TRUE)
emailTest = subset(demo_email, split== FALSE)
nrow(emailTrain)
[1] 1850
nrow(emailTest)
[1] 616

 websiteTrain = subset(demo_website, split== TRUE)
websiteTest = subset(demo_website, split== FALSE)
> nrow(websiteTrain)
[1] 1868
> nrow(websiteTest)
[1] 623

reg = glm(converted ~ total_minutes_on_demo+num_visits, data = emailTrain, family = binomial)
summary(reg)

Call:
glm(formula = converted ~ total_minutes_on_demo + num_visits, 
    family = binomial, data = emailTrain)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.4540  -0.7248  -0.4814   0.7919   2.6763  

Coefficients:
                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)           -7.758689   0.471745 -16.447  < 2e-16 ***
total_minutes_on_demo  0.009711   0.001768   5.492 3.97e-08 ***
num_visits             0.729734   0.041676  17.510  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2197.7  on 1849  degrees of freedom
Residual deviance: 1779.1  on 1847  degrees of freedom
AIC: 1785.1

Number of Fisher Scoring iterations: 4


 exp(coefficients(reg))
          (Intercept) total_minutes_on_demo            num_visits 
         0.0004270161          1.0097579769          2.0745297334 


  coeff = summary(reg)$coef
 coeff
                          Estimate  Std. Error    z value     Pr(>|z|)
(Intercept)           -7.758688863 0.471744898 -16.446789 8.843380e-61
total_minutes_on_demo  0.009710675 0.001768027   5.492378 3.965575e-08
num_visits             0.729734493 0.041675780  17.509798 1.206209e-68

est= exp(coeff[ ,1])
est
          (Intercept) total_minutes_on_demo            num_visits 
         0.0004270161          1.0097579769          2.0745297334 

uppeerCI_reg = exp(coeff[,1]+1.96*coeff[ ,2])
uppeerCI_reg
          (Intercept) total_minutes_on_demo            num_visits 
          0.001076469           1.013263195           2.251100081 
lowerCI_reg = exp(coeff[,1]-1.96*coeff[ ,2])
lowerCI_reg
          (Intercept) total_minutes_on_demo            num_visits 
         0.0001693897          1.0062648842          1.9118090973    

 cbind(est,uppeerCI_reg,lowerCI_reg)
                               est uppeerCI_reg  lowerCI_reg
(Intercept)           0.0004270161  0.001076469 0.0001693897
total_minutes_on_demo 1.0097579769  1.013263195 1.0062648842
num_visits            2.0745297334  2.251100081 1.9118090973   



predictemailTrain = predict(reg, type ="response")
> summary(predictemailTrain)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.01291 0.11480 0.21690 0.28110 0.40410 0.95080 

table(emailTrain$converted, predictemailTrain >= 0.5)
       
        FALSE TRUE
  False  1198  132
  True    320  200

 predictemailTest = predict(reg, newdata = emailTest, type = "response")
table(emailTest$converted,  predictemailTest >= 0.5)
       
        FALSE TRUE
  False   398   45
  True    100   73


tapply(predictemailTrain,emailTrain$converted, mean)
    False      True 
0.2183020 0.4416506 


## load package ROCR

ROCRpred = prediction(predictemailTrain, emailTrain$converted)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)

##website reg

reg2 = glm(converted ~ total_minutes_on_demo+user_age+num_visits, data = websiteTrain, family = binomial)
summary(reg2)

Call:
glm(formula = converted ~ total_minutes_on_demo + user_age + 
    num_visits, family = binomial, data = websiteTrain)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0833  -0.9228  -0.6974   1.1576   2.6816  

Coefficients:
                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)           -8.796494   0.669612 -13.137  < 2e-16 ***
total_minutes_on_demo  0.018686   0.001735  10.769  < 2e-16 ***
user_age               0.066521   0.010064   6.609 3.86e-11 ***
num_visits             0.331702   0.035856   9.251  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2448.6  on 1867  degrees of freedom
Residual deviance: 2226.3  on 1864  degrees of freedom
AIC: 2234.3

Number of Fisher Scoring iterations: 4

exp(coefficients(reg2))
          (Intercept) total_minutes_on_demo              user_age            num_visits 
         0.0001512625          1.0188616555          1.0687831096          1.3933379303 

est2= exp(coeff2[ ,1])
 est2
          (Intercept) total_minutes_on_demo              user_age            num_visits 
         0.0001512625          1.0188616555          1.0687831096          1.3933379303 


 uppeerCI_reg2 = exp(coeff2[,1]+1.96*coeff2[ ,2])
lowerCI_reg2 = exp(coeff2[,1]-1.96*coeff2[ ,2])

 cbind(est2,uppeerCI_reg2,lowerCI_reg2)
                              est2 uppeerCI_reg2 lowerCI_reg2
(Intercept)           0.0001512625  0.0005619739 4.071425e-05
total_minutes_on_demo 1.0188616555  1.0223324686 1.015403e+00
user_age              1.0687831096  1.0900755632 1.047907e+00
num_visits            1.3933379303  1.4947801050 1.298780e+00

 confint(reg)
Waiting for profiling to be done...
                            2.5 %      97.5 %
(Intercept)           -8.70601931 -6.85535492
total_minutes_on_demo  0.00629359  0.01322945
num_visits             0.64957155  0.81304099
> confint.default(reg)
                             2.5 %      97.5 %
(Intercept)           -8.683291873 -6.83408585
total_minutes_on_demo  0.006245405  0.01317595
num_visits             0.648051466  0.81141752
> confint(reg2)
Waiting for profiling to be done...
                             2.5 %      97.5 %
(Intercept)           -10.12966632 -7.50370926
total_minutes_on_demo   0.01534092  0.02214634
user_age                0.04694692  0.08641751
num_visits              0.26216984  0.40279148
> confint.default(reg2)
                             2.5 %      97.5 %
(Intercept)           -10.10890818 -7.48407920
total_minutes_on_demo   0.01528527  0.02208669
user_age                0.04679479  0.08624666
num_visits              0.26142670  0.40197782


 demo_email$convert = as.numeric(demo_email$converted)

ggplot(demo_email, aes(x=user_age, y=convert)) + geom_point() + stat_smooth(method="glm", method.args = list(family = "binomial"), se=FALSE)+geom_jitter(height =0.05)


binomial_smooth = function(...) {
   geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
 }

png('plot1.png')
ggplot(demo_email, aes(x=user_age, y=convert)) + geom_point() + stat_smooth(method="glm", method.args = list(family = "binomial"), 
	se=FALSE)+geom_jitter(height =0.05)+binomial_smooth()+ggtitle("Demosource-email  softwarepurchased vs user age")

png('plot2.png')
ggplot(demo_email, aes(x=total_minutes_on_demo, y=convert)) + geom_point() + stat_smooth(method="glm", method.args = list(family = "binomial"), 
	se=FALSE)+geom_jitter(height =0.05)+binomial_smooth()+ggtitle("Demosource-email  softwarepurchased vs total minitues spent on demo")



demo_website$convert = as.numeric(demo_website$converted)

png('plot3.png')
ggplot(demo_website, aes(x=user_age, y=convert)) + geom_point() + stat_smooth(method="glm", method.args = list(family = "binomial"), 
	se=FALSE)+geom_jitter(height =0.05)+binomial_smooth()+ggtitle("Demosource-Website  softwarepurchased vs user_age")

png('plot5.png')

ggplot(demo_website, aes(x=user_age, y=convert)) + geom_point(aes(group=1)) + stat_smooth(method="glm", method.args = list(family = "binomial"), 
	se=FALSE)+geom_jitter(height =0.05)+binomial_smooth()+ggtitle("Demosource-Website  softwarepurchased vs user_age")

png('plot4.png')

ggplot(demo_website, aes(x=total_minutes_on_demo, y=convert)) + geom_point() + stat_smooth(method="glm", method.args = list(family = "binomial"), 
	se=FALSE)+geom_jitter(height =0.05)+binomial_smooth()+ggtitle("Demosource-Website  softwarepurchased vs total minitues spent on demo")
