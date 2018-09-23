A/B Test Of Cancellation Method
================

## 1. Approximate distribution between test and control groups


All the `csv` files are in the same directory as the **R** project. This code uses `dplyr`, `knitr` and `ggplot2` packages. Import them and read the data using the `read.csv` function:

``` r
library("dplyr")
library("knitr")
library("ggplot2")
users=read.csv("testSamples.csv", header = TRUE)
transactions=read.csv("transData.csv", header = TRUE)
```

There were 59721 users who went to the cancellation page:

``` r
n=dim(users)[1]
n
```

    ## [1] 59721

Let's see how many of them where assigned to each group (0: control group, 1: test group):

``` r
summary_users=
  users %>% 
  group_by(test_group) %>%
  summarise(size_of_group=n())

kable(summary_users, format = "markdown", align='c')
```

| test\_group | size\_of\_group |
|:-----------:|:---------------:|
|      0      |      44886      |
|      1      |      14835      |

``` r
ggplot(summary_users, aes(as.factor(test_group), size_of_group))+
  geom_bar(stat = "identity", aes(fill=as.factor(test_group)))+
  labs(x="Test group", y="Number of Clients")+guides(fill=FALSE)
```

![](https://github.com/BBarys/test/blob/master/unnamed-chunk-4-1.png)

We assume that assignment of each client to a particular group is independent Bernoulli random variable with probability *p* of a client being assigned to the test group. We know that *p=E\[X\]*, where *X~Bernoulli(p)*. An estimator of *E\[X\]* is the sample mean of *x*<sub>*i*</sub>'s, where *x*<sub>*i*</sub> is 1 if a client *i* is in the test group, and zero otherwise. This simply reduces to the proportion of clients who were assigned to the test group:

``` r
p=as.numeric(summary_users[2,2])/n
p
```

    ## [1] 0.2484051

Mle *S*<sub>*x*</sub> of *σ*<sub>*X*</sub>

``` r
Sx=sqrt(p*(1-p))
Sx
```

    ## [1] 0.432088

Now all is ready for inference about *p*. We construct 99% confidence interval as follows:

``` r
ci=c(p-qnorm(0.995)*Sx/sqrt(n), p+qnorm(0.995)*Sx/sqrt(n))
ci
```

    ## [1] 0.2438507 0.2529594

It looks like the clients where assigned to a particular group using the following algorithm:

1.  Generate *U~Uniform(0,1)*;
2.  If *U* ≤ 0.25, then assign client *i* to the test group. Otherwise assign to control group.

Let's test this. With the null hypothesis that *p=0.25*, the alternative hypothesis that *p* ≠ 0.25, our test statistic is:

``` r
test_statistic=sqrt(n)*(p-0.25)/Sx
test_statistic
```

    ## [1] -0.9020473

*P-value* of the test is:

``` r
2*pnorm(test_statistic)
```

    ## [1] 0.3670318

Hence we do not reject the null hypothesis if the significance level is &lt;36%. So we conclude that the test group assigned to a particular client follows *Bernoulli(0.25)*.

*(Notice that standard normal distribution was used instead of t-distribution for constructing confidence intervals and testing. This is justified because our sample is large)*

## 2. Effects on generating additional REBILLs


We want to know whether *P{at least one more REBILL for a client in group 0}*, call it *P*<sub>0</sub>, is greater than *P{at least one more REBILL for a client in group 1}=* *P*<sub>1</sub>. It is easy to estimate using the fact that *P*<sub>*i*</sub> *=E\[I{at least one more REBILL for a client in group i}\]*, *i=0,1*, where *I{...}* is and indicator random variable. Let us first merge randomization data with transactions data:

``` r
data=inner_join(transactions, users, by="sample_id")
```

And let us see how many distinct clients are there in each group in the transactions data (`num_of_clients` column):

``` r
summary_transactions=
  data %>%
  group_by(test_group) %>%
  summarise(rebills=sum(transaction_type=="REBILL"),
            chargebacks=sum(transaction_type=="CHARGEBACK"),
            refunds=sum(transaction_type=="REFUND"),
            num_of_transactions=n(),
            num_of_clients=n_distinct(sample_id))
kable(summary_transactions, format = "markdown", align='c')
```

| test\_group | rebills | chargebacks | refunds | num\_of\_transactions | num\_of\_clients |
|:-----------:|:-------:|:-----------:|:-------:|:---------------------:|:----------------:|
|      0      |   3756  |     106     |   188   |          4050         |       1079       |
|      1      |   3205  |      57     |   118   |          3380         |       1635       |

So there are 1079 clients in the control group, and 1635 clients in the test group. Now go throught the data and assign *I=0* for clients who have not generated any additional REBILLs, and *I=1* otherwise:

``` r
data_I=
  data %>%
  group_by(sample_id) %>%
  summarise(I=ifelse(sum(transaction_type=="REBILL")>0, 1, 0)) %>%
  inner_join(users, by="sample_id")
```

Now compute sample means to estimate *E\[I{...}\]* for each group:

``` r
means=
  data_I %>%
  group_by(test_group) %>%
  summarise(P=mean(I))
kable(means, format = "markdown", align='c')
```

| test\_group |     P     |
|:-----------:|:---------:|
|      0      | 0.8721038 |
|      1      | 0.9516820 |

Once again, we are dealing with Bernoulli random variables. *I{at least one more REBILL for a client in group i}=I\_i* ~ *Bernoulli*(*P*<sub>*i*</sub>), *i=0,1*. We can easily compute 99% confidence intervals as follows:

``` r
N0=summary_transactions[1,6] %>% as.numeric()
N1=summary_transactions[2,6] %>% as.numeric()

P0=means[1,2] %>% as.numeric()
P1=means[2,2] %>% as.numeric()

# ML estimates of standard deviations of P0 and P1
S0=sqrt(P0*(1-P0))
S1=sqrt(P1*(1-P1))

# 99% confidence intervals
CI_0=c(P0-qnorm(0.995)*S0/sqrt(N0), P0+qnorm(0.995)*S0/sqrt(N0))
CI_1=c(P1-qnorm(0.995)*S1/sqrt(N1), P1+qnorm(0.995)*S1/sqrt(N1))

ggplot(data.frame(i=factor(c(0,1)),
                  P=c(P0, P1),
                  L=c(CI_0[1], CI_1[1]),
                  U=c(CI_0[2], CI_1[2])), aes(x=i, y=P))+
  geom_point(size=3, color="red")+geom_errorbar(aes(ymin=L, ymax=U))
```

![](https://github.com/BBarys/test/blob/master/unnamed-chunk-14-1.png)

It seems like probabilities are statistically different because intervals do not overlap. However the graph doesn't account for variability in location of the two means. So we need to do the test, with *H*<sub>0</sub>: *P*<sub>1</sub> − *P*<sub>0</sub> ≤ 0, and *H*<sub>*a*</sub>: *P*<sub>1</sub> − *P*<sub>0</sub> &gt; 0. Compute the test statistic:

``` r
S_delta=sqrt(S0^2/N0+S1^2/N1)
test_statistic=(P1-P0)/S_delta

# calculate P-value:
pnorm(test_statistic, lower.tail=FALSE)
```

    ## [1] 1.965631e-12

With such a small P-value we reject the null hypothesis and conclude that indeed, clients in the test group are more likely to generate at least one additional REBILL than clients in the control group.

## 3. Effect on revenues


Let us calculate the total transaction amount for each user:

``` r
revenues=
  data %>%
  group_by(sample_id) %>%
  summarise(revenue=sum(transaction_amount)) %>%
  inner_join(users, by="sample_id")
```

Compute the mean and standard deviation for each of the test groups:

``` r
means_sds=
  revenues %>%
  group_by(test_group) %>%
  summarise(mean_revenue=mean(revenue), sd_revenue=sd(revenue))
kable(means_sds, format="markdown", align='c')
```

| test\_group | mean\_revenue | sd\_revenue |
|:-----------:|:-------------:|:-----------:|
|      0      |    83.26126   |  103.13254  |
|      1      |    58.36911   |   54.45464  |

Let us compute 99% confidence intervals for revenue of group 0 and group 1, *R*<sub>0</sub> and *R*<sub>1</sub> respectively:

``` r
R0=means_sds[1,2] %>% as.numeric()
R1=means_sds[2,2] %>% as.numeric()

S0=means_sds[1,3] %>% as.numeric()
S1=means_sds[2,3] %>% as.numeric()

# 99% confidence intervals
CI_0=c(R0-qt(0.995, df=N0-1)*S0/sqrt(N0), R0+qt(0.995, df=N0-1)*S0/sqrt(N0))
CI_1=c(R1-qt(0.995, df=N1-1)*S1/sqrt(N1), R1+qt(0.995, df=N1-1)*S1/sqrt(N1))

ggplot(data.frame(i=factor(c(0,1)),
                  R=c(R0, R1),
                  L=c(CI_0[1], CI_1[1]),
                  U=c(CI_0[2], CI_1[2])), aes(x=i, y=R))+
  geom_point(size=3, color="red")+geom_errorbar(aes(ymin=L, ymax=U))
```

![](https://github.com/BBarys/test/blob/master/unnamed-chunk-18-1.png)

It seems like *R*<sub>0</sub> and *R*<sub>1</sub> are statistically different. *R*<sub>0</sub> is greater, even though *P*<sub>0</sub> was smaller, which suggests that although clients who must call in order to cancel are more likely to generate at least one additional REBILL, this "barrier" to cancellation tends to keep clients that are less "enthusiastic".

Test whether *R*<sub>0</sub> and *R*<sub>1</sub> are different: let *H*<sub>0</sub> be a hypothesis that *R*<sub>0</sub> − *R*<sub>1</sub> ≤ 0 and *H*<sub>*a*</sub> is *R*<sub>0</sub> − *R*<sub>1</sub> &gt; 0:

``` r
revenues_distinct=revenues %>% distinct(sample_id, .keep_all = TRUE)

# Vectors of revenues:
r0=revenues_distinct %>% filter(test_group==0) %>% select(revenue) %>% unlist %>% as.vector()
r1=revenues_distinct %>% filter(test_group==1) %>% select(revenue) %>% unlist %>% as.vector()
t.test(r0, r1, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  r0 and r1
    ## t = 7.2863, df = 1478.2, p-value = 2.583e-13
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  19.26928      Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  83.26126  58.36911

P-value is very close to zero, so we reject *H*<sub>0</sub> and conclude that indeed, clients from the control group, on average, generate more revenues than clients in the test group.

## 4. Effect on chargeback rate


Let us calculate the number of CHARGEBACKs and REBILLs for each client:

``` r
char_rebill=
  data %>%
  group_by(sample_id) %>%
  summarise(rebills=sum(transaction_type=="REBILL"),
            chargebacks=sum(transaction_type=="CHARGEBACK")) %>%
  inner_join(users, by="sample_id")
```

If we would calculate a chargeback rate individually for each client we would get divisions by zero. Instead let's estimate chargeback rate for each group by dividing the mean number of CHARGEBACKs, *C*<sub>*b**a**r*</sub>, by the mean number of REBILLs, *R*<sub>*b**a**r*</sub>:

``` r
char_rebill_summary=
  char_rebill %>%
  group_by(test_group) %>%
  summarise(C_bar=mean(chargebacks),
            R_bar=mean(rebills),
            chargeback_rate=C_bar/R_bar)
kable(char_rebill_summary, format = "markdown", align='c')
```

| test\_group |   C\_bar  |  R\_bar  | chargeback\_rate |
|:-----------:|:---------:|:--------:|:----------------:|
|      0      | 0.0982391 | 3.481001 |     0.0282215    |
|      1      | 0.0348624 | 1.960245 |     0.0177847    |

Actually we could have used the `summary_transactions` table to compute this. But we are going to need `char_rebill` data to compute variance-covariance matrix to do hypothesis testing. Using delta method (see [here](https://en.wikipedia.org/wiki/Delta_method#Multivariate_delta_method)) with *h*(*C*<sub>*b**a**r*</sub>, *R*<sub>*b**a**r*</sub>)=*C*<sub>*b**a**r*</sub>/*R*<sub>*b**a**r*</sub> we can find an approximate normal distribution for the chargeback rates, and then do the upper tail test for the difference in means:

``` r
R_bar0=char_rebill_summary[1,3] %>% as.numeric()
R_bar1=char_rebill_summary[2,3] %>% as.numeric()

C_bar0=char_rebill_summary[1,2] %>% as.numeric()
C_bar1=char_rebill_summary[2,2] %>% as.numeric()

# Vectors C and R for each group
C0=char_rebill %>% filter(test_group==0) %>% select(chargebacks) %>% unlist() %>% as.vector()
C1=char_rebill %>% filter(test_group==1) %>% select(chargebacks) %>% unlist() %>% as.vector()
R0=char_rebill %>% filter(test_group==0) %>% select(rebills) %>% unlist() %>% as.vector()
R1=char_rebill %>% filter(test_group==1) %>% select(rebills) %>% unlist() %>% as.vector()

# Compute variance estimates of the ratios (formulas are found using delta method):
Var_C0_by_R0=var(C0)/(N0*R_bar0^2)-2*cov(C0, R0)*C_bar0/(R_bar0^3)+var(R0)*C_bar0^2/(N0*R_bar0^4)

Var_C1_by_R1=var(C1)/(N1*R_bar1^2)-2*cov(C1, R1)*C_bar1/(R_bar1^3)+var(R1)*C_bar1^2/(N1*R_bar1^4)
```

Now we are ready to calculate confidence intervals (*α* = 0.01) for the chargeback rates:

``` r
CI_0=c(C_bar0/R_bar0-qnorm(0.995)*sqrt(Var_C0_by_R0/N0),
       C_bar0/R_bar0+qnorm(0.995)*sqrt(Var_C0_by_R0/N0))
CI_1=c(C_bar1/R_bar1-qnorm(0.995)*sqrt(Var_C1_by_R1/N1),
       C_bar1/R_bar1+qnorm(0.995)*sqrt(Var_C1_by_R1/N1))

ggplot(data.frame(i=factor(c(0,1)),
                  Rate=c(C_bar0/R_bar0, C_bar1/R_bar1),
                  L=c(CI_0[1], CI_1[1]),
                  U=c(CI_0[2], CI_1[2])), aes(x=i, y=Rate))+
  geom_point(size=3, color="red")+geom_errorbar(aes(ymin=L, ymax=U))+
  labs(title="Chargeback rate")
```

![](https://github.com/BBarys/test/blob/master/unnamed-chunk-23-1.png)

Now do the test with *H*<sub>0</sub>: *C*<sub>0</sub>/*R*<sub>0</sub> − *C*<sub>1</sub>/*R*<sub>1</sub> ≤ 0, and *H*<sub>*a*</sub>: *C*<sub>0</sub>/*R*<sub>0</sub> − *C*<sub>1</sub>/*R*<sub>1</sub> &gt; 0:

``` r
test_statistic=(C_bar0/R_bar0-C_bar1/R_bar1)/sqrt(Var_C0_by_R0/N0+Var_C1_by_R1/N1)

# P-value:
pnorm(test_statistic, lower.tail=FALSE)
```

    ## [1] 1.612626e-15

So we reject *H*<sub>0</sub>, and conclude that the chargeback rate for the clients who don't have to call is higher
