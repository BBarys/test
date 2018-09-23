A/B Test Of Cancellation Method
================

### 1. Approximate distribution between test and control groups

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

![](Report_files/figure-markdown_github/unnamed-chunk-4-1.png)

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
2.  If *U* ≤ 0.25, then assign client *i* to the test group. Otherwise assign control group.

Let's test this. With the null hypothesis that *p=0.25*, the alternative hypothesis that *p* ≠ 0.25, our test statistic is:

``` r
t_statistic=sqrt(n)*(p-0.25)/Sx
t_statistic
```

    ## [1] -0.9020473

*P-value* of the test is:

``` r
2*pnorm(t_statistic)
```

    ## [1] 0.3670318

Hence we do not reject the null hypothesis if the significance level is &lt;36%. So we conclude that the test group assigned to a particular client follows *Bernoulli(0.25)*.

*(Notice that standard normal distribution was used instead of t-distribution for constructing confidence intervals and testing. This is justified because our sample is large)*

### 2. Effects on generating additional REBILLs

We want to know whether P{at least one more REBILL for a client in group 1}, call it P0, is greater than P{at least one more REBILL for a client in group 0}=P1. It is easy to estimate using the fact that Pi=E\[I{at least one more REBILL for a client in group i}\], i=0,1, where I{...} is and indicator random variable. Let us first merge randomization data with transactions data:

``` r
data=inner_join(transactions, users, by="sample_id")
```

And let us see how many distinct clients are there in each group in the transactions data:

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

So there are 1079 clients in the control group, and 1635 clients in the test group. Now go throught the data and assign I=0 for clients who have not generated any additional REBILLs, and I=1 otherwise:

``` r
data_I=data %>% group_by(sample_id) %>%
  summarise(I=ifelse(sum(transaction_type=="REBILL")>0, 1, 0)) %>%
  full_join(data, by="sample_id")
```

Now compute sample means to estimate E\[I{...}\] for each group:

``` r
means=data_I %>% distinct(sample_id, .keep_all = TRUE) %>%
  group_by(test_group) %>%
  summarise(P=mean(I))
kable(means, format = "markdown", align='c')
```

| test\_group |     P     |
|:-----------:|:---------:|
|      0      | 0.8721038 |
|      1      | 0.9516820 |

Once again, we are dealing with Bernoulli random variables. I{at least one more REBILL for a client in group i}=I\_i~Bernoulli(P\_i), i=0,1. We can easily compute 99% confidence intervals as follows:

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

ggplot(data.frame(I=factor(c(0,1)),
                  P=c(P0, P1),
                  L=c(CI_0[1], CI_1[1]),
                  U=c(CI_0[2], CI_1[2])), aes(x=I, y=P))+
  geom_point(size=3, color="red")+geom_errorbar(aes(ymin=L, ymax=U))
```

![](test/unnamed-chunk-14-1.png)
