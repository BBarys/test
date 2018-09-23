# Code is commented in the markdown document
library("dplyr")
library("knitr")
library("ggplot2")


# Part 1 
users=read.csv("testSamples.csv", header = TRUE)
transactions=read.csv("transData.csv", header = TRUE)

n=dim(users)[1]

summary_users=
  users %>% 
  group_by(test_group) %>%
  summarise(size_of_group=n())
ggplot(summary_users, aes(as.factor(test_group), size_of_group))+
  geom_bar(stat = "identity", aes(fill=as.factor(test_group)))+
  labs(x="Test group", y="Number of Clients")+guides(fill=FALSE)

p=as.numeric(summary_users[2,2])/n
Sx=sqrt(p*(1-p))
ci=c(p-qnorm(0.995)*Sx/sqrt(n), p+qnorm(0.995)*Sx/sqrt(n))
test_statistic=sqrt(n)*(p-0.25)/Sx
p_value=2*pnorm(test_statistic)





# Part 2
data=inner_join(transactions, users, by="sample_id")
summary_transactions=
  data %>%
  group_by(test_group) %>%
  summarise(rebills=sum(transaction_type=="REBILL"),
            chargebacks=sum(transaction_type=="CHARGEBACK"),
            refunds=sum(transaction_type=="REFUND"),
            num_of_transactions=n(),
            num_of_clients=n_distinct(sample_id))
data_I=
  data %>%
  group_by(sample_id) %>%
  summarise(I=ifelse(sum(transaction_type=="REBILL")>0, 1, 0)) %>%
  inner_join(users, by="sample_id")
means=
  data_I %>%
  group_by(test_group) %>%
  summarise(P=mean(I))

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

S_delta=sqrt(S0^2/N0+S1^2/N1)
test_statistic=(P1-P0)/S_delta

# calculate P-value:
pnorm(test_statistic, lower.tail=FALSE)






# Part 3
revenues=
  data %>%
  group_by(sample_id) %>%
  summarise(revenue=sum(transaction_amount)) %>%
  inner_join(users, by="sample_id")
means_sds=
  revenues %>%
  group_by(test_group) %>%
  summarise(mean_revenue=mean(revenue), sd_revenue=sd(revenue))

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

revenues_distinct=revenues %>% distinct(sample_id, .keep_all = TRUE)

# Vectors of revenues:
r0=revenues_distinct %>% filter(test_group==0) %>% select(revenue) %>% unlist %>% as.vector()
r1=revenues_distinct %>% filter(test_group==1) %>% select(revenue) %>% unlist %>% as.vector()
t.test(r0, r1, alternative = "greater")






# Part 4
char_rebill=
  data %>%
  group_by(sample_id) %>%
  summarise(rebills=sum(transaction_type=="REBILL"),
            chargebacks=sum(transaction_type=="CHARGEBACK")) %>%
  inner_join(users, by="sample_id")
char_rebill_summary=
  char_rebill %>%
  group_by(test_group) %>%
  summarise(C_bar=mean(chargebacks),
            R_bar=mean(rebills),
            chargeback_rate=C_bar/R_bar)


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

test_statistic=(C_bar0/R_bar0-C_bar1/R_bar1)/sqrt(Var_C0_by_R0/N0+Var_C1_by_R1/N1)

# P-value:
pnorm(test_statistic, lower.tail=FALSE)
