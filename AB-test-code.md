Referral Program A/B Test
================

<style type="text/css">

body{ /* Normal  */
      font-size: 12px;
  }
td {  /* Table  */
  font-size: 8px;
}
h1.title {
  font-size: 38px;
  color: DarkRed;
}
h1 { /* Header 1 */
  font-size: 28px;
  color: DarkRed;
}
h2 { /* Header 2 */
    font-size: 22px;
  color: DarkBlue;
}
h3 { /* Header 3 */
  font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: Black;
}
code.r{ /* Code block */
    font-size: 12px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 14px;
}
</style>

## Background Information

### Company X has a referral program that allows customers to get paid for referring their friends to the company. It’s a win for everybody because it’s typically cheaper for company X to pay existing customers to refer their friends than it is for them to advertise on Google, Facebook, or anywhere else.

### Company X allows customers to refer other customers. They usually pay $25 to both the sender and receiver when the receiver gets a quote. To get a quote, the receiver must complete the test drive, which typically takes 2-4 weeks.

### Company X ran an AB test to try to increase referrals. ¾ of customers received a promotion after buying a policy that offered them more money per successfully referred quote ($50) for a limited 30-day window. To qualify for the promotion, the receiver has to create their account within the sender’s 30-day window and then eventually go on to get a quote.¼ received the promo immediately after buying a policy, ¼ received it after 2 days, and ¼ received the promo 7 days after buying a policy. The promo lasts 30 days regardless of when it begins. The other ¼ did not receive the promo.

# Question 1

## What is the fairest way to compare the four buckets?

### Read data

``` r
rpp <- read.csv('referral_promo_participants.csv')
pr <- read.csv('promo_referrals.csv')
```

### Merge two dataframes

``` r
dta <- merge(rpp, pr, 
            by.x = names(rpp)[1], by.y = names(pr)[1],
            all = TRUE, all.x = TRUE)
```

### Convert to date format

``` r
dta$bucket_timestamp <- strptime(dta$bucket_timestamp, "%Y-%m-%d %H:%M:%S")
dta$receiver_account_timestamp <- strptime(dta$receiver_account_timestamp, "%Y-%m-%d %H:%M:%S")
dta$receiver_quote_timestamp <- strptime(dta$receiver_quote_timestamp, "%Y-%m-%d %H:%M:%S")
dta$receiver_policy_timestamp <- strptime(dta$receiver_policy_timestamp, "%Y-%m-%d %H:%M:%S")
```

## Key Findings: fairest way to compare

### There are three phases in the referral process: creating an account, receiving a quote, and purchasing a policy. At each stage, we have customers who has not moved onto the next step. The percentage of customers who has stayed is the conversion rate. The fairest way to compare the four buckets is to compare the conversion rate in the three promotion buckets with the conversion rate in the control bucket.

# Question 2

## In terms of generating referral activity (accounts, quotes, and policies) during the experiment, which variant of the test was most successful? Why do you think that is?

### Calculate account creation rate

``` r
rate <- function (dataframe) {
# calculate number of conversions at each stage
  ttl <- nrow(dataframe)
  act <- sum(dataframe[, 'receiver_account'], na.rm = TRUE)
  quote <- sum(dataframe[, 'receiver_quote'], na.rm = TRUE)
  policy <- sum(dataframe[, 'receiver_policy'], na.rm = TRUE)

  # calculate conversion rates
  act.rt <- act/ttl
  quote.rt <- quote/act
  policy.rt <- policy/act
  quote.rt.ttl <- quote/ttl
  policy.rt.ttl <- policy/ttl

  # combines data
  rt <- c(act.rt, 
          quote.rt, 
          policy.rt, 
          quote.rt.ttl,
          policy.rt.ttl)

  return(round(rt, 3))
}
```

### Format table

``` r
conv.rt.org <- data.frame(rbind(rate(dta[dta$bucket == 'off',]),
                            rate(dta[dta$bucket == '0hr',]),
                            rate(dta[dta$bucket == '48hr',]),
                            rate(dta[dta$bucket == '168hr',])))
rownames(conv.rt.org) <- c('off', '0hr', '48hr', '168hr')
names(conv.rt.org) <- c('account/total',
                    'quote/account',
                    'policy/quote',
                    'quote/total',
                    'policy/total')
```

### Summary table of the conversion rates

``` r
conv.rt.org
```

    ##       account/total quote/account policy/quote quote/total policy/total
    ## off           0.225         0.373        0.133       0.084        0.030
    ## 0hr           0.326         0.380        0.130       0.124        0.042
    ## 48hr          0.296         0.308        0.097       0.091        0.029
    ## 168hr         0.302         0.339        0.089       0.102        0.027

## Key Findings: best result of the A/B test

### In terms of generating account and quote, 0hr bucket has the highest rate among all – 32.6% of the users have generated an account creation and 38% of the accounts have received a quote. In terms of generating policy, off bucket performed the best, with 0hr bucket following with a slightly lower rate. 13.3% of the off bucket quoted users and 13% of the 0hr bucket quoted users have purchased a policy. Noise in the data can cause the 0.3% difference between off and 0hr bucket. Since referral activity is defined as receiving a quote, 0hr bucket is the most successful – 12.4% of the users have generated a quote.

# Question 3

## Consider the fact that we’re paying more money per referred quote during the promo variants. How would you evaluate the tradeoff between more referral activity and more cost?

### Calculate the revenue for the per increased customer

``` r
revenue <- (sum(dta[dta$bucket == '0hr', 'receiver_policy'],na.rm = TRUE)/nrow(dta[dta$bucket == '0hr',]) - 
              sum(dta[dta$bucket == 'off', 'receiver_policy'],na.rm = TRUE)/nrow(dta[dta$bucket == 'off',])) * 1387.5
paste('Revenue Increased Per User: $', round(revenue, 2), sep='')
```

    ## [1] "Revenue Increased Per User: $17.08"

### Calculate the cost for the per increased customer

``` r
cost <- sum(dta[dta$bucket == '0hr', 'sender_earned_amount_in_dollars'],na.rm = TRUE)*2/nrow(dta[dta$bucket == '0hr',]) -
  sum(dta[dta$bucket == 'off', 'sender_earned_amount_in_dollars'],na.rm = TRUE)*2/nrow(dta[dta$bucket == 'off',])
paste('Cost Increased Per User: $', round(cost, 2), sep='')
```

    ## [1] "Cost Increased Per User: $6.79"

### Net income increase per customer

``` r
paste('Net Income Increased Per User: $', round(revenue - cost, 2), sep='')
```

    ## [1] "Net Income Increased Per User: $10.29"

## Key Findings: tradeoff between referral and cost

### We can evaluate the tradeoff between the revenue and the cost of referral activity. For one additional customer, if the increase in revenue is more than the increase in cost, the promotion buckets will bring more profits than control bucket. In this case, we compare 0hr bucket, the most successful bucket, with off bucket, the control bucket. The average cost per user of 0hr bucket is $6.79 higher than that of off bucket.

### For revenue, we compare the difference in policy rate of the two buckets, and then multiply the average car insurance revenue per user. We make the assumptions that the new customer will purchase an insurance of average price and stay with us for one year. The average car insurance price ranges from $1348 to $1427. We use the average of the range, $1387.5, to calculate the increase in revenue. The average revenue per user of 0hr bucket is $17.08 higher than that of off bucket.

### The increase of net income per user generated by the 0hr bucket promotion is the difference of the increase in revenue and cost: $17.08 - $6.79 = $10.29. Since the net income is positive, we should promote.

# Question 4

## Suppose today is 5/08/2018 at 2:15 PM (when the data was pulled). Based on youranswers to questions 1-4, what should we do right now? Do you think we should roll out one of the four variants to everyone? Do you think we should have turned the test off earlier, should we turn the test off now, or should we leave it on to keep collecting more data? Why?

## Key Findings:

### Based on the previous finding, we should roll out 0hr bucket to everyone because it has the highest referral rate and generates more net income than the control bucket. However, the results might not be statistically significant due to the small sample size. Noise in small samples can result in spurious findings. The sample size is important because the more data we have, the impact of randomness will be less. Therefore, we should calculate the sample size needed to have enough confidence in our findings. There are four metrics we should consider: baseline conversion rate, minimum detectable effect, significant level, and statistical power.

### Our ultimate goal is to increase the number of quotes in the whole population, thus we need to compare if the difference in the quote rate, the total number of quotes divided by the total number of users, is significant.

### We set the significance level to 5% ,which means we accept the 5% chance that the difference in conversion rate is detected, but actually does not exist. We set the statistical power to 80%, which means we accept the 20% chance that we have failed to discover the actually existed difference in conversion rate. We choose a stricter significance level than the statistical power, because the cost of rolling out a falsely discovered promotion campaign is much higher than the cost of not rolling out a promotion campaign.

### Our baseline, off bucket, quote rate is 8.4%. The 0hr bucket quote rate is 12.4%. If we set the absolute minimum detectable effect (MDE) to the difference between the two buckets, 4%, we will need a sample size of 800 per variant. Our current sample size of one variant is 1000, which is large enough to detect 2.5% absolute MDE. Therefore, we conclude the increase in quote rate of the 0hr bucket is significant.

### However, we cannot reach significant conclusion with the 48hr and 168hr bucket. The difference in quote rate between the 48hr bucket and off bucket is 0.7%. To detect a 0.7% absolute MDE requires a sample size around 25,000. The difference in quote rate between the 168hr bucket and off bucket is 1.8%. To detect a 1.8% absolute MDE requires a sample size around 3,833. Currently we only have around 1000 per bucket, therefore we need to collect more data to reduce the impact of randomness on the results of 48hr and 168hr bucket.

# Question 5

## After seeing this data, what other kinds of things would you test in the future?

### Filter data by bucket

### Account creation time within the promotion time window

``` r
bkt.flt <- function(bkt) {
  if (bkt == '0hr') {
    tmp <- dta[dta$bucket == '0hr',]
  } else if (bkt == '48hr') {
    hr48 <- dta[dta$bucket == '48hr',]
    tmp <- hr48[(hr48$receiver_account_timestamp >= (hr48$bucket_timestamp + 2*24*3600))    
                & (hr48$receiver_account_timestamp <= (hr48$bucket_timestamp + 32*24*3600))
                ,]
    tmp <- tmp[!is.na(tmp$user_id),]
  } else if (bkt == '168hr') {
    hr48 <- dta[dta$bucket == '168hr',]
    tmp <- hr48[(hr48$receiver_account_timestamp >= (hr48$bucket_timestamp + 7*24*3600)) 
                & (hr48$receiver_account_timestamp <= (hr48$bucket_timestamp + 37*24*3600))
                ,]
    tmp <- tmp[!is.na(tmp$user_id),]
  }
  return(tmp)
}
```

### Re-bucketed data

``` r
qt.0hr <- bkt.flt('0hr')
qt.48hr <- rbind(bkt.flt('48hr'), dta[dta$bucket == '48hr' & is.na(dta$receiver_account),])
qt.168hr <- rbind(bkt.flt('168hr'), dta[dta$bucket == '168hr' & is.na(dta$receiver_account),])
off <- dta[!(rownames(dta) %in% c(rownames(qt.0hr),
                                  rownames(qt.48hr),
                                  rownames(qt.168hr))),]
```

### Format table

``` r
conv.rt.re <- data.frame(rbind(rate(off),
                               rate(qt.0hr),
                               rate(qt.48hr),
                               rate(qt.168hr)))
rownames(conv.rt.re) <- c('off', '0hr', '48hr', '168hr')
names(conv.rt.re) <- c('account/total',
                       'quote/account',
                       'policy/quote',
                       'quote/total',
                       'policy/total')
```

### Summary table of re-bucket conversion rate

``` r
conv.rt.re
```

    ##       account/total quote/account policy/quote quote/total policy/total
    ## off           0.422         0.342        0.108       0.144        0.045
    ## 0hr           0.326         0.380        0.130       0.124        0.042
    ## 48hr          0.169         0.331        0.117       0.056        0.020
    ## 168hr         0.143         0.316        0.068       0.045        0.010

## Key Findings:

### 1\. The current bucketing cannot clearly isolate the delay effect of the promotion, because we change the time and promotion at the same time. Users who are assigned to 48hr bucket and 168hr bucket do not know their assignments. Many of them have referred friends before the promotion taking effect and ended up with earning $25 per person, which they theoretically are the same as users in the off bucket.

### Based on this finding, we re-bucketed the four groups through filtering out users that fell into their corresponding promotion time window. From the re-bucketed results, we found out that off bucket has the highest quote rate and highest policy rate in total. This contradicts our earlier findings.

### The main reason of off bucket having the highest account creation rate because many customers in the 48hr bucket and in 168hr bucket did not know they would receive referral promotions. If they had known, they would likely have referred within the promotion time window. These “mis-bucketed” customers increased the number of account creations in the off bucket, resulting in a higher rate of account creation rate for off bucket. In future tests, we suggest changing in the way of bucketing users to better isolate the effect of delayed promotion and the increased in referral reward. For example, we hold the referral options in users of the 48hr and 168hr bucket until 48 hours and 168 hours after they have bought their policy. When they have the referral options, the referral reward begins at $50.

### 2\. We observe the dramatic decrease in the conversion rate from quote to policy. Since our end goal is to increase the number of people who eventually purchased the policy after getting a quote, we want to increase the quote to policy conversion rate. Currently, the sender received amount and the receiver received amount is the same. We could try to adjust the distribution of promotion amount between the two parties while keeping the total amount at $100. For example, we can give sender $25 and receiver $75, because the receiver takes a long process to pass the test and eventually purchase the policy. Such a process is more time-consuming than the sending out the referral by the sender. Therefore, receiver should get more incentives to complete the process.

### 3\. We can differentiate people who did not participate in the test drive and people who did not finish the test drive or did not qualified for the quote. By separating these two groups, we can send reminders to customers who have not started test drive to increase the test drive completion rate, therefore increasing the quote rate.
