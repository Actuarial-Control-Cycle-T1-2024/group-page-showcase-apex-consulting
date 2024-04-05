# Apex Consulting: Superlife Health Interventions <img src = "Screenshot 2024-04-05 144120.png" alt = "image" width="100" height= "100" align = "right"> 

## Health Incentives: Reduction in Mortality 

##### Prepared by: Apex Consultants<br><br> Tobby McCann, Grace Lam, Nathan Tang, Magdalene Che and Samuel Lam <br>
<font size="2"> *For all R code please click the 'Code and Calculations' Image* </font> [<img src= "Screenshot 2024-04-05 140531.png" alt = "image" align = "right">](https://github.com/Actuarial-Control-Cycle-T1-2024/group-page-showcase-apex-consulting/tree/main/ACTL4001%20Files) 
---

### Table of Contents
1. [Overview](#Overview)
    + [Executive Summary](#Executive_Summary)
    + [Program Objectives](#Program_Objectives)
    + [Program Metrics](#Program_Metrics) 
2. [Program Design](#Program_Design)  
    + [Gamified Health Points System](#Gamified_Health_Points_System) 
    + [Justification](#Justification) 
    + [Expected Program Uptake](#Expected_Program_Uptake)
    + [Impact of Health Incentives](#Impact_of_Health_Incentives)  
    + [Implementation Timeline](#Implementation_Timeline) 
    + [Monitoring Outcomes](#Monitoring_Outcomes)  
3.	[Pricing and Costs](#Pricing_and_Costs)  
    + [Methodology](#Methodology) 
    + [Results](#Results)  
4.	[Assumptions](#Assumptions)  
5.	[Risk Mitigation & Other Considerations](#Risk_Mitigation_&_Other_Considerations)  
    + [Risk Assessment & Mitigation](#Risk_Assessment_&_Mitigation)  
    + [Sensitivity Analysis](#Sensitivity_Analysis)  
6.	[Data & Limitations](#Data_&_Limitations)
7.	[Final Recommendations](#Final_Recommendations)
8.	[Bibliography](#Bibliography)
9.	[Appendix](#Appendix)


## 1) Overview <a id="Overview"></a>

#### 1.1) Executive Summary <a id="Executive_Summary"></a>

A major Lumarian life insurer, SuperLife, has engaged Apex Consultants to develop a health incentive program that can be bundled with SuperLife’s longer-term life insurance offerings. In addition to promoting the overall health and wellbeing of its policyholders, SuperLife is also seeking to improve the profitability of its products through a reduction in expected mortality and improved product marketability. In light of these considerations, Apex Consultants proposes a Gamified Health Challenge app (“App”) which can be paired with SuperLife’s 20year term life insurance product. Through the App, policyholders are incentivised to increase and track their physical activity levels through the accumulation of ‘Health Points’ upon engaging with different health-promoting behaviours. In evaluating the merits of this proposal, this report leverages the Actuarial Control Cycle as a framework through which to iteratively design, test and monitor the effectiveness of the proposed App. As such, this report discusses the program design, modelling assumptions and outputs, and key metrics for future monitoring.  

#### 1.2) Program Objectives <a id="Program_Objectives"></a>
While this report focuses on pairing the App with the 20-year term insurance product, it is designed to be scalable for potential expansion to include pairing with the Single Premium Whole Life product once it has proven effective in the following facets: satisfactory reduction in mortality, sufficient return on investment, and meeting additional objectives as defined in Section 1.3 below.      

#### 1.3) Program Metrics <a id="Program_Metrics"></a>
The effectiveness of the Program should be assessed through the different metrics that are broken down by demographic segments (i.e. age cohort, sex, Lumaria region, and face amount). In understanding how the following metrics vary across different segments, SuperLife can derive insights into the program’s impact and accordingly adjust strategies to optimise effectiveness and reach. Apex Consultants proposes the following metrics:  

> ##### a) Short-Medium Term 
>  - App adoption rate: a high adoption rate indicates strong interest in the program.
>  - Average Health Point accumulation per user: given that policyholders accumulate points when they complete health-promoting activities through the app, this metric is an indicator of user engagement and provides insights into the extent of active utilisation.
>  - Increase in sales of 20-year term insurance product: increased product differentiation with other life insurers should drive sustainable sales growth in a competitive market.  

> ##### b) Long Term  
>  - Long-term reduction in mortality rate: the success of the program is ultimately underpinned by the extent to which the mortality rate reduces. SuperLife should compare mortality trends between policyholders who regularly engage with the App, policyholders who engage with the App intermittently, and non-participants.  
>  - Decrease in lapse rate: if the App is successful in fostering a sense of community amongst policyholders who also perceive the program’s positive impact on their wellbeing, there should be a decrease in lapse rates.    

## 2) Program Design <a id="Program_Design"></a>
#### 2.1) Gamified Health Points System <a id="Gamified_Health_Points_System"></a>
Apex Consultants recommends the adoption of a Gamified Health Challenge app (“App”) to incentivise healthier lifestyles. Through the App, policyholders can earn virtual ‘Health Points’ by engaging in health-promoting activities in the following categories: Physical Health, Mindfulness & Emotional Wellbeing, Lifestyle & Community, and Prevention. A comprehensive list of specific health initiatives within each category is shown in Appendix A.2. These Health Points allow policyholders to compete against each other and can also be used to unlock various rewards, with a higher point accumulation leading to better rewards (see Appendix A.6 and A.7 for more details on Health Points system and rewards respectively).  

The above figrues depicts a prototype of the App which allows policyholders to monitor their healthpromoting behaviours and track their accumulation of Health Points. Through the App, SuperLife can also continually update their data about each policyholder’s underwriting risk through real-time data collection.  
 
To incentivise specific behaviours by different age cohorts, Health Points allocations should be adjusted depending on the age cohort. For instance, Safety Campaigns, which are designed to educate policyholders on safety measures at home and in daily activities, are likely to be more effective for retired policyholders (aged >65 years old) who often spend more time at home. As such, this age cohort will be awarded more points for engaging in Safety Campaigns compared to a younger policyholder. In this way, SuperLife can maximise the cost effectiveness of each health incentive by ensuring that it is aligned to the unique needs of each age cohort. 

#### 2.2) Justification <a id="Justification"></a>
Each category of health-promoting behaviours (as listed in Section 2.1) is designed to address one of the leading causes of mortality within SuperLife’s policyholder population. Within each category, the specific health-promoting activities have either been included due to their costeffectiveness in reducing mortality and / or its alignment with specific policyholder features.  The causes of death were approximately uniform across different face amounts. As such, Apex consultants did not tailor intervention strategies to specific face amounts, instead focusing on mortality trends across the whole portfolio.  

#### 2.2.1) Selecting Categories
While there are different causes of death in the in-force dataset, neoplasms and respiratory illnesses accounted for an overwhelming 33.5% and 30.0% of deaths respectively (see Appendix B for further details). Alarmingly, 59.5% of policyholders who died from neoplasms were classified as a ‘low’ or ‘very low’ risk at the time of underwriting, suggesting that SuperLife does not currently have an accurate mechanism to test a policyholder’s likelihood of contracting cancer. The World Health Organisation (WHO) suggests that prevention is the most cost-effective long-term strategy for the control of cancer and estimates that 30-50% of all cancer cases are preventable through early screening (WHO, n.d.). In light of these findings, Apex Consultants has selected ‘Prevention’ as one of the categories to encourage policyholders to participate in cancer prevention initiatives and preventing screenings.

In addition to preventing respiratory illnesses through activities in the ‘Prevention’ category (e.g. smoking cessation programs), Apex Consultants has also selected ‘Physical Health’ in response to high incidences of respiratory diseases in SuperLife’s policyholders. Medical Studies conducted by the European Respiratory Society have concluded that regular physical activity, reduction in obesity, and better nutrition is effective for the long-term management of respiratory diseases (Ambrosino & Bertella, 2018).  

Additionally, the categories ‘Community & Lifestyle’ and ‘Mindfulness & Emotional Wellbeing’ were also selected to encourage policyholders to adopt a holistic approach to healthy living. By uplifting the overall wellbeing of policyholders, these categories contribute to the holistic management of physical conditions. Indeed, emerging research demonstrates that some acute respiratory illnesses can be stress-induced (Asthma New Zealand, 2024).  

#### 2.2.2) Selecting Specific Health-Promoting Activities 
Within each specific category, health-promoting activities from the list of interventions supplied by SuperLife’s product development team were included due to their cost-effectiveness in reducing mortality per Crown. The cost and expected reduction in mortality were assumed to vary linearly between the lower and upper bounds. See Appendix A.3 for further details. Additionally, in recognising that T20 policyholders tend have an average age of 50.9 (slightly younger than the average of 54 across SuperLife’s portfolio), there are activities that are targeted at policyholders in the 40-50 age cohort. For instance, external causes of mortality (i.e. accidents or transport-related injuries) are the third leading cause of death for T20 policyholders. Accordingly, Safety Campaigns and Travel Safety Tips have been included.  

#### 2.3) Expected Program Uptake <a id="Expected_Program_Uptake"></a>
The App adopts a holistic approach to wellbeing and gamifies the experience by allowing policyholders to accumulate Health Points upon successfully completing various healthpromoting activities. In acknowledging the diversity of the policyholders, who vary by gender, age and region, the App provides them with the flexibility to choose from a range of activities that align with their needs. In addition to incentivising sustainable long-term engagement by offering a wide variety of activities, the gamified aspect of the App also fosters a sense of community by promoting healthy competition among policyholders through features such as daily and weekly leaderboards (see Figure 1 above). Participation is also further incentivised through the rewards system, wherein policyholders can redeem their Health Points in exchange for different rewards. This incentive-based approach is supported by external literature which suggests that incentives are effective in motivating physical activity (Farooqui, 2014).

#### 2.4) Impact of Health Initiatives <a id="Impact_of_health_initiatives"></a>
The impact of the interventions was simulated using a compound Poisson distribution to stochastically capture the expected uptake and reduction in mortality. The methodology is further explained in Section 3. Our results demonstrate that policyholders in all age cohorts will have at least one category in which a majority is expected to participate (see tables below).   
 
#### Reduction in Mortality (by Category and Pecentile)

|Category | 5th Percentile | Mean | 95th Percentile|
|---------|----------------|------|----------------|
|Health   |	   3.26% 	  |4.40% | 	6.26% |	
|Lifestyle |	2.21% | 	2.89% | 4.00% |	 
|Mental | 	2.56% |	3.57% | 	5.31% |	 	 
|Physical| 	2.05%| 	2.69%| 	3.72%| 	 	

#### Expected Participation Proportion

| Category | 18-34 | 35-49 | 50-64 | 65+ |
|----------|-------|-------|-------|-----|
|Health |	23%  |	46% |	68% |	91% |
|Lifestyle | 	16% | 	49% | 	65% | 	65% | 
|Mental  | 	37% | 	50% |	25% | 	25% |
| Physical | 59% | 	47% |	24% | 	12% |

 
#### 2.5) Implementation Timeline  <a id="Implementation_Timeline"></a>
Given the complexity of the App, Apex Consultants proposes a phased pilot implementation approach (outlined in Figure 2). During the initial phase (up to 6 months), SuperLife should offer the App to a pilot group of policyholders, with only subset of all possible health initiatives. After testing and refining the App with the pilot group, SuperLife should gradually increase the number of policyholders who can download the app, before expanding the number of health initiatives available to policyholders. 
  
In the initial ‘Pilot group implementation’, SuperLife should offer the App to policyholders who are categorised as ‘moderate risk’ since this cohort disproportionately accounts for 40.9% of death claims, despite only accounting for 23.8% of in-force policies. Further investigation also reveals that this cohort has a slight skew towards younger age groups where digital adoption capabilities are typically stronger.  

#### 2.6) Monitoring Outcomes <a id="Implementation_Timeline"></a>
SuperLife should monitor outcomes, through the following metrics:  
  - In the __short-term (<2 years)__, SuperLife should assess metrics such as App adoption and utilisation rates. During the first few years of implementation, SuperLife should focus on ensuring the logistical and operational efficiency of the App by monitoring outcomes to validate assumptions and respond to any implementation challenges.  
  - In the __medium term (5-10 years)__, SuperLife should focus on measuring improvements in key health indicators, including a reduction in chronic disease prevalence (particularly respiratory illnesses and cancers). This timeframe is informed by longitudinal studies which have concluded that individuals should maintain a healthier lifestyle over a minimum of 6 years to experience a significant reduction in chronic diseases such as cardiovascular disease (Ding, 2021).  
  - Over the __long-term (>15 years)__, SuperLife should determine the reduction in mortality rates and the consistency of policyholder engagement with the App. This long-term horizon acknowledges that the adoption of healthier lifestyles, and the subsequent reduction in mortality rates, is a gradual process.

## 3) Pricing and Costs <a id="Pricing_and_Costs"></a>
#### 3.1) Methodology  <a id="Methodology"></a>
Mortality savings was calculated by projecting revised mortality rates for each individual policyholder using stochastic methods, and considering investment returns, net premium reserves and various expenses. Further details are provided in Appendix C.  
 	 
#### 3.1.1) Mortality Projection  
To model historical mortality rates, the in-force policyholder dataset was fitted with the semiparametric Cox Proportional Hazards model, which assumes a baseline mortality curve adjusted that is proportionally based on covariate values. The hazard rate is as follows:  

ℎ(𝑡) = ℎ0(𝑡) × exp ( 𝑥𝑖𝛽𝑖) 
Covariate selection was done by fitting models on a range of covariates and conducting statistical tests. Some covariates excluded due to insufficient data. The ‘sex’ and ‘smoking status’ of policyholders were the chosen covariates, with 𝛽 = 1.25 and 𝛽 = 8.72 respectively. Both the individual covariates are considered statistically significant; the fitted Cox model passes the likelihood ratio, Wald and log-rank tests (see Appendix C.1.1 for further details). 
 
#### 3.1.2) Modelling Reduction in Mortality 
The impact of each intervention category was modelled using a compound Poisson model. The severity component of the compound Poisson model captures the expected reduction in mortality, while the frequency component captures the expected level of uptake (i.e. expected number of proportion to participate after applying a 50% uplift from incentives pertaining to increased participation in multiple activities per intervention categories).  

A gamma distribution was used to stochastically model the reduction in mortality for each category of intervention. The gamma distribution was selected due to its usefulness in modelling proportions, as well as its right-tailed nature to capture the fact that interventions are likely to highly effective up to a certain extent, before tapering off. The gamma distribution for each category was truncated at the assumed upper and lower bounds of mortality reduction.   
 
#### 3.1.3) Modelling Uptake in Health-Promoting Activities 
The expected uptake of the activities was modelled using a Poisson distribution, where 𝜆 represents the expected uptake per year adjusted by the expected frequency of participation. Using results reported in the British Healthiest Workplace 2019 Survey, a study seeking to understand the participation in different health initiatives, formed the basis of uptake assumptions. Since uptake figures were reported at an aggregate level in the report, an adjustment per age cohort was applied to stratify the uptake by age cohorts. For instance, policyholders aged 25-34 are assumed to be more likely to participate in Physical Wellbeing initiatives, whereas policyholders >65 are assumed to be more likely to participate in Preventive initiatives. The level of uptake was further adjusted by the expected frequency per year such that the expected time between each activity, (assuming 𝑇~𝐸𝑥𝑝(𝜆)), would be reasonable, based on the rewards system and scope of activities. This level of uptake was taken to be the value of 𝜆 in the Poisson model. 
 
An example of a simulated mortality reduction is given in Figure 3 which depicts the simulated number of policyholders at a different levels of mortality reduction.   
 	 
#### 3.1.4) Individual Policyholder Projection 
Apex Consultants has used a stochastic approach to simulate the characteristics of each individual policyholder at the end of each period. The characteristics of each policyholder are defined using an 8-dimensional matrix consisting of the following information:  
•	Issue year (fixed over time for each policyholder) 
•	Age (assumed to be capped at 120 years old for all policyholders) 
•	Projection time (number of periods remaining on the term of the policy; assumed to be ≤ 20 for T20 policies)  
•	Sex (male or female – fixed over time for each policyholder) 
•	Smoking status (smoker or non-smoker – transition between ‘smoker’ and ‘non-smoker’ at each timepoint by applying a discrete Markov transition matrix)  
•	Policy type (T20 or SPWL – fixed over time for each policyholder)  
•	Face amount (Č50,000 up to Č2,000,000 – fixed over time for each policyholder) 
 
After initialising the loop with the current characteristics of each policyholder in the in-force dataset, the loop was projected out by simulating the characteristics of each policyholder at the end of each period. Crucially, the model projects the number of deaths at each discrete time point by varying mortality assumptions depending on age, gender, and smoking status. Additionally, to account for the impact of health-promoting activities offered via the App, the mortality rates are rated downwards by simulating a compound Poisson distribution assumption for the effectiveness and uptake of each activity (see Appendix A.4 and Section 3.1.3). In this way, the model acknowledges the differences in the responsiveness of each policyholder to the health-promoting activities and accounts for these differences in stochastic manner.  
 
In addition, new policyholders who join at future timepoints are appended onto the matrix. Participant mix was based on the average distribution of policyholders in the last 5 years by contract, with a linear growth rate applied (refer to Appendix. C1.4) 
 
At the end of each discrete timepoint, the model counts the number of deaths over the period. Since the model assumes that withdrawals can only occur at the end of the year, the simulation model randomly withdraws T20 policies at a fixed rate of 0.01 after lives are removed due to the death decrement (see Appendix C.2 for further details on withdrawal assumptions). The model then counts the number of withdrawn policies over the period. At the end of each period, policyholders are removed from the matrix if they withdraw, reach maturity, or die. We have not made any allowance for surrender values for SPWL contracts, and their release in reserve.  

#### 3.1.5) Expenses Projection 
In projecting expenses, the model considers: claims expenses, initial expenses, commission expenses, and renewal expenses. See Appendix C for further details on expenses.  
 
|Expense Category | Assumptions | 	Commentary | 
|-----------------|-------------|--------------|
| Initial Expenses|	• Č150 in acquisition costs per policy | Initial expenses are attracted due to customer acquisition costs.|   
|Commission|•	Commission expenses are a fixed proportion of premium income. <br> •	Online channels do not require commissions| 	Commissions differ by distribution channel with Agents being offered the largest commissions. Agents provide more personalised service and thus command a larger percentage of profit for sales.|  
|Renewal Commission| • Renewal expenses are a fixed proportion of premium income. |	SPWL policies do not attract a renewal commission as they cannot be withdrawn by policyholders. | 
|Expected Death Claims | • Methodology to project the number of deaths at each time point is described in __Section 3.1.2.___  	|Claims expenses are projected by multiplying the number of deaths per face amount by the face amount. |
 
#### 3.1.6) Premium Calculation 
The premium was calculated by calculating the gross premium reserve (excl. Commissions) and applying an additional commission + profit margin loading onto the final estimate (Refer to Appendix C2.3). This simplistic approach for premium calculation, over applying set profit measures, signature was applied to provide a coarse base projection of the companies’ total inflows. The rates were calculated based on 4% and 4.75% discount rate for the T20 and SPWL policies respectively, (see Section 3.1.4) . This ensures premium incomes are sufficient to cover future liabilities and expenses, creating a specified level of profit whilst being underpinned by SuperLife’s portfolio mix when calculating the intervention. 
 
#### 3.1.7) Mortality Savings and Future Economic Value 
Mortality savings was calculated by determining the expected value of savings arising from a reduction in death claims in respect to costs after inflation due to implementing the App. These expected savings are noted as a nominal rate, reflective of the year-on-year mortality cost reduction. To determine projected future economic value, a baseline model has been developed applying steady state assumptions over a 20 year period and provides a comparable point estimate of portfolio profit. However, in the future projection has treated the initiative stochastically for frequency of uptake, mortality reduction, costs and inflation. Consequently, this provide a reasonable view on proportional baseline impact of the initiative and prevent masking by confounding effects based on the model specification in the baseline. 

## 3.2) Results <a id="Results"></a>

#### 3.2.1) Reduction in Number of Deaths 
Figure 4 compares the  	baseline number of deaths (using baseline mortality assumptions)  	with the revised number of deaths (assuming that the App had been implemented over the last 20 years 	). There is a widening gap between historical and adjusted number of deaths over time, as the  initiative becomes increasingly effective given that the  impacts of mortality reduction are cumulative 	 and more policies in force. On average, there is a reduction in 156 deaths per year, amounting to a decrease of approximately 3000 deaths over the 20-year period.  

#### 3.2.2) Historical Mortality Savings and Projected Economic Value 
Figure 5 pertains to the historical mortality savings under the steady-state smoothed mortality assumption. There is a clear reduction in the claim’s costs, with an average saving of $65m per annum, and nominally 1.3b impact over the last 20 years. The initiative is also projected to be effective over the next 20 years, as represented in Figure 6 which depicts the mean projected profit (using adjusted mortality assumptions) being approximately 5-8% higher than the baseline projected profit. Figure 6 illustrates that lower percentiles of projected profitability exhibit comparatively reduced volatility. Specifically, the gap in profit between the 5th and 25th percentiles is narrower than that between the 75th and 95th percentiles. Further, this approach includes variable app cost assumptions (refer Appendix A.8), assuming the additional profits from the intervention are shared as 50% to incentivise policyholders (Č30M - Č 50M as game rewards). 
 
#### 3.3) Potential Pricing Changes 
It is difficult to assess how pricing changes will affect the competitiveness of SuperLife’s product in Lumaria given the lack of data regarding competitors. SuperLife should maintain the current pricing structure and consider offering discounts in the longer term if mortality rates reduce sufficiently. This analysis assumed a steady state of intervention utilisation, however, we expect this to vary over duration of implementation, as SuperLife develops a network economy. Specifically, as noted in section 2.1.3, we note that the initiative will deliver high initial costs, to develop the app infrastructure and market accordingly. We recommend SuperLife to continue with a more conservative pricing view in the medium to short term, using equity and reserves for research and development. However, we recommend SuperLife to monitor mortality trends, uptake levels and broader industry trends to assume a competitive position. Further, internally we recommend SuperLife to continue monitoring to appropriate offer discounts where relevant, with the initiative a flexible alternative to pure reversionary bonuses.                                                                                      
## 4) Assumptions <a id="Assumptions"></a>
#### 4.1) Economic Assumptions <a id="Economic_Assumptions"></a>
The investment and interest rates were forecasted using historical rates from 1982 onwards. The sharp decline in interest rates, and subsequent stability in inflation after 1982, suggests that Lumaria adopted inflation targeting from 1982 onwards (see Appendix C.1.2). Both rates were forecasted using a random walk approach over 10,000 simulations. The random walk assumes normally distributed increments based on historical data mean and standard deviation, and is bounded by the observed historical values. For each future period, the 10th percentile, average, and 90th percentile values were used.  
 
#### 4.2) Mortality Assumptions <a id="Mortality_Assumptions"></a>
The Lumaria life tables were assumed to be an accurate source for projecting future mortality. The in-force policy dataset was not taken as a source for future mortality projections, as this would involve the influence of year-related trends present in the captured period. Since the life tables did not differentiate between males and females, or between non-smokers and smokers, a life table incorporating these distinctions was manually created. This life table assumes that the sex ratio at birth is 1:1. It also assumes a smoking rate of 18%, and that only people over 18 are smokers. To create this table, an assumption was made that hazard rates amongst the different groups were proportional, using female and non-smokers as the baseline, taken from the in-force data. 

|Group 	|	Female | 		Male |	Non-Smoker | Smoker | 
|--------|----------|-----------|---------------|------|
|Ratio 	|1.00 | 		1.27 | 		1 	| 8.72 |

#### 4.3) Intervention Assumptions <a id="Intervention Assumptions"></a>
The intervention uptake rates were assumed to reflect similar programs in real life based on the British Healthiest Workplace 2019 Survey. However, cultural, and environmental differences are unknown and could affect the uptake rates drastically. Thus, a more conservative estimation was used where, the incentive of points was not considered to increment the uptake rate despite, evidence that incentives significantly increase uptake rates.  

## 5) Risk and Risk Mitigation Considerations  <a id="Risk_and_Risk_Mitigation_Considerations"></a>
#### 5.1) Risk Assessment <a id="Risk Assessment"></a>
The most significant risks are displayed in the risk matrix in Figure 7. A description of each risk is as follows:  
1. Data security breach:  given that the app is collecting highly sensitive information, a data breach could result in financial losses  through fines and lawsuits, as well as reputational damage.
2. System failure: technical glitches or a confusing user interface could frustrate policyholders and even deter them from engaging.
3. Model risk: Incorrect assumptions, parameters, or data inputs may overestimate the reduction in mortality, resulting in  
unsustainable reward structures.
4. Adverse changes in economic assumptions: consistently high inflation rates may inflate the cost of the program beyond initial expectations, while persistently low investment rates may dampen the return on mortality savings.
5. Trend risk: unforeseen changes in future health trends, such as new diseases, may affect SuperLife’s ability to effectively create health initiatives that target or prevent these diseases.
6. Regulatory change: unforeseen shifts in regulatory requirements, such as prohibiting the collection of personal health-related data, could disrupt the operations of the App.  

#### 5.2) Mitigation Strategies <a id="Mitigation_Strategies"></a>

|# |	Risk | 	Type |	Mitigation| 
|--|---------|-------|------------|
| 1 | 	Data security breach  |	Both |	Investing in data security measures such as encryption protocols, access controls and regular audits should reduce vulnerabilities in the data security. SuperLife should gradually upgrade as the data security system as the number of users increases over time. | 
| 2 |	System failure |  	Qualitative | 	SuperLife should seek feedback from users and make iterative improvements to the interface based on user needs and preferences. As the number of users increases, SuperLife should increase its investment into system maintenance to minimise system failures.| 
|3 |	Model risk |  	Quantitative | SuperLife should monitor results and recalibrate model parameters in response to updated data.  |
| 4 | 	Adverse changes in economic assumptions |  	Quantitative | 	This report includes sensitivity analysis (see 5.3) to assess the impact of changes in economic assumptions. SuperLife should also ensure that its investment portfolio is sufficiently diversified.  |
| 5 | Trend risk | 	Qualitative |	The App has been intentionally designed with scalability and agility so that SuperLife can easily add or remove specify health initiatives as needed. SuperLife should monitor health trends and emerging diseases to inform its decisions regarding whether and when to modify specific health initiatives. |
| 6	| Regulatory change |  	Qualitative | If regulatory changes transpired, SuperLife should collect anonymised data on an aggregated level.  |

#### 5.3) Sensitivity Analysis (Refer to Appendix D) <a id="Sensitivity_Analysis"></a>

|Assumption | 	Range |	Commentary |
|-----------|---------|------------|
|3.5% inflation | ≤6% | Inflation increases program costs over time. Inflation costs have not exceeded 8.6% since 1982 | 
|Č248/ year for a 35-yearold (baseline mortality) |	≥ Č181 | 	Premium costs should be sufficiently high to account for program expenses and death claim costs |
| Initial expense of Č150 | ≤ Č1720 | 	Initial expenses detract from profitability of policies | 

Using assumptions detailed in Section 3 and 4, we believe that the program will provide a profitable impact to SuperLife at a 70% in the immediate term to 80% confidence level in the long term. This has been determined based on the degree of uncertainty bands surrounding the simulated future mortality, historical mortality projections and program costs future variability. 

## 6) Data and Limitations <a id="Data_and_Limitations"></a>

|Data Requirement |Data Sources | 	Data Limitations | 
|-----------------|-------------|--------------------|
| Mortality rate | 	Lumaria life table (Lumaria govt) | 	Life table provides mortality by age only and does not provide mortality rates by sex or smoking status. |
|Mortality rate |Policyholder mortality rates (in-force policyholder dataset) | 	Mortality rates implied at the ends of the age spectrum are weak estimates due to low sample size or low occurrences of mortality. |  
| Economic assumptions | 	Central bank of Lumaria | 	It is unclear whether inflation targeting practices were adopted. | 
| Smoker – nonsmoker transition rates |	National Library of Medicine (USA), WHO | Cultural differences between the participants surveyed and the fictional people of Lumaria may alter the rates.| 
|Participation Rates of Intervention | British Healthiest |Workplace 2019 Survey 	The survey was conducted on a voluntarily basis; selection bias may be introduced. However, a large sample size is used to reduce the bias.| 

#### 6.2) Covid-19 Considerations  
There was a 50% increase in mortality during the Covid periods in 2020-2023 (see Appendix D.1). In projecting the mortality rates, these anomalous rates were removed from the historical data to allow us to model the historical baseline, and an explicit uplift was made during 20202023 periods. However, future economic projections assumed steady state with historical preCovid experience was applied. Despite this, our view is that Covid uplift is a black-swan event independent to the intervention, designed to reduce general mortality and no confounding effect was applied. SuperLife should vigilantly monitor the impacts of Covid-19 on mortality for their policyholders and the broader Lumaria population and adjust life tables and mortality assumptions accordingly. Future adjustments should not impact the methodology described above, only the parameters and / or assumptions.  
 
## 7) Final Recommendations <a id="Final_Recommendations"></a> 
The proposed gamified program adopts a holistic approach to health and wellbeing and offers SuperLife’s policyholders the ability to choose what health-promoting activities to engage with. In addition to being informed by medical research, the quantitative modelling also reaffirms the projected success of the program from a mortality reduction and savings perspective. As the program is designed to be gradually scaled via a phased pilot implementation approach, SuperLife should monitor the outcomes and expand the program if it proves to be successful against the metrics outlined in Section 1.3.    

## 8) Bibliography <a id="Bibliography"></a>
 
Ambrosino N, Bertella E. Lifestyle interventions in prevention and comprehensive management of COPD. Breathe (Sheff). 2018 Sep;14(3):186-194. doi: 
10.1183/20734735.018618. PMID: 30186516; PMCID: PMC6118879. 
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6118879/ 
 
AppsChopper. “How Much Does Healthcare App Development Cost in 2024?” AppsChopper Blog, 21 Feb. 2024, www.appschopper.com/blog/healthcare-app-development-cost/. Accessed 21 Mar. 2024.  
 
Arancini, L. (2021) Age as a predictor of quit attempts and quit success in ..., WILEY Online Library. Available at: https://onlinelibrary.wiley.com/doi/full/10.1111/add.15454 (Accessed: 2 March 2024). 
 
Associations Between Healthy Lifestyle Trajectories and the Incidence of Cardiovascular Disease With All-Cause Mortality: A Large, Prospective, Chinese Cohort Study (2021) 
 
Darden M, Gilleskie DB, Strumpf K. Smoking and Mortality: New Evidence from a Long Panel. Int Econ Rev (Philadelphia). 2018 Aug;59(3):1571-1619. doi: 10.1111/iere.12314. Epub 2018 Mar 9. PMID: 31274880; PMCID: PMC6608712. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6608712/  
 
Ding X, Fang W, Yuan X, Seery S, Wu Y, Chen S, Zhou H, Wang G, Li Y, Yuan X, Wu S. 
Associations Between Healthy Lifestyle Trajectories and the Incidence of Cardiovascular 
Disease With All-Cause Mortality: A Large, Prospective, Chinese Cohort Study. Front Cardiovasc Med. 2021 Dec 20;8:790497. doi: 10.3389/fcvm.2021.790497. PMID: 34988131; PMCID: PMC8720765. 
  
Espinosa-Salas S, Gonzalez-Arias M. Behavior Modification for Lifestyle Improvement. 
[Updated 2023 Apr 23]. In: StatPearls [Internet]. Treasure Island (FL): StatPearls Publishing; 2024 Jan-. Available from: https://www.ncbi.nlm.nih.gov/books/NBK592418/ 
 
Fahey MC, Dahne J, Wahlquist AE, Carpenter MJ. The Impact of Older Age on Smoking 
Cessation Outcomes After Standard Advice to Quit. J Appl Gerontol. 2023 Jul;42(7):14771485. doi: 10.1177/07334648231158228. Epub 2023 Feb 16. PMID: 36797652; PMCID: PMC10257741.  
 
Farooqui, M.A., Tan, YT., Bilger, M. et al. Effects of financial incentives on motivating physical activity among older adults: results from a discrete choice experiment. BMC Public Health 14, 141 (2014). https://doi.org/10.1186/1471-2458-14-141 
 
Georgiou, Michael. “Cost of Mobile App Maintenance in 2024 and Why It’s Needed.” 
Imaginovation, 8 Nov. 2023, imaginovation.net/blog/importance-mobile-app-maintenancecost/.   
GooApps®. “How Much Does It Cost to Maintain an App in 2024.” GooApps English, 31 
Jan. 2024, gooapps.net/2024/01/31/how-much-does-it-cost-to-maintain-an-app-in-
2024/#:~:text=Considering%20all%20the%20above%2C%20maintaining. Accessed 21 Mar. 2024. 
Guthold, R., Stevens, G. A., Riley, L. M. & Bull, F. C. Worldwide trends in insufficient physical activity from 2001 to 2016: a pooled analysis of 358 population-based surveys with 1.9 million participants. Lancet Glob. Health 6, e1077–e1086 (2018). 
 
Hajat, C., Hasan, A., Subel, S. et al. The impact of short-term incentives on physical activity in a UK behavioural incentives programme. npj Digit. Med. 2, 91 (2019). 
https://doi.org/10.1038/s41746-019-0164-3 https://journals.lww.com/joem/Abstract/2021/09000/Descriptive_Study_of_Employee_Engag ement_With.1.aspx 
 
Hay, S. and Gakidou, E. (2021) Spatial, temporal, and demographic patterns in prevalence of smoking ..., THE LANCET Public Health. Available at: 
https://www.thelancet.com/article/S2468-2667(21)00102-X/fulltext (Accessed: 2 March 2024).  
 
“Healthcare App Development Cost: How to Get the Right Estimate.” Www.linkedin.com, www.linkedin.com/pulse/healthcare-app-development-cost-how-get-right-estimate/. Accessed 21 Mar. 2024.  
 
Individual life - Expense experience studies | SOA. (n.d.). 
SOA. https://www.soa.org/research/topics/indiv-expense-exp-study-list/ 
  
Lariscy, J.T., Hummer, R.A. and Rogers, R.G. (2018) ‘Cigarette smoking and all-cause and cause-specific adult mortality in the United States’, Demography, 55(5), pp. 1855–1885. 
doi:10.1007/s13524-018-0707-2. 
 
MBA, S.J. (2021) Descriptive study of employee engagement with workplace... : Journal of Occupational and Environmental Medicine, JOEM. Available at: https://journals.lww.com/joem/abstract/2021/09000/descriptive_study_of_employee_engagem ent_with.1.aspx  (Accessed: 2 March 2024). 
 
Preventing cancer (no date) World Health Organization. Available at: 
https://www.who.int/activities/preventing-cancer (Accessed: 2 March 2024). 
 
Statistics Canada. Table 13-10-0156-01  Deaths, by cause, Chapter XX: External causes of morbidity and mortality (V01 to Y89) DOI: https://doi.org/10.25318/1310015601-eng 
 
Stress-induced respiratory distress – a preventative approach... (2021) Asthma New Zealand. Available at: https://www.asthma.org.nz/blogs/little-learnings/stress-induced-respiratorydistress-a-preventative-approach (Accessed: 2 March 2024).  
 
Wu, Y.-T. et al. (2021) ‘Sex differences in mortality: Results from a population-based study of 12 longitudinal cohorts’, Canadian Medical Association Journal, 193(11). doi:10.1503/cmaj.200484. 
