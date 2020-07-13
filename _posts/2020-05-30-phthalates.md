---
title: Phthalate Exposure in U.S. Women of Reproductive Age - an NHANES Review, Part I 
author: "alice"
date: '2020-07-11'
excerpt: ""
layout: single
htmlwidgets: true
toc: true
tags:
  - Environment
  - NHANES
---

# Motivation

Perhaps I’m the type of person that worries excessively about things outside of their control, but I’ve long been concerned about plastics leaching into our bodies from things like food packaging and personal care products. 

Some of these anxieties might be justified. A recent analysis of bio-specimen data from the National Health and Nutrition Examination Survey [NHANES](https://www.cdc.gov/nchs/nhanes/index.htm), the largest annual survey of the United States population, found nearly ubiquitous exposure to phthalates (Zota et al, 2014). This is particularly concerning for pregnant women because studies have found adverse effects, including shorter pregnancy duration and decreased head circumference (Polanska et al, 2016), as well as endocrine disrupting effects in males, including decreased anogenital distance (animal study) (Swan et al, 2005) and damage to sperm DNA (Duty et al, 2003). So how concerned should we be? 

After reviewing the literature, I wanted to answer the following questions: 

1. What phthalates are present in women in the US, what are the average levels of these phthalates, and how have these levels changed over time? 
2. Are phthalate levels disproportionately distributed through the population? Namely, is there an association with phthalates and socioeconomic status? 

We'll cover question 1 here and question 2 in the next post. 

## Background

Before we dive in to the data, we need a bit of priming on what phthalates are and why they're important. Phthalates are the most common category of industrial plasticizers (chemicals that alter the flexibility and rigidity of plastics) in use today. Due to their widespread presence in manufacturing, the metabolites of these chemicals (i.e the breakdown products when they enter the body) are now ubiquitously detectable in humans in the United States (Zota et al, 2014). Phthalates are of particular concern in women of reproductive age because they can easily cross the placenta and interact with fetal development during critical windows of pregnancy (Polanska et al, 2016). Previous studies have established detrimental defects on reproductive system development in fetuses exposed to phthalates (Swan et al, 2005). 

Applications of phthalates range from building materials, including flooring and adhesives, to personal care products, including nail polish and shampoo [CDC](https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PHTHTE_I.htm). High molecular weight phthalates like butylbenzyl phthalate (BBzP), di(2-ethylhexyl) phthalate (DEHP) and diisononyl phthalate (DiNP) are commonly found in building materials. Low molecular weight materials like diethyl phthalate (DEP), di-n-butyl phthalate (DnBP) are more commonly found in personal care products and medications (Zota et al, 2014).

Although several studies have been conducted using cohorts, a powerful tool at our disposal is NHANES. This national survey has been performed annually by the CDC since [1960](https://www.cdc.gov/nchs/nhanes/about_nhanes.htm), provides a large sample size (about 10,000 participants per year), and assesses a very wide range of health factors including demographics, diet, chronic health conditions, and biomarkers in blood and urine. Because of its breadth and large sample size, NHANES provides rich datasets for studying associations between these health attributes. 

# Analysis

Although there is a [library](https://cran.r-project.org/web/packages/RNHANES/index.html) for analyzing NHANES in R (aptly named `RNHANES`), if you work with it long enough, you will notice that only some cycles and variables are available. I spent more time than I care to admit wrangling with this library, and ultimately concluded it would not give me what I needed. Instead, I downloaded the SAS export files from CDC's NHANES [website](https://www.cdc.gov/nchs/nhanes/index.htm) and imported them to R using the `foreign` library. Below are the setup, import, and tidying steps I used. 

## Data Preparation



### Libraries


```r
library(tidyverse)
library(foreign)
library(stats)
library(viridis)
library(survey)
library(plotly)
library(kableExtra)
library(gridExtra)
library(sjPlot)
```

### Data Import

First, we read in NHANES cycles 2003 - 2004, 2005 - 2006, 2007 - 2008, 2009 - 2010, 2011 - 2012, 2013 - 2014, and 2015-2016. As of June 2020, phthalate data for 2017 - 2018 was not yet available. The NHANES [datasets](https://wwwn.cdc.gov/nchs/nhanes/default.aspx) used are Demographics (DEMO) and Phthalates and Plasticizers Metabolites - Urine (PHTHTE). For the 2015-2016 cycle we also need the Albumin & Creatinine (ALB_CR_I) file since creatinine data were removed from the phthalates files during this cycle (more on creatinine below).


```r
# reading in 2003 - 2004 data
demo_03_04 = read.xport("../data/2003_2004_DEMO_C.XPT")
phthalate_03_04 = read.xport("../data/2003_2004_PHTHTE_C.XPT.txt")

# reading in 2005 - 2006 data
demo_05_06 = read.xport("../data/2005_2006_DEMO_D.XPT")
phthalate_05_06 = read.xport("../data/2005_2006_PHTHTE_D.XPT.txt")

# reading in 2007 - 2008 data
demo_07_08 = read.xport("../data/2007_2008_DEMO_E.XPT")
phthalate_07_08 = read.xport("../data/2007_2008_PHTHTE_E.XPT.txt")

# reading in 2009 - 2010 data
demo_09_10 = read.xport("../data/2009_2010_DEMO_F.XPT")
phthalate_09_10 = read.xport("../data/2009_2010_PHTHTE_F.XPT.txt")

# reading in 2011 - 2012 data
demo_11_12 = read.xport("../data/2011_2012_DEMO_G.XPT.txt")
phthalate_11_12 = read.xport("../data/2011_2012_PHTHTE_G.XPT.txt")

# reading in 2013 - 2014 data
demo_13_14 = read.xport("../data/2013_2014_DEMO_H.XPT.txt")
phthalate_13_14 = read.xport("../data/2013_2014_PHTHTE_H.XPT.txt")

# reading in 2015 - 2016 data (note change in creatinine source file for this cycle)
demo_15_16 = read.xport("../data/2015_2016_DEMO_I.XPT.txt")
phthalate_15_16 = read.xport("../data/2015_2016_PHTHTE_I.XPT.txt")
creat_15_16 = read.xport("../data/2015_2016_ALB_CR_I.XPT.txt")
```


### Data Tidy

Next we'll bind the data files for each cycle using left-joins, merging on the unique identifier `SEQN`.


```r
data_03_04 = 
  left_join(demo_03_04, phthalate_03_04, by = "SEQN") %>% 
  mutate(cycle = "2003-2004")

data_05_06 = 
  left_join(demo_05_06, phthalate_05_06, by = "SEQN") %>% 
  mutate(cycle = "2005-2006")

data_07_08 = 
  left_join(demo_07_08, phthalate_07_08, by = "SEQN") %>% 
  mutate(cycle = "2007-2008")

data_09_10 = 
  left_join(demo_09_10, phthalate_09_10, by = "SEQN") %>% 
  mutate(cycle = "2009-2010")

data_11_12 = 
  left_join(demo_11_12, phthalate_11_12, by = "SEQN") %>% 
  mutate(cycle = "2011-2012")

data_13_14 = 
  left_join(demo_13_14, phthalate_13_14, by = "SEQN") %>% 
  mutate(cycle = "2013-2014")

data_15_16 = 
  left_join(demo_15_16, phthalate_15_16, by = "SEQN") %>% 
  left_join(creat_15_16, by = "SEQN") %>% 
  mutate(cycle = "2015-2016") 

all_data = 
  bind_rows(data_03_04, data_05_06, data_07_08, data_09_10, data_11_12, data_13_14, data_15_16) 
```


#### Variables Used

Next, we'll select the variables we want. We will choose 12 phthalates measured in NHANES between 2003 and 2016, as well as urinary [creatinine](https://en.wikipedia.org/wiki/Creatinine) `URXUCR`. The latter is a constant byproduct of metabolic activities and is often used to measure urinary dilution. 

NHANES takes biosample data for about 1/3 of the survey participants, so we will remove observations with missing phthalate data. We will restrict analysis to female respondents between the ages of 20 and 44, which we'll take to mean reproductive age in this analysis. We will also include age and socioeconomic measures to be used in Question 2. 

Below are the NHANES variables used, along with abbreviations for phthalate names. More intuitive variable names are assigned the subsequent code chunk. 

* `SEQN`: Unique NHANES identifier
* `RIAGENDR`: Gender
* `RIDAGEYR`: Age
* `RIDRETH1`: Ethnicity
* `DMDEDUC2`: Education
* `DMDMARTL`: Marital Status
* `INDHHIN2`: Annual household income (cycles 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016)
* `INDFMIN2`: Annual family income (cycles 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016)
* `INDHHINC`: Annual household income (cycles 2003-2004, 2005-2006)
* `INDFMINC`: Annual family income (cycles 2003-2004, 2005-2006)
* `WTMEC2YR`, `SDMVPSU`,`SDMVSTRA`: Survey weighting variables, addressed in Question 2 below
* `URXUCR`: Urinary creatinine
* `URXCNP`: Mono(carboxyisononyl) phthalate (MCNP) (ng/mL)
* `URXCOP`: Mono(carboxyisoctyl) phthalate (MCOP) (ng/mL)
* `URXMNP`: Mono-isononyl phthalate (MiNP) (ng/mL)
* `URXECP`: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP) (ng/mL)
* `URXMHH`: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP) (ng/mL)
* `URXMHP`: Mono-(2-ethyl)-hexyl phthalate (MEHP) (ng/mL)
* `URXMOH`: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP) (ng/mL)
* `URXMBP`: Mono-n-butyl phthalate (MnBP) (ng/mL)
* `URXMEP`: Mono-ethyl phthalate (MEP) (ng/mL)
* `URXMIB`: Mono-isobutyl phthalate (MiBP) (ng/mL)
* `URXMC1`: Mono-(3-carboxypropyl) phthalate (MCPP) (ng/mL)
* `URXMZP`: Mono-benzyl phthalate (MBzP) (ng/mL)



```r
# select variables of interest, drop every observation without biomarker data, filter out women aged 20-44, and consolidate household and family income variables
all_data = 
  all_data %>% 
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, DMDMARTL, INDHHINC, INDFMINC, INDHHIN2, INDFMIN2, WTMEC2YR, SDMVPSU, SDMVSTRA, URXUCR, URXECP, URXMBP, URXMC1, URXMEP, URXMHH, URXMHP, URXMIB, URXMOH, URXMZP, URXMNP, URXCOP, URXMNP, URXCNP) %>%
  drop_na(URXMEP) %>% 
  rename(gender = RIAGENDR, age = RIDAGEYR, ethnicity = RIDRETH1, education = DMDEDUC2, marital_status = DMDMARTL, creatinine = URXUCR, mecpp = URXECP, mnbp = URXMBP, mcpp = URXMC1, mep = URXMEP, mehhp = URXMHH, mehp = URXMHP, mibp = URXMIB, meohp = URXMOH, mbzp = URXMZP, mcnp = URXCNP, mcop = URXCOP, minp = URXMNP) %>% 
  filter(gender == 2, age %in% (20:44)) %>% 
  mutate(
    household_income = if_else(cycle %in% c("2003-2004", "2005-2006"), INDHHINC, INDHHIN2), 
    family_income = if_else(cycle %in% c("2003-2004", "2005-2006"), INDFMINC, INDFMIN2)
    ) %>% 
  select(-c(INDHHINC, INDHHIN2, INDFMINC, INDFMIN2))
```

Finally, we will add variables for creatinine-adjusted phthalate concentrations. There is, however, a units mismatch we'll need to deal with. Creatinine is measured in mg/dL and all phthalate biomarkers are measured in ng/mL. Creatinine adjusted measures are reported here (and often in literature) in units of \\(\mu\\)g phthalate/g creatinine. To get to these final units, we multiply the phthalate concentration by 100 and divide by creatinine [^1]. Adjusted values are denoted with a `_c`. 
 

```r
all_data_c = 
  all_data %>% 
  mutate(mecpp_c = 100*mecpp/creatinine, 
         mnbp_c = 100*mnbp/creatinine,
         mcpp_c = 100*mcpp/creatinine,
         mep_c = 100*mep/creatinine, 
         mehhp_c = 100*mehhp/creatinine,
         mehp_c = 100*mehp/creatinine, 
         mibp_c = 100*mibp/creatinine, 
         meohp_c = 100*meohp/creatinine, 
         mbzp_c = 100*mbzp/creatinine, 
         mcnp_c = 100*mcnp/creatinine,
         mcop_c = 100*mcop/creatinine, 
         minp_c = 100*minp/creatinine)
```

## Question 1: What phthalates are present in women in the US, what are the average levels of these chemicals, and how have these levels changed over time?

After I set about answering the first question, I realized that the most interesting part is the temporal aspect. If there were spikes in certain phthalates, for instance, that would narrow my focus going forward. Conversely, if phthalates were steadily decreasing in the population, maybe this would assuage my anxieties and I could find something else to worry about. Either way, I decided to tackle this question first. I used a spaghetti plot to visualize phthalates over the seven cycles of data and added `ggplotly` interactivity to help distinguish one line from the other. 


```r
means_c = 
  all_data_c %>% 
  select(-c(gender:mbzp), -c(SEQN, mcnp, mcop, mcnp_c, mcop_c)) %>% 
  pivot_longer(c(mecpp_c:minp_c), names_to = "chemical_c", values_to = "value") %>% 
  drop_na() %>% 
  mutate(chemical_c = recode(chemical_c, 
                           "mecpp_c"= "Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)", 
                           "mnbp_c"= "Mono-n-butyl phthalate (MnBP_c)",
                           "mcpp_c"= "Mono-(3-carboxypropyl) phthalate (MCPP_c)",
                           "mep_c"= "Mono-ethyl phthalate (MEP_c)",
                           "mehhp_c"= "Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)",
                           "mehp_c"= "Mono-(2-ethyl)-hexyl phthalate (MEHP_c)",
                           "mibp_c"= "Mono-isobutyl phthalate (MiBP_c)",
                           "meohp_c"= "Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)", 
                           "mbzp_c"= "Mono-benzyl phthalate (MBzP_c)", 
                           "mcnp_c"= "Mono(carboxyisononyl) phthalate (MCNP_c)", 
                           "mcop_c"= "Mono(carboxyisoctyl) phthalate (MCOP_c)",
                           "minp_c"= "Mono-isononyl phthalate (MiNP_c)")) %>% 
  group_by(cycle, chemical_c) %>% 
  summarize(mean_c = mean(value), n = n(), sd = sd(value), median_c = median(value), iqr_c = IQR(value))

sp_plot = 
  means_c %>% 
  ggplot(aes(x = cycle, y = mean_c)) + 
  geom_line(aes(group = chemical_c, color = chemical_c))+
  labs(
    title = "Figure 1: Mean creatinine-adjusted phthalate concentration \n by NHANES cycle in women aged 20 - 44 (n = 2,754)",
    x = "NHANES Cycle",
    y = "Phthalate oncentration (ug/g creatinine)"
  ) +
  theme(text = element_text(size = 9)) + scale_colour_viridis_d(option = "inferno")

ggplotly(sp_plot)
```

<div class="figure">
<!--html_preserve--><div id="htmlwidget-222358584d1f3f85ad74" style="width:600px;height:600px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-222358584d1f3f85ad74">{"x":{"data":[{"x":[1,2,3,4,5,6,7],"y":[56.1317725095253,71.1213088191594,47.8599945680783,20.5723899794901,18.1954213545438,9.25407667870049,11.2410649769371],"text":["chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2003-2004<br />mean_c:  56.131773","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2005-2006<br />mean_c:  71.121309","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2007-2008<br />mean_c:  47.859995","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2009-2010<br />mean_c:  20.572390","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2011-2012<br />mean_c:  18.195421","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2013-2014<br />mean_c:   9.254077","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2015-2016<br />mean_c:  11.241065"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,4,1)","dash":"solid"},"hoveron":"points","name":"Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)","legendgroup":"Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[37.9598466599974,47.9622325643997,26.1414726018942,12.5603936489677,11.8178805357524,5.87326787717119,6.95626987119515],"text":["chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2003-2004<br />mean_c:  37.959847","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2005-2006<br />mean_c:  47.962233","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2007-2008<br />mean_c:  26.141473","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2009-2010<br />mean_c:  12.560394","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2011-2012<br />mean_c:  11.817881","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2013-2014<br />mean_c:   5.873268","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2015-2016<br />mean_c:   6.956270"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(27,12,66,1)","dash":"solid"},"hoveron":"points","name":"Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)","legendgroup":"Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[9.99278497880825,13.1671183141721,8.73815800086235,3.69438550065612,4.47177900622704,2.45005691556301,2.74735876731144],"text":["chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2003-2004<br />mean_c:   9.992785","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2005-2006<br />mean_c:  13.167118","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2007-2008<br />mean_c:   8.738158","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2009-2010<br />mean_c:   3.694386","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2011-2012<br />mean_c:   4.471779","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2013-2014<br />mean_c:   2.450057","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2015-2016<br />mean_c:   2.747359"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(75,12,107,1)","dash":"solid"},"hoveron":"points","name":"Mono-(2-ethyl)-hexyl phthalate (MEHP_c)","legendgroup":"Mono-(2-ethyl)-hexyl phthalate (MEHP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[3.24823865307215,3.26268386364904,4.49724272815392,5.37775055234306,10.2908950162625,5.22063454376074,2.16577377054881],"text":["chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2003-2004<br />mean_c:   3.248239","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2005-2006<br />mean_c:   3.262684","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2007-2008<br />mean_c:   4.497243","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2009-2010<br />mean_c:   5.377751","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2011-2012<br />mean_c:  10.290895","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2013-2014<br />mean_c:   5.220635","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2015-2016<br />mean_c:   2.165774"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(120,28,109,1)","dash":"solid"},"hoveron":"points","name":"Mono-(3-carboxypropyl) phthalate (MCPP_c)","legendgroup":"Mono-(3-carboxypropyl) phthalate (MCPP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[79.2952044186525,93.2461710029894,63.8871076234474,30.5874194281034,27.206444419551,14.5669955903277,19.3295550156809],"text":["chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2003-2004<br />mean_c:  79.295204","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2005-2006<br />mean_c:  93.246171","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2007-2008<br />mean_c:  63.887108","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2009-2010<br />mean_c:  30.587419","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2011-2012<br />mean_c:  27.206444","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2013-2014<br />mean_c:  14.566996","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2015-2016<br />mean_c:  19.329555"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(165,44,96,1)","dash":"solid"},"hoveron":"points","name":"Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)","legendgroup":"Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[16.7459134329851,14.1392304270737,14.8716322228646,11.2232160904095,8.69832685691784,9.90055500966045,8.62197756618394],"text":["chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2003-2004<br />mean_c:  16.745913","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2005-2006<br />mean_c:  14.139230","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2007-2008<br />mean_c:  14.871632","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2009-2010<br />mean_c:  11.223216","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2011-2012<br />mean_c:   8.698327","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2013-2014<br />mean_c:   9.900555","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2015-2016<br />mean_c:   8.621978"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(207,68,70,1)","dash":"solid"},"hoveron":"points","name":"Mono-benzyl phthalate (MBzP_c)","legendgroup":"Mono-benzyl phthalate (MBzP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[481.359316089068,634.142881654234,272.356859134769,253.397947751734,194.520374750077,144.475701652891,131.846349595601],"text":["chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2003-2004<br />mean_c: 481.359316","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2005-2006<br />mean_c: 634.142882","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2007-2008<br />mean_c: 272.356859","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2009-2010<br />mean_c: 253.397948","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2011-2012<br />mean_c: 194.520375","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2013-2014<br />mean_c: 144.475702","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2015-2016<br />mean_c: 131.846350"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(237,105,37,1)","dash":"solid"},"hoveron":"points","name":"Mono-ethyl phthalate (MEP_c)","legendgroup":"Mono-ethyl phthalate (MEP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[6.397093519366,9.85433048680007,12.5905056859069,12.7110541594219,10.8162132252463,11.7444695550205,13.2512655659892],"text":["chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2003-2004<br />mean_c:   6.397094","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2005-2006<br />mean_c:   9.854330","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2007-2008<br />mean_c:  12.590506","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2009-2010<br />mean_c:  12.711054","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2011-2012<br />mean_c:  10.816213","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2013-2014<br />mean_c:  11.744470","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2015-2016<br />mean_c:  13.251266"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(251,154,6,1)","dash":"solid"},"hoveron":"points","name":"Mono-isobutyl phthalate (MiBP_c)","legendgroup":"Mono-isobutyl phthalate (MiBP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[1.57438881151627,2.68132352322907,1.87878476996346,3.39540469331367,4.90755524915638,3.3425619064989,2.20937592907465],"text":["chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2003-2004<br />mean_c:   1.574389","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2005-2006<br />mean_c:   2.681324","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2007-2008<br />mean_c:   1.878785","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2009-2010<br />mean_c:   3.395405","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2011-2012<br />mean_c:   4.907555","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2013-2014<br />mean_c:   3.342562","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2015-2016<br />mean_c:   2.209376"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(247,208,60,1)","dash":"solid"},"hoveron":"points","name":"Mono-isononyl phthalate (MiNP_c)","legendgroup":"Mono-isononyl phthalate (MiNP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[37.8824076946198,30.4842116231416,31.9969441199587,63.9225654739426,17.9024470402083,13.5856706047327,14.4248270536611],"text":["chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2003-2004<br />mean_c:  37.882408","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2005-2006<br />mean_c:  30.484212","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2007-2008<br />mean_c:  31.996944","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2009-2010<br />mean_c:  63.922565","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2011-2012<br />mean_c:  17.902447","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2013-2014<br />mean_c:  13.585671","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2015-2016<br />mean_c:  14.424827"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(252,255,164,1)","dash":"solid"},"hoveron":"points","name":"Mono-n-butyl phthalate (MnBP_c)","legendgroup":"Mono-n-butyl phthalate (MnBP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":37.6521378165214,"r":7.30593607305936,"b":32.4782067247821,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517},"title":{"text":"Figure 1: Mean creatinine-adjusted phthalate concentration <br /> by NHANES cycle in women aged 20 - 44 (n = 2,754)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.346201743462},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,7.6],"tickmode":"array","ticktext":["2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"],"tickvals":[1,2,3,4,5,6,7],"categoryorder":"array","categoryarray":["2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":9.56413449564135},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NHANES Cycle","font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-30.0540358306196,665.77130629637],"tickmode":"array","ticktext":["0","200","400","600"],"tickvals":[0,200,400,600],"categoryorder":"array","categoryarray":["0","200","400","600"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":9.56413449564135},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Phthalate oncentration (ug/g creatinine)","font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":9.56413449564135},"y":0.949381327334083},"annotations":[{"text":"chemical_c","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"c8de7486016d":{"colour":{},"x":{},"y":{},"type":"scatter"}},"cur_data":"c8de7486016d","visdat":{"c8de7486016d":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
<p class="caption">plot of chunk spaghetti plot</p>
</div>

Immediately, it's clear that MEP stands out. Peaking during the 2005-2006 cycle at 628.92 \\(\mu\\)g creatinine, MEP represented more than six times the biomarker concentration of the next highest phthalate, MECPP. Following the peak, the concentration fell sharply in 2007-2008 and continued to decline through next four cycles. All other phthalates also show a general decline over time. 

What's special about MEP and why did it decline so sharply? At first, I thought maybe there was an issue with the data. Perhaps an error in the 2005-2006 cycle, or, since we looked at means, some extremely influential outliers that drove the mean upward. However, calculating the median (a statistic not susceptible to outliers) below confirms that MEP is still the highest phthalate by far, measuring more than 4 times the mean of the next highest phthalate, MECPP.  


```r
means_c %>% 
  filter(cycle == "2005-2006") %>% 
  select(cycle, chemical_c, median_c)
```

```
## # A tibble: 10 x 3
## # Groups:   cycle [1]
##    cycle     chemical_c                                        median_c
##    <chr>     <chr>                                                <dbl>
##  1 2005-2006 Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)   18.7  
##  2 2005-2006 Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)       13.2  
##  3 2005-2006 Mono-(2-ethyl)-hexyl phthalate (MEHP_c)              3.13 
##  4 2005-2006 Mono-(3-carboxypropyl) phthalate (MCPP_c)            1.84 
##  5 2005-2006 Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)    29.8  
##  6 2005-2006 Mono-benzyl phthalate (MBzP_c)                       8.76 
##  7 2005-2006 Mono-ethyl phthalate (MEP_c)                       132.   
##  8 2005-2006 Mono-isobutyl phthalate (MiBP_c)                     5.87 
##  9 2005-2006 Mono-isononyl phthalate (MiNP_c)                     0.917
## 10 2005-2006 Mono-n-butyl phthalate (MnBP_c)                     20.4
```

Now that we've gone through this sanity check, we're back to figuring out why MEP stands out from the pack. And to be honest, after scouring the bowels of the internet, I didn't find a smoking gun. We do know that MEP is the primary metabolite of Diethyl Phthalate (DEP), which, like other phthalates, is used as a plasticizer for rigid materials including toys and toothbrushes. Unlike other phthalates, however, DEP is also [used](https://pubchem.ncbi.nlm.nih.gov/compound/Diethyl-phthalate#section=Names-and-Identifiers) as a solvent in liquid cosmetics and perfumes. As such, the route of exposure is not only oral but also topical, perhaps explaining some of this unique trajectory. 

At this point, we might want to pull some information on industrial DEP usage in the US over the past 15 years and perhaps do some research on whether industry/policy changes circa 2005. Ah, but not so fast... After going down this rabbit hole, I learned some enlightening/frustrating information about the history of chemical reporting policies in the US. If this isn't your cup of tea, feel free to skip to the [following section](#back-to-the-numbers).

### __A brief historical interlude__

In 1976, Congress passed the [Toxic Substances Control Act](https://en.wikipedia.org/wiki/Toxic_Substances_Control_Act_of_1976), which sought to measure and regulate industrial usage of chemicals deemed to pose "unreasonable risk" to humans and the environment. 

The EPA was tasked with administration of the act and since passage, an inventory of about 84,000 chemicals has been established. In short, the [inventory](https://www.epa.gov/chemical-data-reporting) is problematic. First, it's only updated every four years. Second, it gives exemptions to smaller manufacturers and an ever-growing list of exempt chemicals. Finally (and most importantly), quantities are often reported in extremely wide ranges ("1,000,000 - <20,000,000 lbs" was an entry I saw in the 2016 data...) ([Zota et al, 2014](#references), [Sciences et al](#references)). 

Basically, I once assumed that some governing body has a solid grasp on the quantities and types of chemicals used in the US. I no longer assume this. 

In another facet of this regulatory picture, we have the [Consumer Product Safety Improvement Act of 2008](https://www.cpsc.gov/Regulations-Laws--Standards/Statutes/The-Consumer-Product-Safety-Improvement-Act). The act focuses on toxic substances in childrens' toys and banned presence of certain phthalates (check out this disturbing pile of [dolls](https://www.flickr.com/photos/cbpphotos/10928300625/) seized by US Customs in 2013 due to excess phthalate levels). 

One might think this would provide some clues on why MEP declined, but the timing doesn't work out- the act went into effect in 2009 and our trend started in 2004/2005. Additionally, DEP is not included in the scope, it's hard to ascribe the act as the root cause. It is possible, however, that the industry responded to pressure from advocacy groups and consumers, or in anticipation of further bans, and undertook formulation changes outside of (and prior to)  passage of the act. 

### Back to the numbers

Up until now, we've explored the temporal trends of common phthalates and did a bit of unsuccessful (but fun?) digging through the exciting world of toxic chemical legislative history. Now we will backtrack and summarize the average levels, as posed by Question 1. 

We proceed to summarizing the raw and creatinine-adjusted values from the 2015-2016 NHANES cycle. Mean and median values for both measures are reported in Table 1 below. Both are shown because there is 
a high degree of right-skew in the data. To illustrate this, here is a quick histogram of unadjusted MEP levels. 


```r
all_data %>% 
  ggplot(aes(x = mep)) +
  geom_histogram(binwidth = 500) +
  labs(x = "Unadjusted MEP (ng/mL)", 
       title = "Figure 2: MEP Histogram")
```

![plot of chunk MEP histogram](/figs/2020-05-30-phthalates/MEP histogram-1.png)

This right-skew is predictable - most people (thankfully) have very low levels of urinary MEP. As such, the median is a better measure of central tendency than the mean. We will look at both since some of the literature I reference below uses mean values and keeping the means will allow for comparison. 


```r
# calculating means and medians for phthalate values, not adjusted for creatinine
means_raw = 
all_data_c %>% 
  filter(cycle == "2015-2016") %>% 
  select(-c(gender:creatinine), -SEQN, -c(mecpp_c:minp_c)) %>% 
  pivot_longer(c(mecpp:mcnp), names_to = "chemical", values_to = "value") %>% 
  drop_na() %>% 
  mutate(chemical = recode(chemical, 
                           "mecpp"= "Mono-2-ethyl-5-carboxypentyl phthalate (MECPP)", 
                           "mnbp"= "Mono-n-butyl phthalate (MnBP)",
                           "mcpp"= "Mono-(3-carboxypropyl) phthalate (MCPP)",
                           "mep"= "Mono-ethyl phthalate (MEP)",
                           "mehhp"= "Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP)",
                           "mehp"= "Mono-(2-ethyl)-hexyl phthalate (MEHP)",
                           "mibp"= "Mono-isobutyl phthalate (MiBP)",
                           "meohp"= "Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP)", 
                           "mbzp"= "Mono-benzyl phthalate (MBzP)", 
                           "mcnp"= "Mono(carboxyisononyl) phthalate (MCNP)", 
                           "mcop"= "Mono(carboxyisoctyl) phthalate (MCOP)",
                           "minp"= "Mono-isononyl phthalate (MiNP)")) %>%
  group_by(chemical) %>% 
  summarize(mean = mean(value), median = median(value)) %>% 
  mutate(id = row_number())

# Calculating means and medians for adjusted values
means_c_15_16 = 
  means_c %>% 
  filter(cycle == "2015-2016") %>% 
  mutate(id = row_number()) %>% 
  select(id, chemical_c, mean_c, median_c)

# joining adjusted and un-adjusted into one table
left_join(means_c_15_16, means_raw, by = "id") %>% 
  select(c("cycle", "chemical", "mean", "median", "mean_c", "median_c")) %>% 
  rename("Chemical" = "chemical", "Adjusted Mean" = "mean_c", "Adjusted Median" = "median_c", "Mean" = "mean", "Median" = "median") %>% 
  knitr::kable(digits = 1, caption = "Table 1: Phthalate Concentration in Women Ages 20-44, per NHANES 2015-2016 Cycle.") %>% 
  kable_styling(c("striped", "bordered")) %>%
  add_header_above(c(" " = 2, "Unadjusted (ng/mL)" = 2, "Creatinine-Adjusted (ug/g creatinine)" = 2))
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>Table 1: Phthalate Concentration in Women Ages 20-44, per NHANES 2015-2016 Cycle.</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="2"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Unadjusted (ng/mL)</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Creatinine-Adjusted (ug/g creatinine)</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> cycle </th>
   <th style="text-align:left;"> Chemical </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Adjusted Mean </th>
   <th style="text-align:right;"> Adjusted Median </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP) </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 5.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP) </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 3.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-(2-ethyl)-hexyl phthalate (MEHP) </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 1.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-(3-carboxypropyl) phthalate (MCPP) </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 0.9 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 0.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-2-ethyl-5-carboxypentyl phthalate (MECPP) </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 9.3 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 8.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-benzyl phthalate (MBzP) </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 4.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-ethyl phthalate (MEP) </td>
   <td style="text-align:right;"> 171.8 </td>
   <td style="text-align:right;"> 39.3 </td>
   <td style="text-align:right;"> 131.8 </td>
   <td style="text-align:right;"> 33.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-isobutyl phthalate (MiBP) </td>
   <td style="text-align:right;"> 16.7 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 13.3 </td>
   <td style="text-align:right;"> 8.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-isononyl phthalate (MiNP) </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015-2016 </td>
   <td style="text-align:left;"> Mono-n-butyl phthalate (MnBP) </td>
   <td style="text-align:right;"> 19.0 </td>
   <td style="text-align:right;"> 11.7 </td>
   <td style="text-align:right;"> 14.4 </td>
   <td style="text-align:right;"> 9.9 </td>
  </tr>
</tbody>
</table>

So what do these values tell us? Are they safe? And what does "safe" even mean in this context?

Figuring out the answers gets us into a gray area. Some (myself included) would argue that no amount of plasticizers should be present in the human body. However, given how frequently we interact with plastics, this is probably not a realistic goal. 

Alternatively, we can draw certain conclusions about threshold values above which detrimental health effects were observed in past studies, and compare those values to what we saw in Table 1. But this, too, is tricky. For one thing, there's no way to ethically study the effects of phthalates in humans using the gold standard of study design - the randomized controlled trial (RCT). You can't just pick a group of pregnant women, split them up, and force one group to eat large amounts of phthalates. 

This leaves us with two options: animal studies and observational human studies. The former has the widely-acknowledged limitation that humans and lab animals (usually rats) are not, in fact, biologically equivalent. The latter has analytic limitations, the major one being unmeasured confounding. 

Confounding is a huge consideration in health/medical research and many epidemiologists spend their entire careers studying its effects and how it distorts measured relationships between exposures and outcomes (you can find a nice primer [here](https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/biases)). For our purposes it's sufficient to say that observational studies of phthalates might tell us, for instance, that women with high levels of urinary MEP gave birth to smaller babies, but we cannot say that phthalates were the cause unless we control for all other potential causes of small babies (randomization accomplishes this, hence the usage of RCTs in humans). Both options have pretty big limitations, but if we combine the knowledge from both, we can perhaps glean conclusions to help us make sense of Table 1. 

Here is what I found: 

* Several studies have established adverse effects in rats, as summarized by Lyche et al ([Lyche et al, 2009](#references)). The outcomes were primarily related to reproductive effects and included sperm count reduction in males ([NTP-CERHR Expert Panel Update on the Reproductive and Developmental Toxicity of Di(2-ethylhexyl) phthalate](#references) and delayed puberty in females [Grande et al, 2006](#references). Looking at the doses, however, these effects were observed in the range of 100 - 2000 mg chemical/kg animal per day. If we take a number in this range, say 375 mg/kg animal/day, we get a dose of roughly 150 mg per rat, per day (the average adult rat weighs about [400 grams](http://web.jhu.edu/animalcare/procedures/rat.html
). If we extrapolate this to humans, using an average weight of 68 kilograms (150 lbs) for women, we get that there are about 170 rats per human (not literally of course), meaning that the human equivalent of 150 mg is 25,500 mg, or 25 grams of phthalate (almost 1 oz) per day. This is pretty huge and I highly doubt that anyone is ingesting this much on a daily basis. 

* Human studies on reproductive outcomes are limited, but Polanska et al (Polanska et al, 2016) found an association between MEP and pregnancy duration in a cohort of Polish women. The other outcomes studied were weight. length, head and chest circumference of the baby and no significant associations were found between these outcomes and any other phthalate. In this study, the median creatinine-adjusted MEP was 22.7 \\(\mu\\)g/g creatinine, which is on par with the value calculated here, (33.2 \\(\mu\\)g/g creatinine). So barring unmeasured confounding in this study, we would conclude that MEP levels in US women are still too high and pose risk for adverse reproductive outcomes. 

* In a cohort of US women who gave birth to males, Swan et al (Swan et al, 2005) found an association between the boys' [anogenital distance](https://en.wikipedia.org/wiki/Anogenital_distance) and mothers' exposure to MEP, MnBP, MBzP, and MiBP, with MnBP having the biggest effect. In this study, the boys with the shortest anogenital distance (i.e. the most reproductive impairment) had median concentrations of MEP, MnBP, MBzP, and MiBP of 225 ng/mL, 24.5 ng/mL, 16.1 ng/mL, and 4.8 ng/mL, respectively (the authors did not adjust for creatinine). Aside from MiBP, these values are much higher than the median values computed above.  


# Conlusions 
At this point, I think we've successfully summarized phthalate exposure in US women aged 20-44 between 2003 and 2016. We've learned the following: 

* Overall phthalate levels have decreased between 2003 and 2016. The most dramatic decrease was observed in Mono-ethyl Phthalate, which has unique applications including use in cosmetics and personal care products. The reason for this sharp decline is not entirely clear, but the investigation led to some enlightening insights into the weaknesses of US toxic chemical tracking. 
* The exposure levels, as measured in urine samples, are comparable to previous studies using cohorts. Some of this work established associations between phthalate exposure during pregnancy and adverse effects on fetal reproductive development. The levels observed here, however, are much lower than toxic exposure levels in animal studies. 

In the next post, we'll address Question 2. 

Thank you for reading. Comments and questions are always welcome. 

# Further Reading

For more than you ever wanted to know about phthalates, check out this publicly available [report](https://www.ncbi.nlm.nih.gov/books/NBK215040/) published by the National Academies and this [document](https://www.epa.gov/sites/production/files/2015-09/documents/phthalates_actionplan_revised_2012-03-14.pdf) published by the EPA. For something a bit more entertaining, The Guardian published this pretty comprehensive [article](https://www.theguardian.com/lifeandstyle/2015/feb/10/phthalates-plastics-chemicals-research-analysis) in 2015. 


[^1]: Since 1 mg/dL = 10,000 ng/mL, we divide phthalate concentration by 10,000 to get the units to match, then take phthalate/creatinine. Most values are in the \\(1e-6\\) (micro) order of magnitude, so we express the final adjusted answer in micrograms phthalate per gram creatinine.


# References
- Zota, Ami R., et al. “Temporal Trends in Phthalate Exposures: Findings from the National Health and Nutrition Examination Survey, 2001–2010.” Environmental Health Perspectives, vol. 122, no. 3, 2014, pp. 235–241., doi:10.1289/ehp.1306681.
- Polańska, Kinga, et al. “Effect of Environmental Phthalate Exposure on Pregnancy Duration and Birth Outcomes.” International Journal of Occupational Medicine and Environmental Health, vol. 29, no. 4, 2016, pp. 683–697., doi:10.13075/ijomeh.1896.00691.
- Swan, Shanna H. “Environmental Phthalate Exposure in Relation to Reproductive Outcomes and Other Health Endpoints in Humans.” Environmental Research, vol. 108, no. 2, 2008, pp. 177–184., doi:10.1016/j.envres.2008.08.007.
- Duty, Susan M, et al. “The Relationship between Environmental Exposures to Phthalates and DNA Damage in Human Sperm Using the Neutral Comet Assay.” Environmental Health Perspectives, vol. 111, no. 9, 2003, pp. 1164–1169., doi:10.1289/ehp.5756.
- Sciences, Roundtable on Environmental Health, et al. “The Challenge: Chemicals in Today's Society.” Identifying and Reducing Environmental Health Risks of Chemicals in Our Society: Workshop Summary., U.S. National Library of Medicine, 2 Oct. 2014, www.ncbi.nlm.nih.gov/books/NBK268889/.
- Lyche, Jan L., et al. “Reproductive and Developmental Toxicity of Phthalates.” Journal of Toxicology and Environmental Health, Part B, vol. 12, no. 4, 2009, pp. 225–249., doi:10.1080/10937400903094091.
- NTP-CERHR Expert Panel Update on the Reproductive and Developmental Toxicity of Di(2-Ethylhexyl) Phthalate.” Reproductive Toxicology, vol. 22, no. 3, 2006, pp. 291–399., doi:10.1016/j.reprotox.2006.04.007.
- Grande, Simone Wichert, et al. “A Dose-Response Study Following In Utero and Lactational Exposure to Di(2-Ethylhexyl)Phthalate: Effects on Female Rat Reproductive Development.” Toxicological Sciences, vol. 91, no. 1, 2006, pp. 247–254., doi:10.1093/toxsci/kfj128.
