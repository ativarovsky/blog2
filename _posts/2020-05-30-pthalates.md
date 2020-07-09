---
title: Phthalate Exposure in U.S. Women of Reproductive Age - an NHANES Review, Part I 
author: "alice"
date: '2020-05-15'
excerpt: ""
layout: single
htmlwidgets: true
categories:
  - Environment
  - NHANES
---
# Motivation
Perhaps I’m the type of person that worries excessively about things outside of their control, but I’ve long been concerned about plastics leaching into our bodies from things like food packaging and personal care products. 

Some of these anxieties might be justified. A recent analysis of bio-specimen data from the National Health and Nutrition Examination Survey [NHANES](https://www.cdc.gov/nchs/nhanes/index.htm), the largest annual survey of the United States population, found nearly ubiquitous exposure to phthalates (Zota et al, 2014). This is particularly concerning for pregnant women because studies have found adverse effects, including shorter pregnancy duration and decreased head circumference (Polanska et al, 2016), as well as endocrine disrupting effects in males, including decreased anogenital distance (animal study) (Swan et al, 2005) and damage to sperm DNA (Duty et al, 2003). So how concerned should we be? 

After reviewing the literature, I wanted to answer the following questions: 

1. What phthalates are present in women in the US, what are the average levels of these phthalates, and how have these levels changed over time? 
2. Are phthalate levels disproportionately distributed through the population? Namely, is there an association with phthalates and socioeconomic status? 


## Background
Before we dive in to the data, we need a bit of priming on what phthalates are and why they're important. Phthalates are the most common category of industrial plasticizers (chemicals that alter the flexibility and rigidity of plastics) in use today. Due to their widespread presence in manufacturing, the metabolites of these chemicals (i.e the breakdown products when they enter the body) are now ubiquitously detectable in humans in the United States (Zota et al, 2014). Phthalates are of particular concern in women of reproductive age because they can easily cross the placenta and interact with fetal development during critical windows of pregnancy (@Polanska2016). Previous studies have established detrimental defects on reproductive system development in fetuses exposed to phthalates (@Swan2005). 

Applications of phthalates range from building materials, including flooring and adhesives, to personal care products, including nail polish and shampoo (@CDC). High molecular weight phthalates like butylbenzyl phthalate (BBzP), di(2-ethylhexyl) phthalate (DEHP) and diisononyl phthalate (DiNP) are commonly found in building materials. Low molecular weight materials like diethyl phthalate (DEP), di-n-butyl phthalate (DnBP) are more commonly found in personal care products and medications (@Zota2014).

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

### Question 1: What phthalates are present in women in the US, what are the average levels of these chemicals, and how have these levels changed over time?

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
<!--html_preserve--><div id="htmlwidget-8c831a721f1561d66cb8" style="width:600px;height:600px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-8c831a721f1561d66cb8">{"x":{"data":[{"x":[1,2,3,4,5,6,7],"y":[56.1317725095253,71.1213088191594,47.8599945680783,20.5723899794901,18.1954213545438,9.25407667870049,11.2410649769371],"text":["chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2003-2004<br />mean_c:  56.131773","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2005-2006<br />mean_c:  71.121309","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2007-2008<br />mean_c:  47.859995","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2009-2010<br />mean_c:  20.572390","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2011-2012<br />mean_c:  18.195421","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2013-2014<br />mean_c:   9.254077","chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />chemical_c: Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)<br />cycle: 2015-2016<br />mean_c:  11.241065"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,4,1)","dash":"solid"},"hoveron":"points","name":"Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)","legendgroup":"Mono-(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[37.9598466599974,47.9622325643997,26.1414726018942,12.5603936489677,11.8178805357524,5.87326787717119,6.95626987119515],"text":["chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2003-2004<br />mean_c:  37.959847","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2005-2006<br />mean_c:  47.962233","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2007-2008<br />mean_c:  26.141473","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2009-2010<br />mean_c:  12.560394","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2011-2012<br />mean_c:  11.817881","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2013-2014<br />mean_c:   5.873268","chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />chemical_c: Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)<br />cycle: 2015-2016<br />mean_c:   6.956270"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(27,12,66,1)","dash":"solid"},"hoveron":"points","name":"Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)","legendgroup":"Mono-(2-ethyl-5-oxohexyl) phthalate (MEOHP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[9.99278497880825,13.1671183141721,8.73815800086235,3.69438550065612,4.47177900622704,2.45005691556301,2.74735876731144],"text":["chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2003-2004<br />mean_c:   9.992785","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2005-2006<br />mean_c:  13.167118","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2007-2008<br />mean_c:   8.738158","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2009-2010<br />mean_c:   3.694386","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2011-2012<br />mean_c:   4.471779","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2013-2014<br />mean_c:   2.450057","chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />chemical_c: Mono-(2-ethyl)-hexyl phthalate (MEHP_c)<br />cycle: 2015-2016<br />mean_c:   2.747359"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(75,12,107,1)","dash":"solid"},"hoveron":"points","name":"Mono-(2-ethyl)-hexyl phthalate (MEHP_c)","legendgroup":"Mono-(2-ethyl)-hexyl phthalate (MEHP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[3.24823865307215,3.26268386364904,4.49724272815392,5.37775055234306,10.2908950162625,5.22063454376074,2.16577377054881],"text":["chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2003-2004<br />mean_c:   3.248239","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2005-2006<br />mean_c:   3.262684","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2007-2008<br />mean_c:   4.497243","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2009-2010<br />mean_c:   5.377751","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2011-2012<br />mean_c:  10.290895","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2013-2014<br />mean_c:   5.220635","chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />chemical_c: Mono-(3-carboxypropyl) phthalate (MCPP_c)<br />cycle: 2015-2016<br />mean_c:   2.165774"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(120,28,109,1)","dash":"solid"},"hoveron":"points","name":"Mono-(3-carboxypropyl) phthalate (MCPP_c)","legendgroup":"Mono-(3-carboxypropyl) phthalate (MCPP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[79.2952044186525,93.2461710029894,63.8871076234474,30.5874194281034,27.206444419551,14.5669955903277,19.3295550156809],"text":["chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2003-2004<br />mean_c:  79.295204","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2005-2006<br />mean_c:  93.246171","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2007-2008<br />mean_c:  63.887108","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2009-2010<br />mean_c:  30.587419","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2011-2012<br />mean_c:  27.206444","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2013-2014<br />mean_c:  14.566996","chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />chemical_c: Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)<br />cycle: 2015-2016<br />mean_c:  19.329555"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(165,44,96,1)","dash":"solid"},"hoveron":"points","name":"Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)","legendgroup":"Mono-2-ethyl-5-carboxypentyl phthalate (MECPP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[16.7459134329851,14.1392304270737,14.8716322228646,11.2232160904095,8.69832685691784,9.90055500966045,8.62197756618394],"text":["chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2003-2004<br />mean_c:  16.745913","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2005-2006<br />mean_c:  14.139230","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2007-2008<br />mean_c:  14.871632","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2009-2010<br />mean_c:  11.223216","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2011-2012<br />mean_c:   8.698327","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2013-2014<br />mean_c:   9.900555","chemical_c: Mono-benzyl phthalate (MBzP_c)<br />chemical_c: Mono-benzyl phthalate (MBzP_c)<br />cycle: 2015-2016<br />mean_c:   8.621978"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(207,68,70,1)","dash":"solid"},"hoveron":"points","name":"Mono-benzyl phthalate (MBzP_c)","legendgroup":"Mono-benzyl phthalate (MBzP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[481.359316089068,634.142881654234,272.356859134769,253.397947751734,194.520374750077,144.475701652891,131.846349595601],"text":["chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2003-2004<br />mean_c: 481.359316","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2005-2006<br />mean_c: 634.142882","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2007-2008<br />mean_c: 272.356859","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2009-2010<br />mean_c: 253.397948","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2011-2012<br />mean_c: 194.520375","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2013-2014<br />mean_c: 144.475702","chemical_c: Mono-ethyl phthalate (MEP_c)<br />chemical_c: Mono-ethyl phthalate (MEP_c)<br />cycle: 2015-2016<br />mean_c: 131.846350"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(237,105,37,1)","dash":"solid"},"hoveron":"points","name":"Mono-ethyl phthalate (MEP_c)","legendgroup":"Mono-ethyl phthalate (MEP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[6.397093519366,9.85433048680007,12.5905056859069,12.7110541594219,10.8162132252463,11.7444695550205,13.2512655659892],"text":["chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2003-2004<br />mean_c:   6.397094","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2005-2006<br />mean_c:   9.854330","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2007-2008<br />mean_c:  12.590506","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2009-2010<br />mean_c:  12.711054","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2011-2012<br />mean_c:  10.816213","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2013-2014<br />mean_c:  11.744470","chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />chemical_c: Mono-isobutyl phthalate (MiBP_c)<br />cycle: 2015-2016<br />mean_c:  13.251266"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(251,154,6,1)","dash":"solid"},"hoveron":"points","name":"Mono-isobutyl phthalate (MiBP_c)","legendgroup":"Mono-isobutyl phthalate (MiBP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[1.57438881151627,2.68132352322907,1.87878476996346,3.39540469331367,4.90755524915638,3.3425619064989,2.20937592907465],"text":["chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2003-2004<br />mean_c:   1.574389","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2005-2006<br />mean_c:   2.681324","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2007-2008<br />mean_c:   1.878785","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2009-2010<br />mean_c:   3.395405","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2011-2012<br />mean_c:   4.907555","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2013-2014<br />mean_c:   3.342562","chemical_c: Mono-isononyl phthalate (MiNP_c)<br />chemical_c: Mono-isononyl phthalate (MiNP_c)<br />cycle: 2015-2016<br />mean_c:   2.209376"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(247,208,60,1)","dash":"solid"},"hoveron":"points","name":"Mono-isononyl phthalate (MiNP_c)","legendgroup":"Mono-isononyl phthalate (MiNP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7],"y":[37.8824076946198,30.4842116231416,31.9969441199587,63.9225654739426,17.9024470402083,13.5856706047327,14.4248270536611],"text":["chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2003-2004<br />mean_c:  37.882408","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2005-2006<br />mean_c:  30.484212","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2007-2008<br />mean_c:  31.996944","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2009-2010<br />mean_c:  63.922565","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2011-2012<br />mean_c:  17.902447","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2013-2014<br />mean_c:  13.585671","chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />chemical_c: Mono-n-butyl phthalate (MnBP_c)<br />cycle: 2015-2016<br />mean_c:  14.424827"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(252,255,164,1)","dash":"solid"},"hoveron":"points","name":"Mono-n-butyl phthalate (MnBP_c)","legendgroup":"Mono-n-butyl phthalate (MnBP_c)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":37.6521378165214,"r":7.30593607305936,"b":32.4782067247821,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517},"title":{"text":"Figure 1: Mean creatinine-adjusted phthalate concentration <br /> by NHANES cycle in women aged 20 - 44 (n = 2,754)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.346201743462},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,7.6],"tickmode":"array","ticktext":["2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"],"tickvals":[1,2,3,4,5,6,7],"categoryorder":"array","categoryarray":["2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":9.56413449564135},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NHANES Cycle","font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-30.0540358306196,665.77130629637],"tickmode":"array","ticktext":["0","200","400","600"],"tickvals":[0,200,400,600],"categoryorder":"array","categoryarray":["0","200","400","600"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":9.56413449564135},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Phthalate oncentration (ug/g creatinine)","font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":9.56413449564135},"y":0.949381327334083},"annotations":[{"text":"chemical_c","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.9551681195517},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"67756df0fb6c":{"colour":{},"x":{},"y":{},"type":"scatter"}},"cur_data":"67756df0fb6c","visdat":{"67756df0fb6c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
</div>
Immediately, it's clear that MEP stands out. Peaking during the 2005-2006 cycle at 628.92 $\mu$g/g creatinine, MEP represented more than six times the biomarker concentration of the next highest phthalate, MECPP. Following the peak, the concentration fell sharply in 2007-2008 and continued to decline through next four cycles. All other phthalates also show a general decline over time. 

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
Now that we've gone through this sanity check, we're back to figuring out why MEP stands out from the pack. And to be honest, after scouring the bowels of the internet, I didn't find a smoking gun. We do know that MEP is the primary metabolite of Diethyl Phthalate (DEP), which, like other phthalates, is used as a plasticizer for rigid materials including toys and toothbrushes. Unlike other phthalates, however, DEP is also used as a solvent in liquid cosmetics and perfumes [@CDC_DEP]. As such, the route of exposure is not only oral but also topical, perhaps explaining some of this unique trajectory. 

At this point, we might want to pull some information on industrial DEP usage in the US over the past 15 years and perhaps do some research on whether industry/policy changes circa 2005. Ah, but not so fast... After going down this rabbit hole, I learned some enlightening/frustrating information about the history of chemical reporting policies in the US. If this isn't your cup of tea, feel free to skip the following section. 

### __A brief historical interlude__
In 1976, Congress passed the [Toxic Substances Control Act](https://en.wikipedia.org/wiki/Toxic_Substances_Control_Act_of_1976), which sought to measure and regulate industrial usage of chemicals deemed to pose "unreasonable risk" to humans and the environment. The EPA was tasked with administration of the act and since passage, an inventory of about 84,000 chemicals has been established. In short, the [inventory](https://www.epa.gov/chemical-data-reporting) is problematic. First, it's only updated every four years. Second, it gives exemptions to smaller manufacturers and an ever-growing list of exempt chemicals. Finally (and most importantly), quantities are often reported in extremely wide ranges ("1,000,000 - <20,000,000 lbs" was an entry I saw in the 2016 data...) (@Zota2014, @Roundtable2014). Basically, I once assumed that some governing body has a solid grasp on the quantities and types of chemicals used in the US. I no longer assume this. 

In another facet of this regulatory picture, we have the [Consumer Product Safety Improvement Act of 2008](https://www.cpsc.gov/Regulations-Laws--Standards/Statutes/The-Consumer-Product-Safety-Improvement-Act). The act focuses on toxic substances in children's toys and banned presence of certain phthalates (check out this disturbing pile of [dolls](https://www.flickr.com/photos/cbpphotos/10928300625/) seized by US Customs in 2013 due to excess phthalate levels). One might think this would provide some clues on why MEP declined, but the timing doesn't work out- the act went into effect in 2009 and our trend started in 2004/2005. Additionally, DEP is not included in the scope, it's hard to ascribe the act as the root cause. It is possible, however, that the industry responded to pressure from advocacy groups and consumers, or in anticipation of further bans, and undertook formulation changes outside of (and prior to)  passage of the act. 


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

![center](/figs/2020-05-30-pthalates/MEP histogram-1.png)

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

This leaves us with two options: animal studies and observational human studies. The former has the widely-acknowledged limitation that humans and lab animals (usually rats) are not, in fact, biologically equivalent. The latter has analytic limitations, the major one being unmeasured confounding. Confounding is a huge consideration in health/medical research and many epidemiologists spend their entire careers studying its effects and how it distorts measured relationships between exposures and outcomes (you can find a nice primer [here](https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/biases)). For our purposes it's sufficient to say that observational studies of phthalates might tell us, for instance, that women with high levels of urinary MEP gave birth to smaller babies, but we cannot say that phthalates were the cause unless we control for all other potential causes of small babies (randomization accomplishes this, hence the usage of RCTs in humans). Both options have pretty big limitations, but if we combine the knowledge from both, we can perhaps glean conclusions to help us make sense of Table 1. 

Here is what I found: 

* Several studies have established adverse effects in rats, as summarized by Lyche et al (@Lyche2009). The outcomes were primarily related to reproductive effects and included sperm count reduction in males (@Kavlock2006) and delayed puberty in females (@Grande2006). Looking at the doses, however, these effects were observed in the range of 100 - 2000 mg chemical/kg animal per day. If we take a number in this range, say 375 mg/kg animal/day, we get a dose of roughly 150 mg per rat, per day (the average adult rat weighs about [400 grams](http://web.jhu.edu/animalcare/procedures/rat.html
).) If we extrapolate this to humans, using an average weight of 68 kilograms (150 lbs) for women, we get that there are about 170 rats per human (not literally of course), meaning that the human equivalent of 150 mg is 25,500 mg, or 25 grams of phthalate (almost 1 oz) per day. This is pretty huge and I highly doubt that anyone is ingesting this much on a daily basis. 

* Human studies on reproductive outcomes are limited, but Polanska et al (@Polanska2016) found an association between MEP and pregnancy duration in a cohort of Polish women. The other outcomes studied were weight. length, head and chest circumference of the baby and no significant associations were found between these outcomes and any other phthalate. In this study, the median creatinine-adjusted MEP was 22.7 $\mu$g/g creatinine, which is on par with the value calculated here, (33.2 $\mu$g/g creatinine). So barring unmeasured confounding in this study, we would conclude that MEP levels in US women are still too high and pose risk for adverse reproductive outcomes. 

* In a cohort of US women who gave birth to males, Swan et al (@Swan2005) found an association between the boys' [anogenital distance](https://en.wikipedia.org/wiki/Anogenital_distance) and mothers' exposure to MEP, MnBP, MBzP, and MiBP, with MnBP having the biggest effect. In this study, the boys with the shortest anogenital distance (i.e. the most reproductive impairment) had median concentrations of MEP, MnBP, MBzP, and MiBP of 225 ng/mL, 24.5 ng/mL, 16.1 ng/mL, and 4.8 ng/mL, respectively (the authors did not adjust for creatinine). Aside from MiBP, these values are much higher than the median values computed above.  


## Question 2: Are phthalate levels disproportionately distributed through the population? Namely, is there an association with phthalates and socioeconomic status? 

Now that we understand a bit about phthalates, it's interesting to explore whether their distributions vary between certain groups. For instance, do women with higher income have lower exposure to some/all phthalates. Or perhaps vice-versa? What about education? Why might this be the case? Well, it's possible that phthalates like MEP are present predominantly in inexpensive consumer products, thereby increasing risk for women who purchase them. And because manufacturers are not required to disclose usage of phthalates (as discussed in our historical interlude above), it's difficult to track them from the source. Understanding impacted groups gives us clues and insights into the mechanisms of action of these chemicals.  

Socioeconomic status is not a straightforward parameter to quantify. Often, it involves complex relationships between income, inherited wealth, education levels, race, and marital status. Because we are limited to the variables measured in NHANES, we will look at education as one proxy, and income as another. 

To figure out the effects of these variables on phthalate levels, we will do two things. The first is an exploratory visualization (mostly because these are nice to look at), and the second is a more rigorous set of statistical regression models. But first, (of course) we have to do a bit more data manipulation.  

### Recode income and education variables
Income is measured in NHANES using two categorical variables, __annual household income__ and __annual family income__. Both variables are coded using the same (somewhat non-intuitive) scale: 

* 1: $0 - 4,999
* 2: $5,000 - 9,999
* 3: $10,000 - 14,999
* 4: $15,000 - 19,999
* 5: $20,000 - 24,999
* 6: $25,000 - 34,999
* 7: $35,000 - 44,999
* 8: $45,000 - 54,999
* 9: $55,000 - 64,999
* 10: $65,000 - 74,999
* 11: $\ge$ $75,000 (2003-2004 and 2005-2006 cycles only)
* 12: $\ge$ 20,000
* 13: < 20,000
* 14: $75,000 - 99,999 (2007-2008 cycles onward)
* 15: $\ge$ $100,000

It's not immediately clear which variable to use, but one would guess that they're highly correlated. Indeed:


```r
all_data_c %>% 
  select(household_income, family_income) %>% 
  cor(use = "complete.obs", method = "pearson")
```

```
##                  household_income family_income
## household_income        1.0000000     0.9095916
## family_income           0.9095916     1.0000000
```

Since the Pearson correlation coefficient is very high, we can choose either. I chose annual family income to account for younger women, or women that reside with family. 

Next, I wanted to simplify analysis, make the levels more intuitive, and represent income levels in rough accordance with US class breakdown (poor, lower-middle class, middle and upper- middle class, and upper class). The income variable was collapsed into four annual income levels: 

* level 1: < $20,000 
* level 2: $20,000 - 45,000 
* level 3: $45,000 - 100,000 
* level 4: > $100,000  

We will recode the education variable per the levels above, drop refused/don't know/missing income observations, and ask R to interpret the variable type as categorical:

```r
all_data_c = 
  all_data_c %>% 
  select(- household_income) %>% 
  mutate(family_income = 
           if_else(family_income %in% c(1:4, 13), 1, 
                   if_else(family_income %in% c(5:7), 2, 
                           if_else(family_income %in% c(8:11, 14), 3, 
                                   if_else(family_income == 15, 4, 20)
                                   )
                           )
                   )
  ) %>%
  filter(!family_income %in% c(20, NA)) %>% 
  mutate(family_income = as.factor(family_income))
```

Working with the education variable is a bit more straightforward. The NHANES categories for adults over 20 years of age are as follows: 

* 1: < 9th grade
* 2: 9-11th grade
* 3: High school grad/GED
* 4: Some college/AA degree
* 5: College grad and above

Thus, all we need to do is drop the refuse/don't know/missing education observations, fix the variable type to categorical, and we're ready to roll. 

```r
all_data_c = 
  all_data_c %>% 
  drop_na(education) %>% 
  filter(education %in% c(1:5)) %>% 
  mutate(education = as.factor(education))
```

### Visualization 
Here we'll look at some boxplots to eyeball trends. We won't plot every phthalate because there's a lot of them and the intent here is exploratory. Instead we'll choose a handful of common ones, including those from both the low and high molecular weight categories. 


```r
# relabel education and income
box_plot_data = 
  all_data_c %>% 
    mutate(
      education = recode(education, "1" = "< 9th grade", "2" = "< High school", "3" = "High school grad/GED", "4" = "Some college/AA degree", "5" = "College graduate and above", .default = NULL, .missing = NULL), 
      family_income = recode(family_income, "1" = "< $20,000", "2" = "$20,000 - 45,000", "3" = "$45,000 - 100,000 ", "4" = "> $100,000", .default = NULL, .missing = NULL)
      )

# income boxplots
mep_1 = 
  box_plot_data %>% 
    ggplot(aes(x = family_income, y = log(mep_c))) + 
    geom_boxplot() +
    labs(
      title = "Log MEP vs Annual Family Income",
      x = "Annual Family Income",
      y = "Log MEP"
    )

mehp_1 = 
  box_plot_data %>% 
    ggplot(aes(x = family_income, y = log(mehp_c))) + 
    geom_boxplot() +
    labs(
      title = "Log MEHP vs Annual Family Income",
      x = "Annual Family Income",
      y = "Log MEHP"
    )

minp_1 = 
  box_plot_data %>% 
    ggplot(aes(x = family_income, y = log(minp_c))) + 
    geom_boxplot() +
    labs(
      title = "Log MINP vs Annual Family Income",
      x = "Annual Family Income",
      y = "Log MINP"
    )

grid.arrange(mep_1, mehp_1, minp_1, top = "Figure 3: Income vs Phthalates")
```

![center](/figs/2020-05-30-pthalates/boxplots-1.png)

```r
# education boxplots
mep_2 = 
  box_plot_data %>% 
    ggplot(aes(x = education, y = log(mep_c))) + 
    geom_boxplot() +
    labs(
      title = "Log MEP vs Education",
      x = "Annual Family Income",
      y = "Log MEP"
    )

mehp_2 = 
  box_plot_data %>% 
    ggplot(aes(x = education, y = log(mehp_c))) + 
    geom_boxplot() +
    labs(
      title = "Log MEHP vs Education",
      x = "Education",
      y = "Log MEHP"
    )

minp_2 = 
  box_plot_data %>% 
    ggplot(aes(x = family_income, y = log(minp_c))) + 
    geom_boxplot() +
    labs(
      title = "Log MINP vs Education",
      x = "Education",
      y = "Log MINP"
    )

grid.arrange(mep_2, mehp_2, minp_2, top = "Figure 4: Education vs Phthalates")
```

![center](/figs/2020-05-30-pthalates/boxplots-2.png)
<br>
Admittedly, these boxplots are not as earth-shattering as I'd hoped. There's definitely a lot of noise and not a definitive consistent pattern. But we can still glean some weak-ish trends. MEP biomarker levels show a slight decline with increasing income and education, MiNP shows the opposite trend, and MEHP fluctuates with both. We'll need more rigorous analysis to pin-point relationships. Which means, it's time to [model](https://en.wikipedia.org/wiki/America%27s_Next_Top_Model). 

### Regression Models
The relationship between SES and phthalate levels is quantified below using multivariable linear regression models (a nice primer on regression modeling can be found [here](https://academic.oup.com/ejcts/article/55/2/179/5265263)). Phthalates are modeled as outcome variables and are log-transformed to address the right-skew discussed above. Income and education are modeled as independent variables, and age is included as a covariate to adjust for potential confounding effects. Creatinine is added as a covariate to adjust for urine dilution.

This is where it gets a little tricky. NHANES uses a complex, stratified survey design (detailed [here](https://www.cdc.gov/nchs/data/series/sr_02/sr02_162.pdf)), along with over-sampling in accordance with the US census. Simply put, the ~10,000 individuals that participate in NHANES do not constitute a perfect random sample of the entire US population. Inevitably, some demographic groups will be over- or under-represented and some might not be represented at all. Therefore, each individual is assigned a sampling weight, which allows us to extrapolate results to the US population.

Because of this, it's not enough to put the variables of interest into a model and hit play. Instead, we must utilize the `survey` package and the three survey variables, `WTMEC2YR, SDMVPSU,SDMVSTRA` that we've ignored up to now. The way these variables work is somewhat complicated and we won't go into detail, but you can read all about it [here](https://www.cdc.gov/nchs/data/series/sr_02/sr02-184-508.pdf). For our purposes, we need two steps: 1. create a survey design object using the survey variables and 2. carry out regression modeling using `svyglm`. [This site](https://stats.idre.ucla.edu/r/faq/how-can-i-do-regression-estimation-with-survey-data/) provides a reference for both. 


```r
# adjust weighting variable for aggregation of 7 cycles
all_data_c$WTMEC14YR = all_data_c$WTMEC2YR/7

# create log variables 
all_data_c = 
  all_data_c %>% 
  mutate(log_mecpp = log(mecpp),
         log_mnbp = log(mnbp),
         log_mcpp = log(mcpp),
         log_mep = log(mep),
         log_mehhp = log(mehhp),
         log_mehp = log(mehp),
         log_mibp = log(mibp),
         log_meohp = log(meohp),
         log_mbzp = log(mbzp),
         log_mcnp = log(mcnp),
         log_mcop = log(mcop),
         log_minp = log(minp)
         )

# selecting needed variables
all_data_m = 
  all_data_c %>% 
  filter(cycle %in% c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012", "2013-2014", "2015-2016")) %>%
  select(age, education, family_income, SDMVPSU, SDMVSTRA, WTMEC14YR, creatinine, log_mecpp, log_mnbp, log_mcpp, log_mep, log_mehhp, log_mehp, log_mibp, log_meohp, log_mbzp, log_mcnp, log_mcop, log_minp)

# create survey design variable
svy_design = svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, data = all_data_m, weights = ~WTMEC14YR, nest = TRUE)
```

Whew. Now we're finally ready to model. Based on the variables we're using, our theoretical model takes the following form. Note that because we have two multi-level categorical variables, we need to create _n-1_ "dummy" variables, n being the number of levels (more on dummy variables [here](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-dummy-coding/)). 

$$y_i = \beta_0 +\beta_1*income_1+\beta_2*income_2+\beta_3*income_3+\beta_4*education_1+\beta_5*education_2+\beta_6*education_3+\beta_7*education_4+\beta_8*age+\beta_9*creatinine+\epsilon_i $$
$$income_1 = \{1\ for\ 20k-45k \ vs <20k, \ 0\ otherwise\}$$
$$income_2 = \{1\ for\ 45k-100k \ vs <20k, \ 0\ otherwise\}$$
$$income_3 = \{1\ for\ >100k\ vs <20k, \ 0\ otherwise\}$$
$$education_1 = \{1\ for\ 9-11th\ grade \ vs <9th\ grade, \ 0\ otherwise\}$$
$$education_2 = \{1\ for\ High\ school\ grad/GED \ vs <9th\ grade, \ 0\ otherwise\}$$
$$education_3 = \{1\ for\ Some\ college/AA\ degree \ vs <9th\ grade, \ 0\ otherwise\}$$
$$education_4 = \{1\ for\ College\ and\ above \ vs <9th\ grade, \ 0\ otherwise\}$$

The fitted models are calculated below. It'll be a lot of numbers but bear with me. We'll get through it together.  

```r
mecpp_model = svyglm(log_mecpp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mnbp_model = svyglm(log_mnbp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mcpp_model = svyglm(log_mcpp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m)

mep_model = svyglm(log_mep ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mehhp_model = svyglm(log_mehhp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mehp_model = svyglm(log_mehp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mibp_model = svyglm(log_mibp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

meohp_model = svyglm(log_meohp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mbzp_model = svyglm(log_mbzp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mcnp_model = svyglm(log_mcnp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

mcop_model = svyglm(log_mcop ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

minp_model = svyglm(log_minp ~ age + family_income + education + creatinine, design = svy_design, data = all_data_m) 

# arranging models into tables
tab_model(mecpp_model, mnbp_model, mcpp_model, show.stat = TRUE, show.aic = FALSE, title = "Table 2: Regression Results for MECPP, MnBP, and MCPP")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">Table 2: Regression Results for MECPP, MnBP, and MCPP</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mecpp</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mnbp</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mcpp</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">2.24</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.89&nbsp;&ndash;&nbsp;2.58</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">12.67</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.92&nbsp;&ndash;&nbsp;1.63</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">7.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.74&nbsp;&ndash;&nbsp;-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.97</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.052</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">age</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.79</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.433</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.00&nbsp;&ndash;&nbsp;0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">2.45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>0.016</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.801</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.17&nbsp;&ndash;&nbsp;0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.62</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.537</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.21&nbsp;&ndash;&nbsp;0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.94</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.351</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.29&nbsp;&ndash;&nbsp;-0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-2.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>0.031</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.03&nbsp;&ndash;&nbsp;0.32</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">2.31</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.023</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.27&nbsp;&ndash;&nbsp;0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.42</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.159</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.26&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.78</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.078</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.52&nbsp;&ndash;&nbsp;-0.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-3.66</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.36</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.56&nbsp;&ndash;&nbsp;-0.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-3.56</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>0.001</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.33&nbsp;&ndash;&nbsp;0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.291</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.44&nbsp;&ndash;&nbsp;0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.51</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.133</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.25&nbsp;&ndash;&nbsp;0.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.867</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.14&nbsp;&ndash;&nbsp;0.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.84</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.403</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.40&nbsp;&ndash;&nbsp;0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.45</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.150</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.37&nbsp;&ndash;&nbsp;0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.288</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.07&nbsp;&ndash;&nbsp;0.42</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">1.41</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.161</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.44&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.89</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.062</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.15</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.39&nbsp;&ndash;&nbsp;0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.220</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.03&nbsp;&ndash;&nbsp;0.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">1.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.095</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [5]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.41&nbsp;&ndash;&nbsp;0.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.53</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.130</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.48&nbsp;&ndash;&nbsp;0.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.086</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.07&nbsp;&ndash;&nbsp;0.48</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">1.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.149</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">creatinine</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">20.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">29.62</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">24.35</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / R<sup>2</sup> adjusted</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.254 / -18.308</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.406 / -14.376</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.261 / -18.117</td>
</tr>

</table>

```r
tab_model(mep_model, mehhp_model, mehp_model, show.stat = TRUE, show.aic = FALSE,  title = "Table 3: Regression Results for MEP, MEHHP, and MEHP")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">Table 3: Regression Results for MEP, MEHHP, and MEHP</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mep</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mehhp</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mehp</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">2.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.99&nbsp;&ndash;&nbsp;2.95</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">10.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.51</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">1.15&nbsp;&ndash;&nbsp;1.88</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">8.14</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.06&nbsp;&ndash;&nbsp;0.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">1.60</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.113</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">age</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01&nbsp;&ndash;&nbsp;0.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">4.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.904</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.40</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.164</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.27&nbsp;&ndash;&nbsp;0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.66</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.512</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.16&nbsp;&ndash;&nbsp;0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.35</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.729</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.12&nbsp;&ndash;&nbsp;0.15</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.818</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.28&nbsp;&ndash;&nbsp;0.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.41</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.685</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.02&nbsp;&ndash;&nbsp;0.33</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">2.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>0.029</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.01&nbsp;&ndash;&nbsp;0.26</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">1.87</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.065</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.60</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.85&nbsp;&ndash;&nbsp;-0.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-4.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.38</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.57&nbsp;&ndash;&nbsp;-0.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-3.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.39&nbsp;&ndash;&nbsp;-0.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-2.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>0.015</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.18&nbsp;&ndash;&nbsp;0.41</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.461</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.36&nbsp;&ndash;&nbsp;0.14</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.398</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.40&nbsp;&ndash;&nbsp;0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.146</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.00&nbsp;&ndash;&nbsp;0.58</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.94</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.055</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.37&nbsp;&ndash;&nbsp;0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.278</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.38&nbsp;&ndash;&nbsp;0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.116</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.15</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.16&nbsp;&ndash;&nbsp;0.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.94</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.347</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.34&nbsp;&ndash;&nbsp;0.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.94</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.351</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.38&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.92</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.058</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [5]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.42&nbsp;&ndash;&nbsp;0.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.58</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.563</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.31&nbsp;&ndash;&nbsp;0.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.61</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.542</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.39&nbsp;&ndash;&nbsp;0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.80</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.075</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">creatinine</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">19.20</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">22.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">19.73</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / R<sup>2</sup> adjusted</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.226 / -19.032</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.261 / -18.115</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.185 / -20.097</td>
</tr>

</table>

```r
tab_model(mibp_model, meohp_model, mbzp_model, show.stat = TRUE, show.aic = FALSE,  title = "Table 4: Regression Results for MIBP, MEOHP, and MBZP")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">Table 4: Regression Results for MIBP, MEOHP, and MBZP</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mibp</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log meohp</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mbzp</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.79&nbsp;&ndash;&nbsp;1.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">8.25</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.87&nbsp;&ndash;&nbsp;1.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">6.73</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.88</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">0.54&nbsp;&ndash;&nbsp;1.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">5.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">age</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.250</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.94</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.352</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-0.84</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.404</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.05&nbsp;&ndash;&nbsp;0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.263</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.15&nbsp;&ndash;&nbsp;0.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.816</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.19&nbsp;&ndash;&nbsp;0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.295</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.15&nbsp;&ndash;&nbsp;0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.464</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.20</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.05&nbsp;&ndash;&nbsp;0.35</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">2.64</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>0.010</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.34&nbsp;&ndash;&nbsp;-0.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-3.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>0.002</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.12&nbsp;&ndash;&nbsp;0.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.44</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.658</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.39</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.57&nbsp;&ndash;&nbsp;-0.20</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-4.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.76&nbsp;&ndash;&nbsp;-0.42</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-6.90</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.33&nbsp;&ndash;&nbsp;0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.32</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.189</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.42&nbsp;&ndash;&nbsp;0.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.36</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.177</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.26</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">0.03&nbsp;&ndash;&nbsp;0.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">2.19</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>0.031</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.36</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.56&nbsp;&ndash;&nbsp;-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-3.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.41&nbsp;&ndash;&nbsp;0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.40</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.164</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.19&nbsp;&ndash;&nbsp;0.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.34</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.732</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.46&nbsp;&ndash;&nbsp;-0.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-3.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.001</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.40&nbsp;&ndash;&nbsp;0.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-1.49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.140</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.15&nbsp;&ndash;&nbsp;0.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.61</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.541</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [5]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.47&nbsp;&ndash;&nbsp;-0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-2.96</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.004</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.34&nbsp;&ndash;&nbsp;0.14</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.399</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.14</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.37&nbsp;&ndash;&nbsp;0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.249</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">creatinine</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">34.30</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">22.93</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">31.87</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / R<sup>2</sup> adjusted</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.430 / -13.757</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.282 / -17.585</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.414 / -14.169</td>
</tr>

</table>

```r
tab_model(mcnp_model, mcop_model, minp_model, show.stat = TRUE, show.aic = FALSE,  title = "Table 5: Regression Results for MCNP, MCOP, and MINP")
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">Table 5: Regression Results for MCNP, MCOP, and MINP</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mcnp</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log mcop</th>
<th colspan="4" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">log minp</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.32&nbsp;&ndash;&nbsp;0.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.872</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.75</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">1.33&nbsp;&ndash;&nbsp;2.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">8.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.38&nbsp;&ndash;&nbsp;0.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-0.69</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.489</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">age</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.70</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.093</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.02&nbsp;&ndash;&nbsp;-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-2.52</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>0.013</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.01&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-1.54</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.127</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.21&nbsp;&ndash;&nbsp;0.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-1.46</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.149</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.06</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.28&nbsp;&ndash;&nbsp;0.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.51</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.612</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.14&nbsp;&ndash;&nbsp;0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-0.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.639</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.13&nbsp;&ndash;&nbsp;0.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.08</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.938</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.27&nbsp;&ndash;&nbsp;0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.70</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.485</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.14&nbsp;&ndash;&nbsp;0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-0.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.860</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">family_income [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.23&nbsp;&ndash;&nbsp;0.14</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.50</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.620</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.15</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.12&nbsp;&ndash;&nbsp;0.42</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">1.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.275</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.02</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.16&nbsp;&ndash;&nbsp;0.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.24</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.809</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [2]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.04</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.20&nbsp;&ndash;&nbsp;0.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.33</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.746</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.38&nbsp;&ndash;&nbsp;0.20</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">-0.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.558</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">-0.05</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.22&nbsp;&ndash;&nbsp;0.13</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">-0.52</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.605</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [3]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.09&nbsp;&ndash;&nbsp;0.32</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.288</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.30&nbsp;&ndash;&nbsp;0.32</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.942</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.10&nbsp;&ndash;&nbsp;0.23</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.83</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.409</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [4]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.17</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-0.04&nbsp;&ndash;&nbsp;0.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.62</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.109</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.20&nbsp;&ndash;&nbsp;0.37</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.61</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.543</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.09</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.08&nbsp;&ndash;&nbsp;0.26</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">0.99</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.327</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">education [5]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.26</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.02&nbsp;&ndash;&nbsp;0.51</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">2.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.038</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.15</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">-0.16&nbsp;&ndash;&nbsp;0.47</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">0.95</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">0.343</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.10</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">-0.08&nbsp;&ndash;&nbsp;0.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">1.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">0.271</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">creatinine</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">19.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">0.01&nbsp;&ndash;&nbsp;0.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">16.41</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9"><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">0.00&nbsp;&ndash;&nbsp;0.00</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">9.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2225</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2225</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="4">2588</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / R<sup>2</sup> adjusted</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.260 / -18.368</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.171 / -20.687</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="4">0.051 / -23.553</td>
</tr>

</table>

<br>
Let's dissect the results. If we look at income first, we see generally negative values for the parameter estimates. This signals that higher income generally corresponds with lower urinary phthalate concentration. The parameters are pretty small and most are not statistically significant at an alpha level of 0.05. Notably, at the highest family income category, family_income_4 (meaning the highest earners vs the lowest earners), the estimates tend to be higher and most of them __are__ statistically significant. This means that, in general, there is only an association between annual family income and phthalate exposure at the extremes of income. 

Looking at education, we also see very small estimates for the beta parameters and almost all of them are not statistically significant. 

So is there an association between socioeconomic status and phthalate exposure? Eh... not really. (I know, that's a very scientific answer.) There are some weak relationships but based on data gathered from 13 years of observation, phthalates seem to be everywhere and permeate all levels of society. But hey, at least we know they've decreased from the early 2000s. 

# Conlusions 
After all this, I think we've successfully summarized phthalate exposure in US women aged 20-44 between 2003 and 2016. We've learned the following: 

* Overall phthalate levels have decreased between 2003 and 2016. The most dramatic decrease was observed in Mono-ethyl Phthalate, which has unique applications including use in cosmetics and personal care products. The reason for this sharp decline is not entirely clear, but the investigation led to some enlightening insights into the weaknesses of US toxic chemical tracking. 
* The exposure levels, as measured in urine samples, are comparable to previous studies using cohorts. Some of this work established associations between phthalate exposure during pregnancy and adverse effects on fetal reproductive development. The levels observed here, however, are much lower than toxic exposure levels in animal studies. 
* Multivariable regression modeling did not establish strong associations between phthalate exposure and measures of socioeconomic status. 

I hope you agree that we've learned a lot of good stuff. It's clear to me that we don't really understand the full story. Research on phthalates didn't start until the early 2000s and plastics themselves are a relatively young addition to the human toolkit. So there's no question that further work needs to be done. Until then, I feel _a little_ less anxious about phthalates. Still, I will definitely not be sticking plastic in the microwave and if you're pregnant, you probably shouldn't either. 

Thank you for reading. Comments and questions are always welcome. 

# Further Reading
For more than you ever wanted to know about phthalates, check out this publicly available [report](https://www.ncbi.nlm.nih.gov/books/NBK215040/) published by the National Academies and this [document](https://www.epa.gov/sites/production/files/2015-09/documents/phthalates_actionplan_revised_2012-03-14.pdf) published by the EPA. For something a bit more entertaining, The Guardian published this pretty comprehensive [article](https://www.theguardian.com/lifeandstyle/2015/feb/10/phthalates-plastics-chemicals-research-analysis) in 2015. 


[^1]: Since 1 mg/dL = 10,000 ng/mL, we divide phthalate concentration by 10,000 to get the units to match, then take phthalate/creatinine. Most values are in the $1e-6$ (micro) order of magnitude, so we express the final adjusted answer in micrograms phthalate per gram creatinine.


# References

