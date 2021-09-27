---
title: Contrasts in Multilevel/Mixed Models with an Interaction
author: Teresa
date: '2021-09-27'
slug: contrasts-in-multilevel-mixed-models-with-an-interaction
categories:
  - R
  - Statistics
tags:
  - R
  - rstats
  - statistics
  - mixed model
  - linear regression
  - random effects
  - regression
subtitle: ''
summary: ''
authors: []
lastmod: '2021-09-27T11:48:18+01:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

We take off from [where we left](https://tdbianco.netlify.app/post/contrasts-in-multilevel-mixed-models/), and add an interaction term to the mixed model, to run customised linear contrasts. As usual, we are using the `sleepstudy` dataset:

```{r, warning=FALSE, message=FALSE}
{r}
# install.packages("lmerTest")
# install.packages("knitr")
# install.packages("multcomp")
# install.packages("dplyr")
# install.packages("ggplot2")
data("sleepstudy")
# Load packages
packs <- c("lmerTest", "knitr", "multcomp", "dplyr", "ggplot2")
lapply(packs, require, character.only = TRUE) 
```

```{r}
kable(head(sleepstudy))
```

| Reaction | Days | Subject |
|---------:|-----:|:--------|
| 249.5600 |    0 | 308     |
| 258.7047 |    1 | 308     |
| 250.8006 |    2 | 308     |
| 321.4398 |    3 | 308     |
| 356.8519 |    4 | 308     |
| 414.6901 |    5 | 308     |

We are generating an additional factor (i.e., a categorical variable), `lunch`, with levels 0 or 1, recording whether the participants had lunch or not before the experiment:

```{r}
set.seed(88)
lunch <- sample(c(0,1), replace=TRUE, size=18)
sleepstudy$lunch <- factor(lunch)
kable(head(sleepstudy))
```

| Reaction | Days | Subject | lunch |
|---------:|-----:|:--------|:------|
| 249.5600 |    0 | 308     | 0     |
| 258.7047 |    1 | 308     | 0     |
| 250.8006 |    2 | 308     | 1     |
| 321.4398 |    3 | 308     | 0     |
| 356.8519 |    4 | 308     | 0     |
| 414.6901 |    5 | 308     | 1     |

At this point, we have 2 levels of variation, days of sleep deprivation and whether the participant had a full belly or not. We are going to investigate whether having lunch or not change the effect of `Days` on `Reaction`, by adding an interaction between the factors.

Before fitting the model, we are also changing the format of `Days` to factor:

```{r}
# Change the data type of Days from number (continous) to factor (categorical)
sleepstudy$Days <- factor(as.character(sleepstudy$Days), 
                          levels = unique(sleepstudy$Days))
levels(sleepstudy$Days)
# Fit the model
m <- lmer(Reaction ~ Days*lunch - 1 + ( 1 | Subject), data=sleepstudy)
kable(coef(summary(m)))
```

|              |   Estimate | Std. Error |        df |    t value | Pr(\>\|t\|) |
|:-------------|-----------:|-----------:|----------:|-----------:|------------:|
| Days0        | 255.124387 |   12.38442 |  47.19209 | 20.6004342 |   0.0000000 |
| Days1        | 256.156839 |   14.50641 |  76.83820 | 17.6581813 |   0.0000000 |
| Days2        | 274.505905 |   12.38474 |  47.19466 | 22.1648470 |   0.0000000 |
| Days3        | 286.583719 |   14.50640 |  76.83813 | 19.7556811 |   0.0000000 |
| Days4        | 290.838099 |   12.38309 |  47.17917 | 23.4867067 |   0.0000000 |
| Days5        | 298.808860 |   14.50365 |  76.81565 | 20.6023274 |   0.0000000 |
| Days6        | 305.549582 |   12.38171 |  47.16590 | 24.6774926 |   0.0000000 |
| Days7        | 316.437061 |   14.51779 |  76.94158 | 21.7965064 |   0.0000000 |
| Days8        | 343.417781 |   12.38155 |  47.16463 | 27.7362418 |   0.0000000 |
| Days9        | 359.819590 |   14.51723 |  76.93791 | 24.7856947 |   0.0000000 |
| lunch1       |   6.873382 |   18.50998 | 145.24446 |  0.3713339 |   0.7109299 |
| Days1:lunch1 |   8.136668 |   24.14985 | 145.16727 |  0.3369242 |   0.7366607 |
| Days2:lunch1 | -48.021403 |   26.62930 | 146.00451 | -1.8033297 |   0.0733985 |
| Days3:lunch1 | -13.338456 |   24.22097 | 145.41194 | -0.5506986 |   0.5826854 |
| Days4:lunch1 | -16.722425 |   25.65177 | 144.37249 | -0.6519013 |   0.5155013 |
| Days5:lunch1 |  10.603891 |   24.86291 | 146.34662 |  0.4264943 |   0.6703744 |
| Days6:lunch1 |  22.955649 |   26.55690 | 145.78060 |  0.8643949 |   0.3887911 |
| Days7:lunch1 |  -2.709042 |   23.49512 | 144.08581 | -0.1153023 |   0.9083660 |
| Days8:lunch1 | -37.420620 |   26.49472 | 145.59599 | -1.4123802 |   0.1599719 |
| Days9:lunch1 | -23.016444 |   24.27124 | 145.58290 | -0.9483013 |   0.3445476 |

The above output shows the main effects of `Days` and lunch and their interactions. We should interpret the main effect of `Days` without lunch (lunch = 0). In addition to the main effect of `lunch`, each day will receive an additional coefficient given by the interaction.

The output shows that the main effect of lunch and the interactions are not significant. But we are interested in the differences between all the days and the first day of the experiment, Day 1, so we want to compare all the other days to Day 1. We need again to build a contrasts matrix.

# Building the Contrasts Matrix

First, we need to compute all the combinations of the levels of `Days` and `lunch`:

```{r}
group <- paste0(sleepstudy$Days, sleepstudy$lunch)
group <- aggregate(model.matrix(m) ~ group, FUN=mean)
# group
rownames(group) <- group$group
(group <- group[,-1])
kable(head(group)) #displaying only the first 6 rows
```

|     | Days0 | Days1 | Days2 | Days3 | Days4 | Days5 | Days6 | Days7 | Days8 | Days9 | lunch1 | Days1:lunch1 | Days2:lunch1 | Days3:lunch1 | Days4:lunch1 | Days5:lunch1 | Days6:lunch1 | Days7:lunch1 | Days8:lunch1 | Days9:lunch1 |
|:----|------:|------:|------:|------:|------:|------:|------:|------:|------:|------:|-------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|
| 00  |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 01  |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      1 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 10  |     0 |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 11  |     0 |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      1 |            1 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 20  |     0 |     0 |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 21  |     0 |     0 |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      1 |            0 |            1 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |

The first column specifies the interaction between `Days` and `lunch` for each row: we have Day 0 with lunch 0 and 1 (`00` and `01`), day 1 with lunch 0 and 1 (`10` and `11`), and so on. The other columns contains zeros and ones specifying the factors combination (1s mark the active level - `Days` 0 and `lunch` 1, on row 2) in the extended model matrix.

Now we can specify which comparisons we want to compute (all days vs Day 1):

```{r}
contrasts <- rbind(group["11",] - group["01",],
                   group["11",] - group["21",],
                   group["11",] - group["31",],
                   group["11",] - group["41",],
                   group["11",] - group["51",],
                   group["11",] - group["61",],
                   group["11",] - group["71",],
                   group["11",] - group["81",],
                   group["11",] - group["91",])
kable(head(contrasts))
```

|     | Days0 | Days1 | Days2 | Days3 | Days4 | Days5 | Days6 | Days7 | Days8 | Days9 | lunch1 | Days1:lunch1 | Days2:lunch1 | Days3:lunch1 | Days4:lunch1 | Days5:lunch1 | Days6:lunch1 | Days7:lunch1 | Days8:lunch1 | Days9:lunch1 |
|:----|------:|------:|------:|------:|------:|------:|------:|------:|------:|------:|-------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|
| 11  |    -1 |     1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      0 |            1 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 111 |     0 |     1 |    -1 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |      0 |            1 |           -1 |            0 |            0 |            0 |            0 |            0 |            0 |            0 |
| 112 |     0 |     1 |     0 |    -1 |     0 |     0 |     0 |     0 |     0 |     0 |      0 |            1 |            0 |           -1 |            0 |            0 |            0 |            0 |            0 |            0 |
| 113 |     0 |     1 |     0 |     0 |    -1 |     0 |     0 |     0 |     0 |     0 |      0 |            1 |            0 |            0 |           -1 |            0 |            0 |            0 |            0 |            0 |
| 114 |     0 |     1 |     0 |     0 |     0 |    -1 |     0 |     0 |     0 |     0 |      0 |            1 |            0 |            0 |            0 |           -1 |            0 |            0 |            0 |            0 |
| 115 |     0 |     1 |     0 |     0 |     0 |     0 |    -1 |     0 |     0 |     0 |      0 |            1 |            0 |            0 |            0 |            0 |           -1 |            0 |            0 |            0 |

Now the matrix specifies couples of contrasts that we want to estimate from the model.

# Extracting the contrasts from the model

For extracting the contrasts from the model, we are going to use the `glht` function from the `multcomp` package.

```{r}
# Transform into Matrix
contrast.matrix <- rbind("Lunch Day 0 vs Day 1"=as.numeric(contrasts[1,]),
                         "Lunch Day 2 vs Day 1"=as.numeric(contrasts[3,]),
                         "Lunch Day 3 vs Day 1"=as.numeric(contrasts[4,]),
                         "Lunch Day 4 vs Day 1"=as.numeric(contrasts[5,]),
                         "Lunch Day 5 vs Day 1"=as.numeric(contrasts[6,]),
                         "Lunch Day 6 vs Day 1"=as.numeric(contrasts[7,]),
                         "Lunch Day 7 vs Day 1"=as.numeric(contrasts[8,]),
                         "Lunch Day 8 vs Day 1"=as.numeric(contrasts[8,]),
                         "Lunch Day 9 vs Day 1"=as.numeric(contrasts[8,])
                         )

comparisons <- summary(glht(m, contrast.matrix))

comparisons
```

![](images/Screenshot%202021-09-27%20at%2012.30.03.png)

From this output, we can extract that the effect of Lunch is critical on Days 4-6, compared to Day 1.

Let's format the results of the contrasts test into a table:

```{r}
pq <- comparisons$test

mtests <- data.frame(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(comparisons$alternativ, 
                less = paste("Pr(<", 
                             ifelse(comparisons$df ==0, "z", "t"), ")", 
                             sep = ""), 
                 greater = paste("Pr(>", 
                                 ifelse(comparisons$df == 0, "z", "t"), ")", 
                                 sep = ""), 
                 two.sided = paste("Pr(>|", 
                                   ifelse(comparisons$df == 0, "z", "t"), "|)"
                                   , sep = ""))                                   
colnames(mtests) <- c("Estimate", "Std.Error", 
                      ifelse(comparisons$df ==0, "zvalue", "tvalue"), pname)

mtests <- mtests %>%
  tibble::rownames_to_column("Comparison") %>%
  mutate(`Pr(>|z|)`=ifelse(`Pr(>|z|)`< 0.001, 
                           "< 0.001", 
                           ifelse(`Pr(>|z|)` < 0.01, 
                                  "< 0.01",
                                  ifelse(`Pr(>|z|)` < 0.05, 
                                         "< 0.05",
                                         paste(round(`Pr(>|z|)`,
                                                     4)))))) %>%
  mutate_if(is.numeric, funs(round(.,
                                   digits=2)))
kable(mtests)
```

| Comparison           | Estimate | Std.Error | zvalue | Pr(\>\|z\|) |
|:---------------------|---------:|----------:|-------:|:------------|
| Lunch Day 0 vs Day 1 |     9.17 |     19.00 |   0.48 | 0.998       |
| Lunch Day 2 vs Day 1 |    -8.95 |     14.31 |  -0.63 | 0.9906      |
| Lunch Day 3 vs Day 1 |    -9.82 |     19.01 |  -0.52 | 0.997       |
| Lunch Day 4 vs Day 1 |   -45.12 |     14.29 |  -3.16 | \< 0.05     |
| Lunch Day 5 vs Day 1 |   -64.21 |     18.71 |  -3.43 | \< 0.01     |
| Lunch Day 6 vs Day 1 |   -49.43 |     13.97 |  -3.54 | \< 0.01     |
| Lunch Day 7 vs Day 1 |   -41.70 |     19.02 |  -2.19 | 0.1578      |
| Lunch Day 8 vs Day 1 |   -41.70 |     19.02 |  -2.19 | 0.1578      |
| Lunch Day 9 vs Day 1 |   -41.70 |     19.02 |  -2.19 | 0.1578      |

This is one way of investigating interactions with mixed models. Remember that, the more comparisons one does, the more there is the chance of incurring in Type I error, and there is [the need of correcting the p-values](http://daniellakens.blogspot.com/2016/02/why-you-dont-need-to-adjust-you-alpha.html) (that the function `glht` does automatically).
