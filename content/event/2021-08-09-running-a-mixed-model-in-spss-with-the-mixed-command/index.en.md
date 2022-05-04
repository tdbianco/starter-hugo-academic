---
title: Running a Mixed Model in SPSS  with the mixed Command
author: Teresa Del Bianco
date: '2021-08-09'
slug: running-a-mixed-model-in-spss-with-the-mixed-command
categories:
  - Talk
tags:
  - linear regression
  - mixed model
  - random effects
  - regression
  - statistics
  - SPSS
subtitle: ''
summary: ''
authors: []
lastmod: '2022-05-04T11:53:41+01:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

{{< youtube TCT6cVF7Ueg >}}

<br>

As part of this series on Mixed Models, [Mary Agyapong](https://twitter.com/_MaryAgyapong) shared her analytic pipeline to run a mixed model in SPSS using the mixed command.

Our motivation to run this demo was a common misconception of the definition of mixed/multilevel models as a repeated-measures regression only, originating from the fact that SPSS mixed command can be run without specifying the random effect (such model would be a Generalised Estimating Equation (GEE) model, and not a mixed model).

While GEE is a valid method, it will not output the same numerical results of a mixed model run in r with lmer. Therefore, in this presentation, Mary illustrated the steps to specify the random effect in SPSS to obtain the same output in R and SPSS.

As an example, Mary used the `sleepstudy` dataset, the same used in the [Essential Demo of a Multilevel/Mixed Model](https://tdbianco.netlify.app/post/essential-demo-of-a-multilevel-mixed-model) in R.

To switch on auto-generated subtitles, use the 5th icon from the right bottom corner of the player!

For Mary's slides and further information see:

{{% staticref "uploads/Running-A-Mixed-Model-in-SPSS-with-mixed.pdf" "newtab" %}}Slides{{% /staticref %}}

Watch the introductory talk of this series: [The Hitchhiker's Guide to Mixed Models](https://tdbianco.netlify.app/post/the-hitchhiker-s-guide-to-mixed-models/)
