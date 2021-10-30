## Gender bias

<img align="right" src="gender.png" alt="gender" width="200" style="margin-top: 20px">

<em>R</em> code and data files associated with the following paper:

Bradshaw, CJA, F Courchamp. 2018. <a href="http://doi.org/10.3897/rethinkingecology.3.24333">Gender bias when assessing recommended ecology articles</a>. <em>Rethinking Ecology</em> 3: 1-12

## Abstract
Gender bias is still unfortunately rife in the sciences, and men co-author most articles (> 70%) in ecology. Whether ecologists subconsciously rate the quality of their peers’ work more favourably when they are the same gender (homophily) is still unclear. To test this hypothesis, we examined how ecologist editors ranked important ecology articles based on a previously compiled list where they had first each proposed some articles and then voted on all proposed articles. The proportion of female co-authors on the articles proposed by men were lower (0.06 to 0.09) than those proposed by women (0.13 to 0.27), although the data were highly skewed and most proposed articles (77%) had no female co-authors. For the 100 top-ranked articles voted by women or men only, the gender difference remained: female voters ranked articles in the top 100 that had more female co-authors (0.029 to 0.093 proportion women) than did those voted by men (0.001 to 0.029). Female voters tended to rank articles more highly as the number of male co-authors increased, and the relationship between article rank and proportion of male co-authors was even stronger when only men voted. This effect disappeared after testing only articles that editors declared they had actually read. This could indicate a persistent, subconscious tendency toward homophily when assessing the perceived quality of articles that ecologists have not actually read.

<br>
This repository includes the <em>R</em> code and data files needed to reproduce the analyses and results given in the aforementioned paper. Most is annotated such that an experienced user should be able to follow.

## Data
In addition to the data files available in the mother directory 'HIPE', You will also need to download the following data files to a dedicated directory and update the <code>setwd()</code> command in <em>R</em> accordingly:

- gender.csv
- proposer.gender.csv

## Required <em>R</em> libraries
- <code>igraph</code>
- <code>boot</code>
- <code>Hmisc</code>

<br>
Prof <a href="http://scholar.google.com.au/citations?sortby=pubdate&hl=en&user=1sO0O3wAAAAJ&view_op=list_works">Corey J. A. Bradshaw</a> <br>
<a href="http://globalecologyflinders.com" target="_blank">Global Ecology</a>, <a href="http://flinders.edu.au" target="_blank">Flinders University</a>, Adelaide, Australia <br>
17 September 2017 (updated 16 November 2017; updated 02 April 2018) <br>
<a href=mailto:corey.bradshaw@flinders.edu.au>e-mail</a> <br>
