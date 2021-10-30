# HIPE (Highly Important Papers in Ecology)

<em>R</em> code and data files accompanying the paper:

<blockquote><a href="https://www.ese.universite-paris-saclay.fr/en/team-members/franck-courchamp/">Courchamp, F</a>, <a href="http://scholar.google.com.au/citations?sortby=pubdate&hl=en&user=1sO0O3wAAAAJ&view_op=list_works">CJA Bradshaw</a>. 2017. <a href="https://doi.org/10.1038/s41559-017-0370-9">100 papers every ecologist should read</a>. <em>Nature Ecology and Evolution</em> 2: 395-401</blockquote>

## Abstract
Reading scientific articles is a valuable and major part of the activity of scientists. Yet, with the upsurge of currently available articles and the increasing specialization of scientists, it becomes difficult to identify, let alone read, important papers covering topics not directly related to one’s own specific field of research, or that are older than a few years. Our objective was to propose a list of seminal papers deemed to be of major importance in ecology, thus providing a general ‘must-read’ list for any new ecologist, regardless of particular topic or expertise. We generated a list of 544 papers proposed by 147 ecology experts (journal editorial members) and subsequently ranked via random-sample voting by 368 of 665 contacted ecology experts, covering 6 article types, 6 approaches and 17 fields. Most of the recommended papers were not published in the highest-ranking journals, nor did they have the highest number of mean annual citations. The articles proposed through the collective recommendation of several hundred experienced researchers probably do not represent an ‘ultimate’, invariant list, but they certainly contain many high-quality articles that are undoubtedly worth reading—regardless of the specific field of interest in ecology—to foster the understanding, knowledge and inspiration of early-career scientists.

<br>
This repository includes the R code and <a href="https://github.com/cjabradshaw/HIPE/tree/master/data">data files</a> needed to reproduce the analyses and results given in the aforementioned
paper. Most is annotated such that an experienced user should be able to follow.

## Data
You will also need to download the following data files to a dedicated directory and update the <code>setwd()</code> command in <em>R</em> accordingly:

- HIPE.refs.txt
- citation.csv
- VoteArticles.final.csv
- type.csv
- field.csv
- approach.csv
- nproposed.csv

## Required <em>R</em> libraries
- <code>igraph</code>
- <code>boot</code>
- <code>Hmisc</code>

## Sister paper
Note that the <a href="https://github.com/cjabradshaw/HIPE/tree/master/gender">gender</a> folder contains additional <em>R</em> code and data associated with the sister paper:

Bradshaw, CJA, F Courchamp. 2018. <a href="http://doi.org/10.3897/rethinkingecology.3.24333">Gender bias when assessing recommended ecology articles</a>. <em>Rethinking Ecology</em> 3: 1-12


<br>
Prof <a href="http://scholar.google.com.au/citations?sortby=pubdate&hl=en&user=1sO0O3wAAAAJ&view_op=list_works">Corey J. A. Bradshaw</a> <br>
<a href="http://globalecologyflinders.com" target="_blank">Global Ecology</a>, <a href="http://flinders.edu.au" target="_blank">Flinders University</a>, Adelaide, Australia <br>
August 2017 <br>
<a href=mailto:corey.bradshaw@flinders.edu.au>e-mail</a> <br>

