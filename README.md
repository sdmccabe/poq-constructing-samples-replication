# Replication archive for Hughes et al. (2021)

## Overview

This repository contains replication materials for the paper “Using
Administrative Records and Survey Data to Construct Samples of Tweeters and
Tweets.”.

This project involved two research groups (Pew and Northeastern) analyzing
separate data sets (the samples described in the paper). Because of the terms of
our data-sharing agreements, motivated by a desire to protect user privacy as
much as possible, these samples were analyzed separately. For this reason, much
of the analysis is split across two folders, `voter_file_panel` and
`pew_panels`.

The code for constructing the voter file sample was originally developed for
[Grinberg et al. (2019)](https://science.sciencemag.org/content/363/6425/374);
that code can be found [here](https://github.com/kennyjoseph/twitter_matching).

## `pew_panels`

Overview of contents:

* `internet_phone_poll.R`: analysis of RDD phone poll (Figure 3)
* `poq_replication_survey.R`: analysis of KP and ATP samples (all figures and
  Table 1)
* `poq_tweetextanalysis.R`: analysis of tweet text for KP/ATP (Table 2)

## `voter_file_panel`

Overview of contents:

* `definitions.py`: Utility file, describing file schemas.
* `extract_demographics.py`: Pre-processing script for constructing sample
  demographics
* `demographics.ipynb`: Analysis notebook, using output of
  `extract_demographics.py` (Figures 1 and 2)
* `behavior.py`: Pre-processing script for constructing sample Tweet metadata
* `behavior.ipynb`: Analysis notebook, using output of `behavior.py` (Figure 4)
* `summarize_for_poq_neu_pew.R`: Script for combining histogram data (Table 1, Figure 4)
* `histograms.sh`: Script for computing voter-file histograms (Table 1)
* `hash/`: Construction of sample intersections (Appendix A5)
* `hash_agreement.ipynb`: Analysis of sample intersections (Appendix A5)
