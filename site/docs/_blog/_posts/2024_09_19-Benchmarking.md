---
title: Benchmarking
---

# Benchmarking

This project, was my first foray into something that is performance sensitive. We will be using JMH (for the JVM) and mills JMH plugin. I investigated this action for CI;

https://github.com/benchmark-action/github-action-benchmark

But ended unconvinced that it was the right thing. It uses the older style github pages from branch deployment. I also didn't understand what happened to it's outputs, and how I could hook in myself outside of the plot itself. There was a bewildering array of knobs and levers which I failed to tweak to my usecase.

From first principles, I thought about 3 problems;
- How should a benchmark be triggered? 
- Where shoudl the data be stored
- How to consume

## Trigger

I concluded I wanted a manual trigger. Each push to main is too much - a lot of redundant data and duplicated work on each commit. In future perhaps switch to benchmark on  release. As the suite is being built out, I think manual makes more sense. 

## Where shoudl the data be stored

JMH itself doesn't give you more than the results. We need extra metadata. ChatGPT wrote me a shell script which appends the date, commt and branch that the results were generated from. 

This file is then pushed to an orphaned  [branch](https://github.com/Quafadas/vecxt/tree/benchmark). The benchmarking process ends here - it only stores the data, it's then left to the consumer to figure out what to do with it. 

## Consumption




