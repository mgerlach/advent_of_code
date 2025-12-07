# advent_of_code

## What is this?

[Advent of Code](https://adventofcode.com/) notebooks/worksheets, partly with input files.

Python notebooks created with [Google Colaboratory](https://colab.research.google.com/).

Scala worksheets created with [Intellij IDEA](https://www.jetbrains.com/idea/) with Scala plugin.

## How to use?

### 2020

(Days 6 to 24 only) - Input files are referred to as `input.txt` for all days. 
So either the input file for each day of AoC [2020/](2020/) needs to be uploaded to Colab runtime root dir as `input.txt` or you upload all files and change the notebook code to read `input<day>.txt` rather than just `input.txt`. 
You may also upload all files to Google Drive and mount Google Drive as described for 2021 and 2024 below (also need to change the code to read from the Google Drive location.)

### 2021, 2024

Input files for AoC [2021/](2021/) and [2024/](2024/) are expected in your Google Drive in `My Drive/AoC/2021/` and `My Drive/AoC/2024/`. 
Colab must be given permission to mount your Google Drive.

### 2022

Used [Scala worksheets](2022/src/main/scala) in IDEA instead of Python notebooks. One per day.
Inputs can be found in [2022/src/main/resources](2022/src/main/resources).

### 2024

See 2021/2024 above.

### 2025

Input files not downloaded, the notebook [2025/aoc2025.ipnb](2025/aoc2025.ipynb) contains code to download the input data directly from AoC website using your session cookie.
There is also a utility to find all test input data candidates from the problem description page.
You can run it once to display all candidates, then run it again with the desired candidate index.
