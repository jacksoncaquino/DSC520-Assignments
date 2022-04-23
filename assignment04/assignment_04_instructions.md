# Assignment 4 - Instructions

For this assignment, you will create an R Markdown report. Use the `assignment_04_LastnameFirstname.Rmd` file as a template and add the required markup and code blocks. The following are useful references for creating R Markdown documents. 

* [Introduction to R Markdown](https://rmarkdown.rstudio.com/lesson-1.html)
* [The R Markdown Cheatsheet](https://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf)
* [The R Markdown Reference Guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)

## Tasks

### YAML Metadata

- [V] Update the YAML metadata in the template to include your name and the current date. 

### Markdown Basics

- [V] Under the `## Favorite Foods` section, add an ordered list of your top three favorite foods
- [V] Under the `## Images` section, insert the image `10-all-cases-log.png` from `completed/assignment04/plots/` directory with the caption `All Cases (Log Plot)`
- [V] Add one of your favorite quotes under the `## Add a Quote` section.
- [V] Add a LaTeX formatted equation under the `## Add an Equation` section.
- [V] Add any footnote under the `## Add a Footnote` section.
- [V] Under the `## Add Citations` section, create an unordered list with `R for Everyone` and `Discovering Statistics Using R` with the proper citations.  

### Inline Code

- [V] Under the `# Inline Code` section include a [code chunk](https://rmarkdown.rstudio.com/lesson-3.html) that loads the `ggplot2` library and following dataframes from the previous exercise: 
   - [V] `heights_df`
   - [V] `california_df`
   - [V] `florida_df`
   - [V] `ny_df` 
   - [V] Use the `include=FALSE` option
- [V] Under the `## NY Times COVID-19 Data` section, include code used to generate the log scale plot from the previous exercise.  
   - [V] This is the plot that corresponds to the image `10-all-cases-log.png` from `completed/assignment04/plots`.  
   - [V] Use the `echo=FALSE` option to only show the plot and not the code
- [V] Under the `## R4DS Height vs Earnings` section include the code to generate the plot corresponding to the image `completed/assignment03/plots/08-height-vs-earn-with-labels.png`
   - [V] Use the `echo=FALSE` option to only show the plot and not the code

### Tables

- [V] Using the `knitr` package and `kable` function, generate a table of the Lord of the Rings characters as given in the `characters_df` dataframe in the prior exercise.  
   - [V] Give the table the caption `One Ring to Rule Them All`
- [V] Under the `## Pandoc Grid Table` recreate the following table using [Pandoc Markdown](https://pandoc.org/MANUAL.html#tables)

| Name      | Race      | In Fellowship? | Is Ring Bearer? | Age    |
|-----------|-----------|----------------|-----------------|-------:|
| Aragon    | Men       | Yes            | No             | 88     |
| Bilbo        | Hobbit    | No            | Yes            | 129    |
| Frodo        | Hobbit   | Yes           | Yes            | 51     |
| Sam      | Hobbit   | Yes           | Yes            | 36     |
| Sauron    | Maia     | No            | Yes            | 7052   |

### Word and PDF Output

- [V] Generate Word and PDF output

RStudio should add the following to your YAML metadata

```yaml
output:
  pdf_document: default
  html_document: default
  word_document: default
```
