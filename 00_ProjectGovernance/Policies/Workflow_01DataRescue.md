# Data Rescue Workflow
Katherine Todd-Brown
2026-October

- [Understanding data source](#sec-UndertandingDataSource)
  - [Identify, find, and read data
    source](#identify-find-and-read-data-source)
  - [Summarize and propose plan](#summarize-and-propose-plan)
  - [Expert review](#expert-review)
- [Transcribing data](#sec-TranscribingData)
  - [Prep documents](#prep-documents)
  - [Transcribe](#transcribe)
  - [Cross check](#cross-check)
- [Reviewing transcripts](#sec-ReviewingTranscripts)
  - [Prep the data rescue](#prep-the-data-rescue)
  - [Pull Request](#pull-request)
  - [PR Review](#pr-review)
  - [Requested changes](#requested-changes)
  - [Accepting the PR](#accepting-the-pr)

This workflow provides guidance for moving data out of a pdf manuscript,
report, or other non-machine readable format into a machine readable
format. The data rescue is the first stage in the
rescue-harmonize-curate process that elevates data from a non-machine
readable format (rescue) to inter-operable data collection (harmonize)
to a analysis-ready data product (curate). This workflow document is
meant to supplement the data rescue template
`01_DataRescue/_TemplateYYYY/README_AuthorYYYY.qmd`.

Data rescue is the transformation of information from a non-machine
readable format to a machine readable one. This can include scanning
paper copies of reports or records, transcribing scanned records into
flat text documents, or extracting x-y values from a scanned graph.
Generally this stage should be as close to the original information as
practical. Information here may be primary or meta data. This stage must
maintain provenance of the data including links to the original source,
how the data was made machine readable, what data was not rescued, and
who did this work.

| Data Rescue | Lead | Do | Measure |
|----|----|----|----|
| Input: Understand data source <a href="#sec-UndertandingDataSource" class="quarto-xref">Section 1</a> | Identify, find, and read the paper | Summarize the data in paper and identify relevant components | *Expert Review*: Are the relevant data elements identified? |
| Transformation: Transcribe the data <a href="#sec-TranscribingData" class="quarto-xref">Section 2</a> | Prep the spreadsheet and figure capture software | Transcribe data from text, tables, and figures | Cross check - Compare and reconcile data from second transcriber |
| Output: Push rescue to github <a href="#sec-ReviewingTranscripts" class="quarto-xref">Section 3</a> | Prep transcription package and render ReadMe | Pull request to incorporate the data rescue into the repository | *Expert Review*: Does the pull request have the documentation identified below? Are there obvious errors that would cause the code not to render? |

This process should result in the following documentation:

- A ReadMe file with (qmd-format)
  - human readable summary of data source with context for data rescue
  - all contributors identified in the metadata and their time log
  - code chunks that read in the transcribed data and displays it in a
    way that allows comparison with the original document
  - code that cross checks at least two transcriptions and displays any
    results
- Data elements that have been reconciled from two independent
  transcriptions. Data elements may include,
  - tables (csv-format)
  - x-y extracted data points from figures (csv-format)
  - methods (md-format) with paired BibTex formatted reference
    (bib-format)
- Two bibTex formatted file with reference to
  - the original the data source(s)
  - citations from the data elements transcription

## Understanding data source

### Identify, find, and read data source

Data sources can be identified in a number of different ways but will
generally be nominated by an expert in the field in connection for a
specific desired re-analysis. Once a set of papers is identified a
curation file will be created that will generally hold a list or table
of files to be rescued (for example:
https://github.com/ktoddbrown/SoilDRaH/blob/main/03_DataCuration/HiCSC/HiCSC2025.qmd)
Most people start their data rescue working from the identified sources
in the data curation file.

Once the data source as been selected create a github issue using the
Data Rescue template that identifies the citation, working branch (this
can be updated later) and any people you expect to be working on this
with you (also something that can be created later). This will allow
other people to see that the data source is being worked on and prevent
duplicative efforts.

Finally the data source must be read in it’s entirety to understand the
context for that data. Generally there will be a specific re-analysis
identified and understanding should be targeted towards this purpose.
Reading the data source can include reviewing manuscripts from the
primary literature and their supplemental information, or published
agency reports. Specific attention should be paid to the methods
section, and any tables or figures.

### Summarize and propose plan

At this point you are ready to propose a plan to rescue specific data
elements of your source. Copy the
`https://github.com/ktoddbrown/SoilDRaH/tree/main/01_DataRescue/_TemplateYYYY`
and rename this new folder by the last name of the first author followed
by year of publication. Follow the template and add your name and
affiliation in the YAML header as well as any known team members. Then
describe the data source you are working with and the curation that this
is associated with including an overview of all the data in the source
and highlight specific sections or components for fit-for-purpose
rescue. You should identify key information in the methods section,
individual tables and figures under the `Fit for purpose` section as
well as list the figures and tables you plan to rescue in the
`Data rescue plan` list. Push this new folder with the modified readme
file to a new branch on your fork.

### Expert review

Pass the summary and proposed plan to a reviewer by tagging them in your
original issue, be sure to update the fork and branch link to reflect
where your readme with the data rescue plan is located (for example:
https://github.com/ktoddbrown/SoilDRaH/issues/304). This reviewer should
have some degree of familiarity of the methodologies used in the study
and the proposed reanalysis. The reviewer should:

1)  Pull source paper and skim
2)  Check for ‘extra’ data identified in plan
3)  Check that the location of target data in the source document is
    included in the rescue plan and correct
4)  Skim for additional data that got missed, often looking for
    components (e.g. bulk density, organic fraction, coarse fraction …)
5)  Access for unusual rescue methods that require follow up
    guidance/training - high number of points in figures (\>50,
    requiring OCR code or more people) - very large tables (\>2 pages,
    PDF table read code or more people) - information in archived data
    files (download rather then transcribe) - information in proprietary
    format (export rather then transcribe)

Finally comment on the issue with guidance on any need modifications.
For example, can a table be dropped or does a figure need to be added to
the data rescue.

## Transcribing data

| Lead | Do | Measure |
|----|----|----|
| Prep the spreadsheets and any figure capture software | Transcribe data from text, tables, and figures | Compare and reconcile data from second transcriber |

Transcribing data is probably the first thing that comes to mind with
data rescue. Here we are looking to bring data from a PDF or other
format into a more structured machine readable format. Yes, technically
a PDF is machine readable in many cases however there are several issues
with PDFs that make this a non-ideal format. In the end it’s often more
work to try to automate this portion then it is to go in and transcribe
the data via manual entry, edge cases where this is not the case should
be caught by your expert review in the prior step.

Issues around optical character recognition, special characters, common
formatting (like supper/sub scripts), header/footer information, table
structures, and figure images; all contribute to a need for a highly
manual transcription of PDF documents. Older PDFs often rely on
character recognition to translate the text image into a
[ASCII](https://en.wikipedia.org/wiki/ASCII) like text character. More
modern PDFs will often have special characters or formatting which may
or may not be correctly read by scripts. Information like page number or
headers/footers will also be read in sequentially with the primary text.
Tables will often have unclear column splits (as far as a text reader is
concerned) and sub-tables may require additional considerations.
Finally, figures like graphs are completely cryptic to text readers.

### Prep documents

Generally, you will have three types of data capture files: a
[Markdown](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
for the methods section, a spreadsheet for the table and figure
captures, and software for the figure x-y captures. RStudio is a pretty
good markdown editor, although you will need to ensure that the YAML
header is removed. There are other markdown editors out there and you
are encouraged to explore your options here. A spreadsheet software that
allows you to enter tablular data (for example Excel or Numbers) is
recommended rather then directly transcribing into a text-based csv
file. Finally, figure x-y capture software like
[PlotDigitizer](https://plotdigitizer.com/app) will capture mouse click
locations allowing you to translate figures into a set of coordinates.
If you have never worked with these files or software you may wish to
look up online tutorials and familarize yourself with them before moving
on.

#### ReadMe

This is a good step to create the ReadMe file from the
01_DataRescue/\_TemplateYYYY/README_AuthorYYYY.qmd template file. Fork
this repository (see
https://docs.github.com/en/pull-requests/how-tos/work-with-forks/fork-a-repo).
Clone your forked repository to your computer (see
https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).
Then create a branch for this data rescue on the forked repository (see
https://docs.github.com/en/pull-requests/how-tos/commit-changes/managing-branches-within-your-repository).
Finally copy the 01_DataRescue/\_TemplateYYYY to a new folder and rename
it the study name (generally last name of the first author followed by
publication year).

Set up the new ReadMe file by changing the citations and updating the
documentation in the README_AuthorYYYY.qmd. You’ll want to review the
template file carefully, there are useful hits in the
`<!--- comments --->`. Rename the files to reflect your study name.
Update the activity log to reflect the work you’ve done so far! Make
your first `git commit` at this point so you can start tracking changes
to share with your team.

### Transcribe

Finally there! This should be where the bulk of your work happens, the
data transcription. At this stage you are pull in the data elements
(methods, tables, and figures) to populate the template. At the end you
should then update the template to read in and reproduce all of the data
elements.

#### Methods

Method sections are critical reference in the curation phase when
deciding what measurements are equivalent for a given purpose and
important for future data discovery. Thus the entire methods section
should be transcribed, regardless of the specific data needs of the
purpose. In addition, analysis of the cited articles in the methods
section can provide key incites when developing vocabularies and
ontologies.

With these two motivations in mind, here are some general practices to
follow when creating your methods section:

1)  Copy and paste the text. This will miss the formatting but, in many
    cases, reduce typos.
2)  Separate paragraphs and headers with an empty line. This will
    increase the structure of the text file and make it easier to parse
    in the read function.
3)  Capture the formatting of the text using
    [Markdown](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
    with special attention to sections and
    [mathematics](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/writing-mathematical-expressions).
4)  Check your characters. There are common optical recognition errors
    (0O and l1 for example) that happen when you copy/paste. Do a read
    through and correct any errors here.
5)  Pull the references in the methods section. This includes generating
    a new [BibTex](https://www.bibtex.org/) file that is separate from
    the reference for the source. Right now we are putting the reference
    to the bib entry at the end of the line where it is references with
    a `@AuthorYear` tag that corresponds to the bib label.

Bibliography files should be downloaded from the referenced journal
article page. This is generally at the top of the webpage under the
title and authors. You will select the `bibtex` option here and often
download a `.bib` file. Open this file in a text editor (like RStudio)
and copy/paste the contents into the `AuthorYYYY_Methods.bib` (for
method references), or `AuthorYYYY.bib` for the primary reference of the
study. If there is no journal download, try to create the bibentry based
on <https://www.bibtex.com/format/#templates>. Note that the automatic
reference generators have a hard time getting the more obscure
references correct. Use them with caution.

When in doubt, try something. Then bring it to a [SoilDRaH
meeting](https://github.com/ktoddbrown/SoilDRaH/discussions) or make a
common on the issue associated with the data rescue.

#### Tables

Tables in manuscripts are often formatted in such a way that they read
well to humans but not to machines. The challenge with rescuing tables
is to balance preservation of the original data model, which has
information in it’s structure about how the original authors saw their
system, with ensuring that it is machine readable. This balance is
subjective and may require discussion between both data transcribers and
the reviewer to determine what is best for the particular paper. When in
doubt, try something.

With this in mind, here are some general practices:

1)  Use a spreadsheet software for the data entry then export it later
    to a csv-file that you can archive. Spreadsheets are generally much
    easier to work with columns in then flat text files, but old
    spreadsheet files are not always readable by modern software, which
    is why the data should not *stay* in a spreadsheet format.
2)  Set your spreadsheet to text entries. Spreadsheets try to be clever
    and guess what type of data you are entering. Numbers are often
    truncated if they end in a zero or can end up ‘padded’ due to
    numerical imprecision (0.1 ends up as 0.1000001), both loose
    information in the significant digits about how precise the number
    is entered. Treat everything as a text when you are transcribing.
3)  Use the first row to copy the caption. This will ensure that the
    table and it’s caption are always together. The compromise here is
    that you have to skip the first row when reading the final csv, no
    ideal but also not difficult.
    1)  An alternative that was also considered here was commenting out
        the row using `#` instead, however the comment character is not
        standardized so we decided to go with the first row convention
        instead.
4)  Sub-table headers may new columns and removing the sub-header rows.
    While spreadsheets will merge cells for you, it does not export
    nicely to csv tables. This may require discussion with other people
    who have done data rescues before if you are new. An example of this
    is in Table 1 of Burke2003.

As a final check you should recreate the table in the template and
compare it to the table in the manuscript. There is sample code for you
to work off of in the readme template.

#### Figures

Figures in manuscripts are designed to community complex data visually
in such a way that trends supporting research conclusions are evident.
This is broadly not machine readable. In addition to the x-y location on
the graph, figure shapes, colors, line types, and error bars all
contains information that needs to be captured.

1)  Take a screenshot of the figure and upload it to a click capture
    tool. We have been using the online version of
    https://plotdigitizer.com/ as of September 2026, but you can also do
    this in Powerpoint.
2)  Record points. You will generally need to define the axis bounds and
    then can click on points on the image and the tool will record the
    x-y location of those clicks in a spread sheet.
3)  Label the x and y columns with the axis label OR create a x-label
    column and y-label column if you are dealing with multiple
    sub-panels with different axises.
4)  Add the caption to the first line, similar to a table, and export to
    a csv file.

A word on precision. The software will give you a very high precision on
your mouse click. In general you should look at the axis label and
record 1 to 2 significant digits more then is represented on the labels.
If two points are mostly overlapping, it is okay to give them the same
number.

In some cases it is not possible to capture the data in a figure to a
useful degree. Highly dense, overlapping points where you don’t know the
number of points being obscured or ‘3D’ figures are two examples of
figures that may not be able to be digitized. You should discuss this
with the data curator for your project if this appears to be the case.

Finally you should create a plot that duplicates the one in the paper to
make sure you have the figure transcribed correctly. Figure creation can
be complicated. Project standard libraries are declared in the template
and you should not include any new libraries without discussion with the
maintainers. There is a basic example in the template of a figure
creation but you may need to ask for help if you have not worked with
the R package `ggplot2` before.

### Cross check

Now is the time to ask for a secondary transcription to cross check your
work. This second transcription generally lives in the `temp` folder of
the study folder. Code in the template uses `R` to check the line by
line character matches and outputs lines that do not have matching
characters. The character matches are very sensitive and will flag as
different \* space vs tab \* newlines on different operating machines
(sometimes) \* special characters on different operating machines
(sometimes) \* upper vs lower characters.

Both the primary and secondary transcribers should meet and discuss the
line by line differences and arrive on something that is ‘good enough’.

## Reviewing transcripts

| Lead | Do | Measure |
|----|----|----|
| Prep transcription package | Create a pull request to incorporate the data rescue into the repository | **Expert Review** Does the pull request have the documentation identified below? Are there obvious errors that would cause the code not to render? |

Almost there!

### Prep the data rescue

First you should review your entire data rescue. **Render** the README
file and make sure that everything runs. Common things to check are: -
Do the figures and tables match what is in the primary literature? - Do
you have everyone logged as authors who needs to be? - Did you record
the time that you spent on this data rescue?

You should have need to include the following files:

- A README file with (qmd)
- A data transcription(s) include methods section and secondary
  transcriptions in the `temp` folder (csv, md)
- BibTex files including methods and primary citation (bib)

### Pull Request

Make a Pull Request on the main repository. Commit and push your files
to your github fork. Here is a good reference to start with:
https://docs.github.com/en/pull-requests/how-tos/create-pull-requests/creating-a-pull-request
but feel free to ask if this is your first pull request! Be sure to
mention your orignal issue in the pull request message.

### PR Review

Tag yourself as the reviewer for the pull request. Unless you are very
enthusiastic, you probably will not be executing the quarto document;
level 1 harmonization will check the content and fix runtime errors
before it can proceed. Rather you are looking for broad inconsistencies
and common mistakes.

Reviewer check list:

- README_AUTHORYYYY.qmd
  - [ ] all contributors identified in the metadata and yml format looks
    good
  - [ ] human readable summary of data source with context for data
    rescue with citation
  - [ ] A BibTex file with a single citation for the primary paper
    should be included as well using a *AuthorYear* bib key format with
    the first authors last name and year of publication.
  - [ ] chunk labels are all different
  - [ ] code looks like it would render
- AUTHORYYYY\_\*.csv
  - [ ] first line contains the caption and any footnotes
  - [ ] transcriptions of tables or x-y extracted data points appear to
    have reasonable precision
  - [ ] second transcription of methods and csv files are in the temp
    folder
- AUTHORYYYY_Methods.csv
  - [ ] methods section headers should be well formatted using `#` or
    `##` as appropriate with an empty line above and below.
  - [ ] mathematics in the text should be well formatted LaTex using
    `$ ... $`
- BibTex formatted references
  - [ ] citations should be linked to a BibTex file using a *AuthorYear*
    bib key format with the first authors last name and year of
    publication.
  - [ ] all lines within the citation end in a comma
  - [ ] bibtex appears to be well formatted

Add line by line (or file level) suggestions to address any changes that
need to be made and use githubs PR review process
https://docs.github.com/en/pull-requests/reference/pull-request-reviews

### Requested changes

Most of the time there are changes that need to be made. Don’t worry!
There should be line by line changes for you to respond to on the pull
request. Make the changes to your fork/branch (add-commit-push), and
they will automatically update on the pull request. Respond to the any
comments or suggested changes and ask for a re-review from your
reviewer. Repeat as needed.

### Accepting the PR

Once the data rescue is good enough, squash and merge.

Celebrate!
