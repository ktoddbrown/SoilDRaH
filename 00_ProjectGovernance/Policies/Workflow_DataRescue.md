# Data Rescue Workflow
Katherine Todd-Brown
2026-January

- [Understanding data source](#sec-UndertandingDataSource)
  - [Identify, find, and read data
    source](#identify-find-and-read-data-source)
  - [Summarize and propose plan](#summarize-and-propose-plan)
  - [Expert review](#expert-review)
- [Transcribing data](#sec-TranscribingData)
  - [Prep documents](#prep-documents)
- [Reviewing transcripts](#sec-ReviewingTranscripts)

This workflow provides guidance for moving data out of a pdf manuscript,
report, or other non-machine readable format into a machine readable
format. See the data rescue template at
`01_DataRescue/_TemplateYYYY/README_AuthorYYYY.qmd` for detailed
checklist.

Data rescue is the transformation of information from a non-machine
readable format to a machine readable one. This can include scanning
paper copies of reports or records, transcribing scanned records into
flat text documents, or extracting x-y values from a scanned graph.
Generally this stage should be as close to the original information as
practical for specific purpose, and should be extendable for future
purpose. Information here may be primary or meta data. This stage must
maintain provenance of the data including links to the original source,
how the data was made machine readable, what data was not rescued, and
who did this work.

| Data Rescue | Lead | Do | Measure |
|----|----|----|----|
| Input: Understand data source <a href="#sec-UndertandingDataSource" class="quarto-xref">Section 1</a> | Identify, find, and read the paper | Summarize the data in paper and identify relevant components | *Expert Review*: Does the summary reflect the paper? is it shorter then the paper itself? |
| Transformation: Transcribe the data <a href="#sec-TranscribingData" class="quarto-xref">Section 2</a> | Prep the spreadsheets and any figure capture software | Transcribe data from text, tables, and figures | Compare and reconcile data from second transcriber |
| Output: Push rescue to github <a href="#sec-ReviewingTranscripts" class="quarto-xref">Section 3</a> | Prep transcription package | Pull request to incorporate the data rescue into the repository | *Expert Review*: Does the pull request have the documentation identified below? Does the ReadMe render? |

This process should result in the following documentation:

- A ReadMe file with (qmd or Rmd)
  - human readable summary of data source with context for data rescue
  - specific plan for fit-for-purpose data rescue
  - all contributors identified in the metadata
- A data transcription(s) that have been reconciled from two independent
  data transcriptions. Data transcriptions may include,
  - transcriptions of tables with fit for purpose information
  - x-y extracted data points from figures
  - transcription of methods section with paired BibTex formatted
    reference
- BibTex formatted file with reference to
  1)  the original the data source(s)
  2)  citations from the methods section

## Understanding data source

### Identify, find, and read data source

Data sources can be identified in a number of different ways but will
generally be nominated by an expert in the field in connection for a
specific desired re-analysis. Reading the data source can include
reviewing manuscripts from the primary literature and their supplemental
information, or published agency reports. Specific attention should be
paid to the methods section, and any tables or figures. Generally there
will be a specific re-analysis identified and understanding should be
targeted towards this purpose.

*Example:*

> Townsend, A.R., Vitousek, P.M. and Trumbore, S.E. (1995), Soil Organic
> Matter Dynamics Along Gradients in Temperature and Land Use on the
> Island of Hawaii. Ecology, 76: 721-733.
> <https://doi.org/10.2307/1939339> is identified as a data source for
> the Hawai’i Soil Organic Carbon Database <https://osf.io/hmtv6/> as
> part of the
> [HiCSC](https://github.com/ktoddbrown/SoilDRaH/wiki/Hawaii-SOC).

### Summarize and propose plan

Write a short summary of the data in the data source. This should
include an overview of all the data in the source and highlight specific
sections or components for fit-for-purpose rescue. You should identify
key information in the methods section, individual tables and figures.
This summary is intended to provide a proposal for the data rescue plan
to be reviewed by an expert.

### Expert review

Pass the summary and proposed plan to a reviewer. This reviewer should
have some degree of familiarity of the methodologies used in the study
and the proposed reanalysis. The reviewer should consider check that:

-\[ \] Does the summary reflect the paper while being shorter then the
paper itself? -\[ \] Does the identified elements in the data rescue
plan serve the purpose of the reanalysis?

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
the data via manual entry.

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
concerned) and sub-tables may require additional considerations. Finally
figures like graphs are completely cryptic to text readers.

### Prep documents

Generally you will have three types of data capture files: a
[Markdown](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
for the methods section, a spreadsheet for the table and figure
captures, and software for the figure x-y captures.

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
    a `@AuthorYear` tag that corresponds to the bib label. This may
    change in the future (as of spring 2025).

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
    spreadsheet files are not always readable by modern software.
2)  Be sure to set your spreadsheet to text entries. Spreadsheets try to
    be clever and guess what type of data you are entering. Numbers are
    often truncated if they end in a zero, but this looses significant
    digits. Treat everything as a text when you are transcribing.
3)  Use the first row to copy the caption. This will ensure that the
    table and it’s caption are always together. The compromise here is
    that you have to skip the first row when reading the final csv, no
    ideal but also not difficult.
4)  Sub-table headers may new columns or multi column entires should be
    repeated. While spreadsheets will merge cells for you, it does not
    export nicely to csv tables. You should strive to preserve the
    structure of the data by repeating entries either in the header or
    as a new column.

## Reviewing transcripts

| Lead | Do | Measure |
|----|----|----|
| Prep transcription package | Create a pull request to incorporate the data rescue into the repository | Expert Review: Does the pull request have the documentation identified below? Does the ReadMe render? |

Almost there! The final step in this data rescue is an expert review of
the entire data package. Generally this is a fairly high level review
looking for specific sections, formatting, and a working Level 0 read
demo. It is possible (likely even) that when the data moves onto the
harmonization phase that there will be additional issues that will need
to be addressed.

**To create a pull request.** First you will need to fork the main
branch on the primary repository. Then create a new branch on your fork
for this specific rescue. Upload the following files to that branch
either via the web GUI or using a `git push` command. Finally create a
pull request on the primary repository asking for your branch to be
merged into the main branch and let an expert reviewer know that it’s
ready for review!

You will need to include the following files:

- A ReadMe file with (qmd)
- A data transcription(s) include methods sections (csv, md)
- BibTex files (bib)

**Reviewer checks** should include the following:

- A qmd ReadMe file that renders without error with the following
  sections
  1)  all contributors identified in the metadata
  2)  human readable summary of data source with context for data rescue
      with citation
      - A BibTex file with a single citation for the primary paper
        should be included as well using a *AuthorYear* bib key format
        with the first authors last name and year of publication.
  3)  specific plan for fit-for-purpose data rescue
  4)  a markdown read of the rescued text sections (generally Methods
      section)
  5)  chunks that reproduce the tables/figures rescued and explain any
      deviation from the format in the article.
  6)  a section that constructs the Level 0 data product
- Data transcription(s) that
  1)  transcriptions of tables or x-y extracted data points from figures
      with fit for purpose information are csv (comma separated) format
      - The first line should contain the caption and any footnotes
      - The second line should be header information that reflects the
        table headers as much as possible or figure axis/labels
      - There should be evidence of a cross check between two
        transcriptions that either includes a commented out section that
        uses a diff command to compare the two transcriptions or
        documentation as to why this was not done
  2)  transcription of methods section with paired BibTex formatted
      reference
      - methods section headers should be well formatted using `##` or
        `###` as appropriate with an empty line above and below.
      - mathematics in the text should be well formatted LaTex using
        `$ ... $`
      - citations should be linked to a BibTex file using a *AuthorYear*
        bib key format with the first authors last name and year of
        publication.
