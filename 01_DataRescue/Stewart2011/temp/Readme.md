This folder should be used to hold files two kinds of files:
1) those that you do not intend to commit to the repository, and
2) secondary transcriptions used in the cross-checks that are committed

Files not committed are up to you but can include article pdf or a spreadsheet file.
By storing it locally in this folder you have a well organized way to access these files.
However these (pdf and xlsx) are binary files and may be under copyrights that are not compatible with the repository.
*Pull requests that have pdf or spreadsheet files will likely be reject.*

Secondary transcription files should be csv or markdown files matching the table, figure, and methods files in the primary parent directory (file names may include the initials of the secondary transcriber).
They should be referred to in the README in the cross-check section, and the primary/secondary cross check transcriptions should pass the line-by-line `diff` without any outputs.
These files may need to be updated during the cross-check phase.
