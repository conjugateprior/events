# Resubmission

* This is a package update and re-release after the previous package version 
  failed checks and was dropped from CRAN. This went unnoticed because my 
  institutional email address (will.lowe@uni-mannheim.de) in the
  DESCRIPTION ceased to function.

In this version I have

* Fixed my contact information to a more permanent non-institutional 
  email address (conjugateprior@gmail.com)

* Ensured the package passes all local CRAN checks

* Ensured the package passes RHub multiplatform CRAN checks.

I have also taken the opportunity to make some updates to the package 
functionality and bumped the version

## RHub check details:

RHub notes (on platforms *Ubuntu Linux 20.04.1 LTS, R-release, GCC* and 
*Fedora Linux, R-devel, clang, gfortran*) that 
```
Maintainer: ‘William Lowe <conjugateprior@gmail.com>’

New submission

Package was archived on CRAN
```
which is correct. That's still me and it was archived.

Rhub notes on *Windows Server 2022, R-devel, 64 bit*
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
There is no LaTeX source in the package (the vignette is RMarkdown), 
so MiKTeX should never be invoked. Presumably an RHub platform issue.

## Connecting the archived and new versions of the package

For reference, the source for the *archived* CRAN version of this package is 
on the HEAD of Github repo conjugateprior/events here:

* https://github.com/conjugateprior/events/commit/1f4dd4b5c273286bcd4fa772603d37361a842007

and the *current submission* is on the CRAN branch of the same repo here: 

* https://github.com/conjugateprior/events/tree/cran