# Files

* 01.R old -- superseded (focus on 04 and later)
* 02.R old -- superseded (focus on 04 and later)
* 03.R old -- superseded (focus on 04 and later)
* 04.R reading more data types
* 04_temperature.Rmd plot temperature using 04.out
* 05.R save info from FIRST LEADER data type into 05.rda
* 05_temperature.Rmd plot temperature using 05.rda
* 06.R read heading, pitch and roll. Note that this is very slow.


# Plans

* Why is 06.R so slow?  Try [] method instead of c() method.
* How are times linked up between entries?  Need to read the docs.
* Just what *are* the entries?  There are data chunks within entries, so this
  is a bit like an ad2cp file, in a way.  But, for the latter, each separate
  thread is complete (e.g. holds time information).
