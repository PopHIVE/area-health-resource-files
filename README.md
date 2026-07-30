# Area Health Resource File (AHRF)

This is a repackaging of the Area Health Resource File, published annually by
the Health Resources & Services Administration (HRSA):
https://data.hrsa.gov/data/download

It covers editions from 1999 through the present (HRSA did not publish a 2010
or 2013 edition), pulling each year's release in whichever format HRSA used
at the time (zipped fixed-width files with a separate SAS/layout tech doc,
direct `.asc` files, zips with an embedded `.asc`+`.sas`, or CSV component
zips), and reformats the combined result into the PopHIVE standard format
using the `dcf` package.

This repo is pulled by [PopHIVE/Ingest](https://github.com/PopHIVE/Ingest)'s
`data/area_health_resource_file/ingest.R`, which downloads the standardized
`standard/data.csv.gz` output directly rather than re-running this pipeline.
It's run manually/locally when HRSA publishes a new edition (annually), not
on an automated schedule.
