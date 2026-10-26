# =============================================================================
# Area Health Resource File (AHRF) Multi-Year Data Ingestion
# Source: HRSA Area Health Resource File — https://data.hrsa.gov/data/download
#
# Editions covered:
#   1999:        AHRF1999.zip (data) + AHRF_USER_TECH_1999-2000.zip (layout.txt)
#   2000-2006:   AHRF{Y}.zip  (data) + AHRF_USER_TECH_{Y}-{Y+1}.zip (SAS format)
#   2007-2008:   AHRF{Y}.zip  (data) + AHRF_USER_TECH_{Y}-{Y+1}.zip (layout.txt)
#   2009,2011-2017: ahrf{Y}.asc (direct) + AHRF_USER_TECH_{...}.zip (SAS format)
#   2018-2022:   AHRF_{Y-1}-{Y}.zip  (data + embedded SAS)
#   2023-2025:   CSV component files in zip
#
# Notes:
#   - 2010, 2013: no data file on HRSA servers → skipped
# =============================================================================

library(dplyr)
library(vroom)
library(readr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

if (!file.exists("process.json")) {
  process <- list(raw_state = NULL)
} else {
  process <- dcf::dcf_process_record()
}

# ---------------------------------------------------------------------------
# Helper: find a file by pattern within a directory (recursive, case-insensitive)
# ---------------------------------------------------------------------------
find_file <- function(dir, pattern) {
  hits <- list.files(dir, pattern = pattern, recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  if (length(hits)) hits[1] else NULL
}

# ---------------------------------------------------------------------------
# 1. Edition manifest
# ---------------------------------------------------------------------------
base_url   <- "https://data.hrsa.gov/DataDownload/AHRF"
static_url <- "https://data.hrsa.gov/DataDownload/StaticDocuments"

editions <- list(
  # --- 1999: old-style zip + tech doc with pipe-delimited layout.txt ---
  list(yr = 1999L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF1999.zip"),
       data_local = "raw/AHRF1999.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_1999-2000.zip"),
       tech_local = "raw/AHRF_USER_TECH_1999-2000.zip"),

  # --- 2000-2006: old-style zip + separate tech doc zip (SAS format) ---
  list(yr = 2000L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2000.zip"),
       data_local = "raw/AHRF2000.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2000-2001.zip"),
       tech_local = "raw/AHRF_USER_TECH_2000-2001.zip"),
  list(yr = 2001L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2001.zip"),
       data_local = "raw/AHRF2001.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2001-2002.zip"),
       tech_local = "raw/AHRF_USER_TECH_2001-2002.zip"),
  list(yr = 2002L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2002.zip"),
       data_local = "raw/AHRF2002.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2002-2003.zip"),
       tech_local = "raw/AHRF_USER_TECH_2002-2003.zip"),
  list(yr = 2003L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2003.zip"),
       data_local = "raw/AHRF2003.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2003-2004.zip"),
       tech_local = "raw/AHRF_USER_TECH_2003-2004.zip"),
  list(yr = 2004L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2004.zip"),
       data_local = "raw/AHRF2004.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2004-2005.zip"),
       tech_local = "raw/AHRF_USER_TECH_2004-2005.zip"),
  list(yr = 2005L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2005.zip"),
       data_local = "raw/AHRF2005.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2005-2006.zip"),
       tech_local = "raw/AHRF_USER_TECH_2005-2006.zip"),
  list(yr = 2006L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2006.zip"),
       data_local = "raw/AHRF2006.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2006-2007.zip"),
       tech_local = "raw/AHRF_USER_TECH_2006-2007.zip"),

  # --- 2007-2008: old-style zip + tech doc with layout.txt (no SAS) ---
  list(yr = 2007L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2007.zip"),
       data_local = "raw/AHRF2007.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2007-2008.zip"),
       tech_local = "raw/AHRF_USER_TECH_2007-2008.zip"),
  list(yr = 2008L, type = "old_zip",
       data_url   = paste0(base_url, "/AHRF2008.zip"),
       data_local = "raw/AHRF2008.zip",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2008-2009.zip"),
       tech_local = "raw/AHRF_USER_TECH_2008-2009.zip"),

  # --- Direct ASC file + separate tech doc zip ---
  # Note: 2010 (404) and 2013 (404) do not exist on HRSA servers
  list(yr = 2009L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2009.asc"),
       data_local = "raw/ahrf2009.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2009-2010.zip"),
       tech_local = "raw/AHRF_USER_TECH_2009-2010.zip"),
  list(yr = 2011L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2011.asc"),
       data_local = "raw/ahrf2011.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2011-2012.zip"),
       tech_local = "raw/AHRF_USER_TECH_2011-2012.zip"),
  list(yr = 2012L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2012.asc"),
       data_local = "raw/ahrf2012.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2012-2013.zip"),
       tech_local = "raw/AHRF_USER_TECH_2012-2013.zip"),
  list(yr = 2014L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2014.asc"),
       data_local = "raw/ahrf2014.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2013-2014.zip"),
       tech_local = "raw/AHRF_USER_TECH_2013-2014.zip"),
  list(yr = 2015L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2015.asc"),
       data_local = "raw/ahrf2015.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2014-2015.zip"),
       tech_local = "raw/AHRF_USER_TECH_2014-2015.zip"),
  list(yr = 2016L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2016.asc"),
       data_local = "raw/ahrf2016.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2015-2016.zip"),
       tech_local = "raw/AHRF_USER_TECH_2015-2016.zip"),
  list(yr = 2017L, type = "direct_asc",
       data_url   = paste0(base_url, "/ahrf2017.asc"),
       data_local = "raw/ahrf2017.asc.gz",
       tech_url   = paste0(base_url, "/AHRF_USER_TECH_2016-2017.zip"),
       tech_local = "raw/AHRF_USER_TECH_2016-2017.zip"),

  # --- Range zip (data .asc + embedded .sas) ---
  list(yr = 2018L, type = "asc_zip",
       data_url   = paste0(base_url, "/AHRF_2017-2018.zip"),
       data_local = "raw/AHRF_2017-2018.zip"),
  list(yr = 2019L, type = "asc_zip",
       data_url   = paste0(base_url, "/AHRF_2018-2019.zip"),
       data_local = "raw/AHRF_2018-2019.zip"),
  list(yr = 2020L, type = "asc_zip",
       data_url   = paste0(base_url, "/AHRF_2019-2020.zip"),
       data_local = "raw/AHRF_2019-2020.zip"),
  list(yr = 2021L, type = "asc_zip",
       data_url   = paste0(base_url, "/AHRF_2020-2021.zip"),
       data_local = "raw/AHRF_2020-2021.zip"),
  list(yr = 2022L, type = "asc_zip",
       data_url   = paste0(base_url, "/AHRF_2021-2022.zip"),
       data_local = "raw/AHRF_2021-2022.zip"),

  # --- CSV component zip ---
  # yr = final year of range (matches filenames inside: ahrf{yr}*.csv)
  list(yr = 2023L, type = "csv_zip",
       data_url   = paste0(static_url, "/AHRF_CSV_2022-2023.zip"),
       data_local = "raw/AHRF_CSV_2022-2023.zip"),
  list(yr = 2024L, type = "csv_zip",
       data_url   = paste0(static_url, "/AHRF%202023-2024%20CSV.zip"),
       data_local = "raw/AHRF_2023-2024_CSV.zip"),
  list(yr = 2025L, type = "csv_zip",
       data_url   = paste0(base_url, "/AHRF_2024-2025_CSV.zip"),
       data_local = "raw/AHRF_2024-2025_CSV.zip")
)

# ---------------------------------------------------------------------------
# 2. Download raw files (data + tech docs where applicable)
# ---------------------------------------------------------------------------
safe_download <- function(url, dest) {
  if (file.exists(dest)) return(invisible(NULL))
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  # HRSA serves these uncompressed; when dest is a .gz path, fetch to a temp
  # file first and gzip it into place so the raw store stays compressed.
  is_gz  <- grepl("\\.gz$", dest)
  fetch_to <- if (is_gz) tempfile() else dest
  tryCatch({
    suppressWarnings(download.file(url, fetch_to, mode = "wb", quiet = TRUE))
    if (is_gz) {
      in_con  <- file(fetch_to, "rb")
      out_con <- gzfile(dest, "wb")
      writeBin(readBin(in_con, "raw", n = file.size(fetch_to)), out_con)
      close(in_con); close(out_con)
      file.remove(fetch_to)
    }
  },
    error = function(e) {
      message("Could not download ", basename(dest))
      if (file.exists(fetch_to)) file.remove(fetch_to)
      if (file.exists(dest)) file.remove(dest)
    }
  )
}

for (ed in editions) {
  safe_download(ed$data_url, ed$data_local)
  if (!is.null(ed$tech_url)) safe_download(ed$tech_url, ed$tech_local)
}

raw_files    <- sort(list.files("raw", full.names = TRUE, recursive = FALSE))
current_hash <- list(hash = unname(tools::md5sum(raw_files)))

if (!identical(process$raw_state, current_hash)) {

  # -------------------------------------------------------------------------
  # 3. ASCII parser functions
  # -------------------------------------------------------------------------

  # Parse SAS INPUT fixed-width spec: @START  varname  $? WW.D?  /* desc */
  # Returns data.frame(start, varname, width, decimals, desc, year_4digit)
  parse_sas_format <- function(sas_file) {
    lines <- readLines(sas_file, warn = FALSE, encoding = "latin1")
    pat   <- "^\\s*@(\\d+)\\s+(\\S+)\\s+\\$?\\s*(\\d+)\\.(\\d?)\\s*/\\*(.*)\\*/"
    rows  <- lapply(lines, function(ln) {
      m <- regexec(pat, ln, perl = TRUE)
      r <- regmatches(ln, m)[[1]]
      if (length(r) == 0) return(NULL)
      data.frame(start       = as.integer(r[2]),
                 varname     = r[3],
                 width       = as.integer(r[4]),
                 decimals    = if (nchar(r[5]) > 0) as.integer(r[5]) else 0L,
                 desc        = trimws(r[6]),
                 year_4digit = NA_integer_,
                 stringsAsFactors = FALSE)
    })
    fmt <- do.call(rbind, Filter(Negate(is.null), rows))

    # The same SAS file also carries an explicit value-label section
    # (`varname = "description YYYY"`) giving each variable's true
    # 4-digit year directly. This is the only reliable way to order
    # variables chronologically: some editions reuse a 2-digit varname
    # suffix as a bare internal sequence number rather than a year --
    # e.g. f1198495 is labeled "Population Estimate 1995", not a
    # variable newer than f1198421's "Population Estimate 2021" ("95"
    # sorts higher than "21" as a raw 2-digit integer, but 1995 is 26
    # years OLDER than 2021). Confirmed to cause a real bug: without
    # this, find_sas_var's varname-suffix fallback picked f1198495 as
    # the "highest year" over f1198421, so Lincoln County SD's
    # ahrf_population came out as 17,666 (the actual 1995 count) instead
    # of 67,870 (the correct, most recent 2021 count) in the 2021-2022
    # edition. find_sas_var already prefers year_4digit over the
    # varname-suffix guess whenever it's non-NA (see below), so
    # populating it correctly here is the entire fix.
    label_pat  <- "^(\\S+)\\s*=\\s*\"[^\"]*?(\\d{4})\"\\s*$"
    label_rows <- lapply(lines, function(ln) {
      m <- regexec(label_pat, ln, perl = TRUE)
      r <- regmatches(ln, m)[[1]]
      if (length(r) == 0) return(NULL)
      yr <- suppressWarnings(as.integer(r[3]))
      if (is.na(yr) || yr < 1960L || yr > 2035L) return(NULL)
      data.frame(varname = r[2], label_year = yr, stringsAsFactors = FALSE)
    })
    labels <- do.call(rbind, Filter(Negate(is.null), label_rows))

    if (!is.null(labels) && nrow(labels) > 0L && !is.null(fmt) && nrow(fmt) > 0L) {
      # A varname's label line is always its own "varname = description
      # YEAR" line immediately after its @position declaration in every
      # edition checked; keep the first match if a varname is ever
      # repeated for some unrelated reason elsewhere in the file.
      labels <- labels[!duplicated(labels$varname), ]
      fmt$year_4digit <- labels$label_year[match(fmt$varname, labels$varname)]
    }

    fmt
  }

  # Parse plain-text layout files (pipe-delimited 1999/2007 or space-delimited 2008).
  # Returns data.frame(start, varname, width, decimals, desc, year_4digit)
  #
  # All three formats share: FIELDNAME  SSSSS-EEEEE  [YYYY]  DESCRIPTION...
  # The field name is anchored at start of line; positions are five-digit pairs.
  parse_layout_txt <- function(layout_file) {
    lines <- readLines(layout_file, warn = FALSE, encoding = "latin1")
    # Match: optional leading space, field name (may have -YY year suffix),
    # optional pipe/space, then 5-digit start - 5-digit end, then rest.
    pat <- "^\\s*([A-Z][\\w-]+?)\\s*\\|?\\s*(\\d{5})-(\\d{5})(.*)"
    rows <- lapply(lines, function(ln) {
      m <- regexec(pat, ln, perl = TRUE)
      r <- regmatches(ln, m)[[1]]
      if (length(r) == 0) return(NULL)
      varname <- r[2]
      start   <- as.integer(r[3])
      end     <- as.integer(r[4])
      rest    <- r[5]

      # Normalize separators: replace pipes with semicolons, collapse whitespace
      rest <- gsub("\\|", ";", rest)
      rest <- gsub("\\s+", " ", trimws(rest))
      rest <- gsub("^[; ]+", "", rest)   # strip leading separators

      # Extract optional 4-digit year at start (e.g. "2006 " or "; 2006 ;")
      year_4digit <- NA_integer_
      ym <- regexec("^(\\d{4})\\s", rest, perl = TRUE)[[1]]
      if (ym[1] > 0) {
        yr_val <- as.integer(substr(rest, ym[2], ym[2] + 3L))
        if (!is.na(yr_val) && yr_val >= 1970L && yr_val <= 2035L) {
          year_4digit <- yr_val
          rest <- gsub("^[; ]+", "", trimws(sub("^\\d{4}\\s+", "", rest)))
        }
      }

      # Detect implied decimals from notation like (.1) (.01) etc.
      decimals <- 0L
      dm <- regexec("\\(\\.([0-9]+)\\)", rest, perl = TRUE)[[1]]
      if (dm[1] > 0) decimals <- nchar(substr(rest, dm[2], dm[2] + attr(dm, "match.length")[2] - 1L))

      data.frame(start       = start,
                 varname     = varname,
                 width       = end - start + 1L,
                 decimals    = decimals,
                 desc        = trimws(rest),
                 year_4digit = year_4digit,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  }

  # Find the variable matching a description pattern.
  # When multiple matches exist (e.g., multi-year editions), pick the one
  # with the highest year. Uses explicit year_4digit column when available
  # (layout.txt format); otherwise extracts 2-digit year from varname (SAS format).
  find_sas_var <- function(fmt, pattern) {
    hits <- fmt[grepl(pattern, fmt$desc, ignore.case = TRUE, perl = TRUE), ]
    if (nrow(hits) == 0) return(NULL)

    if (!is.null(hits$year_4digit) && any(!is.na(hits$year_4digit))) {
      hits$sort_yr <- ifelse(!is.na(hits$year_4digit), hits$year_4digit, -1L)
    } else {
      has_yr <- nchar(hits$varname) >= 8L
      yr_num <- suppressWarnings(as.integer(
        substr(hits$varname, nchar(hits$varname) - 1L, nchar(hits$varname))
      ))
      # Every AHRF edition checked keeps a rolling window of 2-digit-suffix
      # variables spanning both the 1990s and 2000s+ under the same
      # description with no year label at all (e.g. f1198401..f1198413
      # alongside f1198491..f1198499) -- comparing the raw suffix as a bare
      # integer put "99" above "01", so the 1999 value was picked over the
      # far more current 2001 one. Confirmed on Henrico County VA (51087):
      # the 2002 edition's un-labeled "01"-suffix population variable reads
      # 264,973 (a plausible continuation of the true ~262,300 2000 Census
      # count) while its "99"-suffix variable -- the one this fallback used
      # to pick -- reads 244,652. Same pattern confirmed for ahrf_md_all
      # (suffix "00" reads 1,325, consistent with neighboring years; suffix
      # "99" reads an implausible 137).
      #
      # The pivot below can't sit at the textbook 50: physician-count
      # variables carry a legacy baseline all the way back to suffix "40"
      # (1940, e.g. f0885740) -- a <50 pivot would misread that as 2040. The
      # newest un-labeled suffix seen across every measure and edition
      # checked is 25 (2025); the oldest legacy suffix is 40 (1940). 30 sits
      # in that gap with margin on both sides.
      full_yr <- ifelse(yr_num < 30L, 2000L + yr_num, 1900L + yr_num)
      hits$sort_yr <- ifelse(has_yr & !is.na(yr_num), full_yr, -1L)
    }
    hits[which.max(hits$sort_yr), ]
  }

  # Every match, with the data year each one carries. find_sas_var above keeps
  # only the newest, which discards the back-series every edition holds and
  # labels the value with the edition year instead of the data year.
  find_sas_vars <- function(fmt, pattern) {
    hits <- fmt[grepl(pattern, fmt$desc, ignore.case = TRUE, perl = TRUE), ]
    if (nrow(hits) == 0) return(NULL)

    if (!is.null(hits$year_4digit) && any(!is.na(hits$year_4digit))) {
      hits$sort_yr <- ifelse(!is.na(hits$year_4digit), hits$year_4digit, -1L)
    } else {
      has_yr <- nchar(hits$varname) >= 8L
      yr_num2 <- suppressWarnings(as.integer(
        substr(hits$varname, nchar(hits$varname) - 1L, nchar(hits$varname))
      ))
      # Century pivot as in find_sas_var.
      full_yr <- ifelse(yr_num2 < 30L, 2000L + yr_num2, 1900L + yr_num2)
      hits$sort_yr <- ifelse(has_yr & !is.na(yr_num2), full_yr, -1L)
    }
    # One row per data year; on a repeated year the later position wins.
    hits <- hits[order(hits$sort_yr, hits$start), ]
    hits[!duplicated(hits$sort_yr, fromLast = TRUE), ]
  }

  # Variable targets: each entry is list(output_column_name, description_regex)
  # Patterns cover both modern SAS naming and older layout.txt naming conventions.
  targets <- list(
    list("fips_st_cnty",             "Header.*FIPS.*St.*Cty|FIPS St.*Cty Code"),
    list("ahrf_hpsa_prim_care",      "HPSA Code.*Primary Care"),
    list("ahrf_hpsa_dental",         "HPSA Code.*Dentists"),
    list("ahrf_hpsa_mental_health",  "HPSA Code.*Mental"),
    list("ahrf_rural_urban_code",    "Rural.Urban Continuum"),
    list("ahrf_pcp",                 "Phys,Primary Care, Patient Care.*Excl"),
    list("ahrf_md_all",              "M\\.D\\..*Total Ptn Care Non|^Total Active M\\.D\\..*Non.Federal"),
    list("ahrf_psych",               "Psychiatry, Total.*Non-Fed;MD"),
    list("ahrf_dentists",            "Dentists w/NPI"),
    list("ahrf_hospitals",           "^Total Number Hospitals"),
    list("ahrf_critical_access_hosp","^# Critical Access Hospitals"),
    list("ahrf_population",          "^Population Estimate"),
    list("ahrf_good_air_pct",        "% Good Air Quality"),
    list("ahrf_pm25",                "Fine Particulate Matter"),
    list("ahrf_pop_density",         "Population Density per Sq Mile"),
    list("ahrf_medicare_per_capita", "Actual Per Capita Medicare Cost"),
    list("ahrf_ed_per_1k_medicare",  "ED Visits per 1K Medicare")
  )

  # Core parser: given an ASC data file and a pre-parsed format data.frame,
  # extract target columns and return a tidy data frame.
  parse_asc_data <- function(asc_path, fmt, yr_num) {
    if (is.null(fmt) || nrow(fmt) == 0) {
      message("  No parseable format for yr=", yr_num); return(NULL)
    }
    # The data year is encoded in the column name so it survives read_fwf.
    vars <- lapply(targets, function(t) {
      if (t[[1]] == "fips_st_cnty") {
        v <- find_sas_var(fmt, t[[2]])
        if (is.null(v)) return(NULL)
        v$out_name <- t[[1]]
        return(v)
      }
      v <- find_sas_vars(fmt, t[[2]])
      if (is.null(v)) return(NULL)
      # No detectable year means the variable is the edition's own value.
      v$data_year <- ifelse(is.na(v$sort_yr) | v$sort_yr < 0L, yr_num, v$sort_yr)
      v$out_name  <- paste0(t[[1]], "@@", v$data_year)
      v
    })
    vars <- Filter(Negate(is.null), vars)
    if (length(vars) == 0) { message("  No matching vars for yr=", yr_num); return(NULL) }

    vdf <- dplyr::bind_rows(vars)
    vdf <- vdf[!duplicated(vdf$out_name), ]

    result <- readr::read_fwf(
      asc_path,
      readr::fwf_positions(
        start     = vdf$start,
        end       = vdf$start + vdf$width - 1L,
        col_names = vdf$out_name
      ),
      col_types      = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress       = FALSE
    )

    for (i in seq_len(nrow(vdf))) {
      nm  <- vdf$out_name[i]
      dec <- vdf$decimals[i]
      if (dec > 0L)
        result[[nm]] <- suppressWarnings(as.numeric(result[[nm]])) / (10^dec)
    }

    result %>%
      mutate(
        fips_raw  = trimws(fips_st_cnty),
        # Pad 4-digit FIPS to 5 chars when raw data omits leading zero
        geography = case_when(
          nchar(fips_raw) == 5L ~ fips_raw,
          nchar(fips_raw) == 4L ~ paste0("0", fips_raw),
          TRUE                  ~ NA_character_
        )
      ) %>%
      filter(!is.na(geography)) %>%
      select(geography, dplyr::any_of(vdf$out_name[vdf$out_name != "fips_st_cnty"])) %>%
      # pivot_longer refuses mixed types, and the decimal loop above converts
      # only some columns.
      mutate(across(-geography, ~ suppressWarnings(as.numeric(.)))) %>%
      tidyr::pivot_longer(
        -geography,
        names_to  = c("measure", "data_year"),
        names_sep = "@@",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) %>%
      transmute(
        geography,
        time    = paste0(data_year, "-12-31"),
        measure,
        value,
        edition = as.integer(yr_num)
      )
  }

  # Load format from a tech doc zip: try SAS first, fall back to layout.txt.
  load_fmt_from_zip <- function(tech_zip, yr_num) {
    tmp <- tempfile(); dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    tryCatch({
      unzip(tech_zip, exdir = tmp)
      sas_f    <- find_file(tmp, "\\.sas$|\\.SAS\\.txt$|SA\\.DOC\\.txt$")
      layout_f <- if (is.null(sas_f))
        find_file(tmp, "layout\\.txt$|ARF\\d{4}\\.txt$") else NULL
      if (!is.null(sas_f)) {
        parse_sas_format(sas_f)
      } else if (!is.null(layout_f)) {
        parse_layout_txt(layout_f)
      } else {
        message("  No SAS or layout file in tech doc for yr=", yr_num); NULL
      }
    }, error = function(e) { message("  Error loading fmt yr=", yr_num, ": ", e$message); NULL })
  }

  # Wrapper: old-style data zip (.txt inside) + tech doc zip
  parse_old_zip <- function(data_zip, tech_zip, yr_num) {
    tmp1 <- tempfile(); dir.create(tmp1)
    on.exit(unlink(tmp1, recursive = TRUE))
    tryCatch({
      unzip(data_zip, exdir = tmp1)
      data_f <- find_file(tmp1, "\\.(txt|asc)$")
      if (is.null(data_f)) {
        message("  No data file in zip for yr=", yr_num); return(NULL)
      }
      fmt <- load_fmt_from_zip(tech_zip, yr_num)
      if (is.null(fmt) || nrow(fmt) == 0) return(NULL)
      parse_asc_data(data_f, fmt, yr_num)
    }, error = function(e) { message("  Error yr=", yr_num, ": ", e$message); NULL })
  }

  # Wrapper: direct .asc file + tech doc zip
  parse_direct_asc <- function(asc_path, tech_zip, yr_num) {
    fmt <- load_fmt_from_zip(tech_zip, yr_num)
    if (is.null(fmt) || nrow(fmt) == 0) return(NULL)
    tryCatch(
      parse_asc_data(asc_path, fmt, yr_num),
      error = function(e) { message("  Error yr=", yr_num, ": ", e$message); NULL }
    )
  }

  # Wrapper: range zip with embedded .asc + .sas
  parse_asc_zip <- function(zip_path, yr_num) {
    tmp <- tempfile(); dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    tryCatch({
      unzip(zip_path, exdir = tmp)
      asc_f <- find_file(tmp, "\\.(asc|txt)$")
      sas_f <- find_file(tmp, "\\.sas$")
      if (is.null(asc_f) || is.null(sas_f)) {
        message("  Missing ASC or SAS in ", basename(zip_path)); return(NULL)
      }
      fmt <- parse_sas_format(sas_f)
      parse_asc_data(asc_f, fmt, yr_num)
    }, error = function(e) { message("  Error yr=", yr_num, ": ", e$message); NULL })
  }

  # -------------------------------------------------------------------------
  # 4. CSV parser (component files with dynamic year-suffix column selection)
  # -------------------------------------------------------------------------
  # Every year-suffixed column, returned long. The CSV editions carry two years
  # per measure, so reading both lets the newest edition correct the year before.
  read_all_cols <- function(file, base_col_map) {
    if (is.null(file)) return(NULL)
    tryCatch({
      all_names <- suppressMessages(
        names(vroom(file, n_max = 0, show_col_types = FALSE))
      )
      picks <- list()
      for (out_nm in names(base_col_map)) {
        base  <- base_col_map[[out_nm]]
        pat   <- paste0("^", gsub("\\.", "\\\\.", base), "_\\d{2}$")
        cands <- all_names[grepl(pat, all_names, ignore.case = TRUE)]
        for (cd in cands) {
          yy <- as.integer(sub(".*_", "", cd))
          picks[[cd]] <- list(
            measure   = out_nm,
            data_year = if (yy < 30L) 2000L + yy else 1900L + yy
          )
        }
      }
      if (length(picks) == 0) return(NULL)
      # Read as text: vroom guesses each column's type from the first rows, so a
      # count column that is 0/1 early on is typed as logical and its later
      # decimal values are dropped as parse failures. as.numeric() below converts.
      df <- suppressMessages(
        vroom(file, col_select = c("fips_st_cnty", all_of(names(picks))),
              col_types = readr::cols(.default = readr::col_character()),
              show_col_types = FALSE)
      )
      df$fips_st_cnty <- ifelse(
        nchar(df$fips_st_cnty) == 4L, paste0("0", df$fips_st_cnty), df$fips_st_cnty
      )
      out <- lapply(names(picks), function(cd) {
        tibble(
          fips_st_cnty = df$fips_st_cnty,
          measure      = picks[[cd]]$measure,
          data_year    = picks[[cd]]$data_year,
          value        = suppressWarnings(as.numeric(df[[cd]]))
        )
      })
      dplyr::bind_rows(out) %>% filter(!is.na(value))
    }, error = function(e) NULL)
  }

  parse_csv_zip <- function(zip_path, yr_num) {
    tmp_dir <- tempfile(); dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE))
    tryCatch({
      unzip(zip_path, exdir = tmp_dir)

      comp <- function(sfx)
        find_file(tmp_dir, paste0("ahrf", yr_num, sfx, ".*\\.csv$")) %||%
        find_file(tmp_dir, paste0("AHRF", yr_num, sfx, ".*\\.csv$"))

      geo <- read_all_cols(comp("geo"), c(
        ahrf_hpsa_prim_care     = "hpsa_prim_care",
        ahrf_hpsa_dental        = "hpsa_dent",
        ahrf_hpsa_mental_health = "hpsa_mentl_hlth",
        ahrf_rural_urban_code   = "rural_urban_contnm"
      ))
      hp <- read_all_cols(comp("hp"), c(
        ahrf_pcp         = "phys_nf_prim_care_pc_exc_rsdt",
        ahrf_md_all      = "md_nf_all_pc",
        ahrf_psych       = "md_nf_psych",
        ahrf_dentists    = "dent_npi",
        ahrf_obgyn_nf_do = "do_nf_obgyn_gen_all_pc",
        ahrf_obgyn_nf_md = "md_nf_obgyn_gen_all_pc"
      ))
      hf <- read_all_cols(comp("hf"), c(
        ahrf_hospitals                 = "hosp",
        ahrf_critical_access_hosp      = "critcl_access_hosp",
        ahrf_obstetric_hospitals       = "stgh_obstetrc_care",
        ahrf_obstetric_beds            = "stgh_obstetrcs_care_beds",
        ahrf_neonatal_icu_hospitals    = "stgh_neonat_icu",
        ahrf_neonatal_icu_beds         = "stgh_neonat_icu_beds",
        ahrf_neonatal_interm_hospitals = "stgh_neonat_interm_care",
        ahrf_neonatal_interm_beds      = "stgh_neonat_interm_care_beds",
        ahrf_birth_room_hospitals      = "stgh_birth_postprtm_rm",
        ahrf_teen_outreach_hospitals   = "stgh_teen_outrch_svc"
      ))
      # Maternal and infant measures come from the 2023+ CSV editions only.
      pop <- read_all_cols(comp("pop"), c(
        ahrf_population             = "popn",
        ahrf_births                 = "births_3yr_avg",
        ahrf_births_wh              = "births_3yr_wh_avg",
        ahrf_births_bl              = "births_3yr_bl_avg",
        ahrf_births_hsp             = "births_3yr_hsp_avg",
        ahrf_births_oth             = "births_3yr_oth_avg",
        ahrf_births_pretrm          = "births_pretrm_3yr_avg",
        ahrf_births_to_teens        = "births_to_teens_3yr_lt18_avg",
        ahrf_low_birth_wgt          = "lo_birth_wgt_3yr_avg",
        ahrf_very_low_birth_wgt     = "very_lo_birth_wgt_3yr_avg",
        ahrf_inf_mort_rate          = "inf_mort_rate_5yr_lt1_avg",
        ahrf_infant_deaths          = "mort_age_yr_3yr_lt1_avg",
        ahrf_infant_deaths_fem_wh   = "inf_deth_3yr_fem_wh_lt1_avg",
        ahrf_infant_deaths_fem_bl   = "inf_deth_3yr_fem_bl_lt1_avg",
        ahrf_infant_deaths_fem_hsp  = "inf_deth_3yr_fem_hsp_lt1_avg",
        ahrf_infant_deaths_fem_oth  = "oth_inf_deth_3yr_fem_lt1_avg",
        ahrf_infant_deaths_mal_wh   = "inf_deth_3yr_mal_wh_lt1_avg",
        ahrf_infant_deaths_mal_bl   = "inf_deth_3yr_mal_bl_lt1_avg",
        ahrf_infant_deaths_mal_hsp  = "inf_deth_3yr_mal_hsp_lt1_avg",
        ahrf_infant_deaths_mal_oth  = "oth_inf_deth_3yr_mal_lt1_avg"
      ))
      env <- read_all_cols(comp("env"), c(
        ahrf_good_air_pct = "good_air_qulty_dys_pct",
        ahrf_pm25         = "annul_partclt_mattr_2_5_avg",
        ahrf_pop_density  = "popn_densty_per_squr_mi"
      ))
      exp <- read_all_cols(comp("exp"), c(
        ahrf_medicare_per_capita = "actl_per_cap_ffs_cost"
      ))
      utl <- read_all_cols(comp("utl"), c(
        ahrf_ed_per_1k_medicare = "ed_vists_per_1k_medcr_ffs"
      ))

      parts <- Filter(Negate(is.null), list(geo, hp, hf, pop, env, exp, utl))
      if (length(parts) == 0L) return(NULL)

      # Components are already long, so they stack rather than join.
      dplyr::bind_rows(parts) %>%
        mutate(
          geography = case_when(
            nchar(fips_st_cnty) == 5L ~ fips_st_cnty,
            nchar(fips_st_cnty) == 4L ~ paste0("0", fips_st_cnty),
            TRUE                       ~ NA_character_
          )
        ) %>%
        filter(!is.na(geography)) %>%
        transmute(
          geography,
          time    = paste0(data_year, "-12-31"),
          measure,
          value,
          edition = as.integer(yr_num)
        )
    }, error = function(e) {
      message("  Error CSV yr=", yr_num, ": ", e$message); NULL
    })
  }

  # -------------------------------------------------------------------------
  # 5. Process each edition
  # -------------------------------------------------------------------------
  year_dfs <- list()

  for (ed in editions) {
    message("Processing yr=", ed$yr, " (", ed$type, ")")
    result <- NULL

    if (ed$type == "old_zip") {
      if (file.exists(ed$data_local) && file.exists(ed$tech_local))
        result <- parse_old_zip(ed$data_local, ed$tech_local, ed$yr)
      else
        message("  Skipping yr=", ed$yr, ": file(s) not downloaded")

    } else if (ed$type == "direct_asc") {
      if (file.exists(ed$data_local) && file.exists(ed$tech_local))
        result <- parse_direct_asc(ed$data_local, ed$tech_local, ed$yr)
      else
        message("  Skipping yr=", ed$yr, ": file(s) not downloaded")

    } else if (ed$type == "asc_zip") {
      if (file.exists(ed$data_local))
        result <- parse_asc_zip(ed$data_local, ed$yr)
      else
        message("  Skipping yr=", ed$yr, ": zip not downloaded")

    } else if (ed$type == "csv_zip") {
      if (file.exists(ed$data_local))
        result <- parse_csv_zip(ed$data_local, ed$yr)
      else
        message("  Skipping yr=", ed$yr, ": zip not downloaded")
    }

    if (!is.null(result) && nrow(result) > 0L) {
      message("  -> ", nrow(result), " obs across ",
              length(unique(result$time)), " data year(s), ",
              length(unique(result$measure)), " measure(s)")
      year_dfs[[as.character(ed$yr)]] <- result
    } else {
      message("  -> no data")
    }
  }

  # -------------------------------------------------------------------------
  # 6. Combine, resolve known Alaska disagreements, aggregate to state and
  #    national, and write. AHRF is natively county-only (fips_st_cnty), so
  #    state/national rows are computed here rather than provided by HRSA --
  #    mixed into the same standard/data.csv.gz by geography length (5 =
  #    county, 2 = state, "00" = national), matching every other multi-level
  #    source in this pipeline (e.g. NSSP, CMS MMD) rather than shipping
  #    separate per-level files.
  # -------------------------------------------------------------------------
  if (length(year_dfs) > 0L) {
    # Assemble by data year, newest edition winning. Editions overlap and
    # restate history, so the same (geography, year, measure) is published more
    # than once; the newest edition is the publisher's current view of it.
    # Only ahrf_md_all reaches before 1970, and thinly.
    AHRF_FIRST_YEAR <- 1970L

    long_all <- bind_rows(year_dfs) %>%
      filter(as.integer(substr(time, 1, 4)) >= AHRF_FIRST_YEAR)

    n_before <- nrow(long_all)
    long_resolved <- long_all %>%
      arrange(geography, time, measure, desc(edition)) %>%
      distinct(geography, time, measure, .keep_all = TRUE)

    restated <- long_all %>%
      group_by(geography, time, measure) %>%
      summarise(n_ed = n(), n_val = n_distinct(value), .groups = "drop") %>%
      filter(n_ed > 1L, n_val > 1L)

    message(
      "\nAssembled by data year: ", format(n_before, big.mark = ","),
      " observations from ", length(year_dfs), " editions -> ",
      format(nrow(long_resolved), big.mark = ","), " resolved; ",
      format(nrow(restated), big.mark = ","),
      " (measure, geography, year) cells were restated between editions"
    )

    data_standard <- long_resolved %>%
      select(-edition) %>%
      tidyr::pivot_wider(names_from = measure, values_from = value) %>%
      arrange(geography, time)

    # Retired Alaska borough/census-area FIPS codes for which AHRF sometimes
    # duplicates and sometimes disagrees with the current successor(s) for
    # the same (measure, time) -- see git history for how each row below was
    # resolved. Applied here, at the source, so every consumer (county rows
    # in this file, and the state/national aggregates below) gets clean,
    # non-duplicated data rather than each downstream repo needing its own
    # copy of this table.
    # Keep whichever generation was in effect for the data year: a retired area
    # is valid up to its dissolution, its successors from that date on. Replaces
    # a hand-listed set of rows keyed on edition years, which has no data-year
    # equivalent. Dates match GEOGRAPHY_LINEAGE in us-rates/geography_helpers.R.
    ak_lineage <- list(
      list(gens = list("02231", "02232", c("02230", "02105")),
           eff  = c(NA, "2007-06-20")),
      list(gens = list("02201", "02198"),            eff = c("2008-05-19")),
      list(gens = list("02280", c("02195", "02275")), eff = c("2008-06-01")),
      list(gens = list("02270", "02158"),            eff = c("2015-07-01")),
      list(gens = list("02261", c("02063", "02066")), eff = c("2019-01-02"))
    )

    validity <- dplyr::bind_rows(lapply(ak_lineage, function(ln) {
      n <- length(ln$gens)
      dplyr::bind_rows(lapply(seq_len(n), function(i) {
        from <- if (i == 1L) as.Date("1900-01-01") else as.Date(ln$eff[i - 1L])
        to   <- if (i == n)  as.Date("2999-12-31") else as.Date(ln$eff[i])
        # 02231 -> 02232 predates the change logs; leave that window open.
        if (is.na(from)) from <- as.Date("1900-01-01")
        if (is.na(to))   to   <- as.Date("2999-12-31")
        tibble(geography = ln$gens[[i]], gen = i, valid_from = from, valid_to = to)
      }))
    }))

    county_long <- data_standard %>%
      mutate(time = as.Date(time)) %>%
      tidyr::pivot_longer(cols = starts_with("ahrf_"), names_to = "measure", values_to = "value") %>%
      filter(!is.na(value))

    n_pre <- nrow(county_long)

    ak_rows <- county_long %>% inner_join(validity, by = "geography")
    ak_keep <- ak_rows %>%
      filter(time >= valid_from, time < valid_to) %>%
      # For the undated boundary: if two generations claim a year, take the later.
      group_by(measure, time, valid_from) %>%
      filter(gen == max(gen)) %>%
      ungroup() %>%
      select(-gen, -valid_from, -valid_to)

    county_long <- county_long %>%
      anti_join(validity, by = "geography") %>%
      bind_rows(ak_keep)

    message(
      "Alaska lineage resolved by data year: ", format(nrow(ak_rows), big.mark = ","),
      " rows on lineage geographies -> ", format(nrow(ak_keep), big.mark = ","),
      " kept (", format(n_pre - nrow(county_long), big.mark = ","), " dropped as out-of-era)"
    )

    # Pure counts sum cleanly across counties. Categorical designation codes
    # (HPSA shortage-area status, USDA rural-urban continuum code) have no
    # meaningful sum or average at state/national scale and are left out --
    # they stay county-only. The remaining rate/ratio measures don't have
    # their raw numerator and denominator available separately here, so a
    # population-weighted mean is used as an approximation of the true
    # population-level rate rather than an exact recomputation.
    # Maternal and infant counts are sums too. ahrf_inf_mort_rate is a rate and
    # is left county-only: a state rate needs pooled infant deaths over births,
    # which this approximation can't reproduce.
    AHRF_SUM_MEASURES <- c(
      "ahrf_population", "ahrf_hospitals", "ahrf_critical_access_hosp",
      "ahrf_pcp", "ahrf_md_all", "ahrf_psych", "ahrf_dentists",
      "ahrf_births", "ahrf_births_wh", "ahrf_births_bl", "ahrf_births_hsp",
      "ahrf_births_oth", "ahrf_births_pretrm", "ahrf_births_to_teens",
      "ahrf_low_birth_wgt", "ahrf_very_low_birth_wgt",
      "ahrf_infant_deaths",
      "ahrf_infant_deaths_fem_wh", "ahrf_infant_deaths_fem_bl",
      "ahrf_infant_deaths_fem_hsp", "ahrf_infant_deaths_fem_oth",
      "ahrf_infant_deaths_mal_wh", "ahrf_infant_deaths_mal_bl",
      "ahrf_infant_deaths_mal_hsp", "ahrf_infant_deaths_mal_oth",
      "ahrf_obstetric_hospitals", "ahrf_obstetric_beds",
      "ahrf_neonatal_icu_hospitals", "ahrf_neonatal_icu_beds",
      "ahrf_neonatal_interm_hospitals", "ahrf_neonatal_interm_beds",
      "ahrf_birth_room_hospitals", "ahrf_teen_outreach_hospitals",
      "ahrf_obgyn_nf_md", "ahrf_obgyn_nf_do"
    )
    AHRF_WEIGHTED_MEAN_MEASURES <- c(
      "ahrf_good_air_pct", "ahrf_pm25",
      "ahrf_medicare_per_capita", "ahrf_ed_per_1k_medicare"
    )

    pop_lookup <- county_long %>%
      filter(measure == "ahrf_population") %>%
      select(geography, time, population = value)

    # ahrf_pop_density needs different treatment than the other rate
    # measures: a population-weighted mean of county densities is NOT
    # equivalent to true state/national density (total population / total
    # land area) and systematically overstates it by overweighting dense
    # counties -- e.g. that approach gives California ~1,796 people/sq mi
    # vs. its actual ~254. Land area isn't provided as its own field here,
    # but it can be recovered exactly from the two fields that are (each
    # county's own population / density), then true density is
    # sum(population) / sum(implied area) -- no external land-area source
    # needed, and it reproduces California's real density (~252 vs. ~254)
    # almost exactly.
    density_area_lookup <- county_long %>%
      filter(measure == "ahrf_pop_density", value > 0) %>%
      select(geography, time, density = value) %>%
      inner_join(pop_lookup, by = c("geography", "time")) %>%
      mutate(implied_area = population / density)

    aggregate_ahrf <- function(df, group_cols) {
      sum_part <- df %>%
        filter(measure %in% AHRF_SUM_MEASURES) %>%
        group_by(across(all_of(group_cols)), measure) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
      weighted_part <- df %>%
        filter(measure %in% AHRF_WEIGHTED_MEAN_MEASURES) %>%
        left_join(pop_lookup, by = c("geography", "time")) %>%
        filter(!is.na(population)) %>%
        group_by(across(all_of(group_cols)), measure) %>%
        summarise(value = sum(value * population) / sum(population), .groups = "drop")
      bind_rows(sum_part, weighted_part)
    }

    aggregate_density <- function(df, group_cols) {
      df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          value = sum(population) / sum(implied_area), .groups = "drop"
        ) %>%
        mutate(measure = "ahrf_pop_density")
    }

    state_agg <- bind_rows(
      county_long %>%
        mutate(state_fips = substr(geography, 1, 2)) %>%
        aggregate_ahrf(c("state_fips", "time")) %>%
        rename(geography = state_fips),
      density_area_lookup %>%
        mutate(state_fips = substr(geography, 1, 2)) %>%
        aggregate_density(c("state_fips", "time")) %>%
        rename(geography = state_fips)
    )

    national_agg <- bind_rows(
      aggregate_ahrf(county_long, "time"),
      aggregate_density(density_area_lookup, "time")
    ) %>%
      mutate(geography = "00")

    data_standard <- bind_rows(county_long, state_agg, national_agg) %>%
      mutate(time = format(time, "%Y-%m-%d")) %>%
      tidyr::pivot_wider(names_from = measure, values_from = value) %>%
      arrange(geography, time)

    vroom::vroom_write(data_standard, "standard/data.csv.gz", delim = ",")
    message(
      "Wrote ", nrow(data_standard), " rows (county + state + national) across ",
      length(year_dfs), " editions"
    )
    process$raw_state <- current_hash
    dcf::dcf_process_record(updated = process)
  } else {
    message("No editions processed — process.json not updated.")
  }
}
