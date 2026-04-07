bibtex_2academic <- function(bibfile,
                             outfold,
                             abstract = FALSE,
                             overwrite = FALSE) {
  require(RefManageR)
  require(dplyr)
  require(stringr)
  require(anytime)

  # Import the bibtex file and convert to data.frame
  mypubs <- ReadBib(bibfile, check = "warn", .Encoding = "UTF-8") %>%
    as.data.frame()

  # Preserve the BibTeX citekey (row names) as a column for use in filenames
  mypubs$citekey <- rownames(mypubs)

  # Assign Hugo publication_types based on BibTeX entry type
  mypubs <- mypubs %>%
    dplyr::mutate(
      pubtype = dplyr::case_when(
        bibtype == "Article"          ~ "2",
        bibtype == "Article in Press" ~ "2",
        bibtype == "InProceedings"    ~ "1",
        bibtype == "Proceedings"      ~ "1",
        bibtype == "Conference"       ~ "1",
        bibtype == "Conference Paper" ~ "1",
        bibtype == "MastersThesis"    ~ "3",
        bibtype == "PhdThesis"        ~ "3",
        bibtype == "Manual"           ~ "4",
        bibtype == "TechReport"       ~ "4",
        bibtype == "Book"             ~ "5",
        bibtype == "InCollection"     ~ "6",
        bibtype == "InBook"           ~ "6",
        bibtype == "Misc"             ~ "0",
        TRUE                          ~ "0"
      )
    )

  # Helper: read existing URL fields from an .md file so we can preserve them
  # when overwriting (prevents manual edits from being lost on re-runs)
  read_existing_urls <- function(filepath) {
    defaults <- list(
      url_pdf      = "",
      url_preprint = "",
      url_code     = ""
    )
    if (!file.exists(filepath)) return(defaults)

    lines <- readLines(filepath, warn = FALSE)
    for (field in names(defaults)) {
      pattern <- paste0("^", field, "\\s*=\\s*\"(.*)\"")
      match   <- regmatches(lines, regexpr(pattern, lines, perl = TRUE))
      if (length(match) > 0) {
        defaults[[field]] <- sub(pattern, "\\1", match[1], perl = TRUE)
      }
    }
    defaults
  }

  # Main function: create a single .md file for one publication
  create_md <- function(x) {
    # Build date string
    if (!is.na(x[["year"]])) {
      x[["date"]] <- paste0(x[["year"]], "-01-01")
    } else {
      x[["date"]] <- "2999-01-01"
    }

    # FIX 1: Use the BibTeX citekey in the filename instead of a 20-char title
    # snippet. This avoids collisions when two papers share the same opening
    # words (e.g., two versions of the same paper at preprint vs. published).
    filename <- paste0(x[["date"]], "_", x[["citekey"]], ".md")

    if (!file.exists(file.path(outfold, filename)) | overwrite) {

      # FIX 2: Read back any URL fields the user may have edited manually so
      # that overwriting the file doesn't discard them.
      existing_urls <- read_existing_urls(file.path(outfold, filename))

      # FIX 3: Use booktitle for conference/proceedings entries, not just journal.
      # Articles use 'journal'; inproceedings/conference entries use 'booktitle'.
      venue <- x[["journal"]]
      if (is.na(venue) || venue == "") {
        venue <- x[["booktitle"]]
      }
      if (is.na(venue)) venue <- ""

      # Build the full publication string
      publication <- venue
      if (!is.na(x[["volume"]]) && x[["volume"]] != "")
        publication <- paste0(publication, ", (", x[["volume"]], ")")
      if (!is.na(x[["pages"]]) && x[["pages"]] != "")
        publication <- paste0(publication, ", _pp. ", x[["pages"]], "_")
      if (!is.na(x[["doi"]]) && x[["doi"]] != "")
        publication <- paste0(publication, ", https://doi.org/", x[["doi"]])

      # Helper: collapse newlines and escape internal quotes — both are
      # illegal inside TOML basic strings
      clean_str <- function(s) {
        s <- gsub("[\r\n]+", " ", s)   # newlines → space
        s <- gsub('"', '\\\\"', s)     # " → \"
        trimws(s)
      }

      fileConn <- file.path(outfold, filename)
      write("+++", fileConn)

      # Title and date
      write(paste0("title = \"", clean_str(x[["title"]]), "\""), fileConn, append = TRUE)
      write(paste0("date = \"", anydate(x[["date"]]), "\""), fileConn, append = TRUE)

      # Authors — keep "First Last" order, transliterate accented characters
      auth_hugo <- str_replace_all(x["author"], " and ", "\", \"")
      auth_hugo <- stringi::stri_trans_general(auth_hugo, "latin-ascii")
      write(paste0("authors = [\"", auth_hugo, "\"]"), fileConn, append = TRUE)

      # Publication type
      write(paste0("publication_types = [\"", x[["pubtype"]], "\"]"),
            fileConn, append = TRUE)

      # Venue / journal / proceedings
      write(paste0("publication = \"", clean_str(publication), "\""), fileConn, append = TRUE)
      write(paste0("publication_short = \"", clean_str(publication), "\""), fileConn, append = TRUE)

      # Abstract
      if (abstract && !is.na(x[["abstract"]])) {
        write(paste0("abstract = \"", clean_str(x[["abstract"]]), "\""), fileConn, append = TRUE)
      } else {
        write("abstract = \"\"", fileConn, append = TRUE)
      }
      write("abstract_short = \"\"", fileConn, append = TRUE)

      write("image_preview = \"\"", fileConn, append = TRUE)
      write("selected = false",      fileConn, append = TRUE)
      write("projects = []",         fileConn, append = TRUE)
      write("tags = []",             fileConn, append = TRUE)

      # FIX 2 (continued): Write back whatever URLs were already set,
      # falling back to empty strings for new files.
      write(paste0("url_pdf = \"",      existing_urls$url_pdf,      "\""), fileConn, append = TRUE)
      write(paste0("url_preprint = \"", existing_urls$url_preprint, "\""), fileConn, append = TRUE)
      write(paste0("url_code = \"",     existing_urls$url_code,     "\""), fileConn, append = TRUE)
      write("url_dataset = \"\"",  fileConn, append = TRUE)
      write("url_project = \"\"",  fileConn, append = TRUE)
      write("url_slides = \"\"",   fileConn, append = TRUE)
      write("url_video = \"\"",    fileConn, append = TRUE)
      write("url_poster = \"\"",   fileConn, append = TRUE)
      write("url_source = \"\"",   fileConn, append = TRUE)
      write("math = true",         fileConn, append = TRUE)
      write("highlight = true",    fileConn, append = TRUE)
      write("[header]",            fileConn, append = TRUE)
      write("image = \"\"",        fileConn, append = TRUE)
      write("caption = \"\"",      fileConn, append = TRUE)
      write("+++",                 fileConn, append = TRUE)
    }
  }

  apply(mypubs, FUN = function(x) create_md(x), MARGIN = 1)
}

# FIX 4: Point to the bib file committed in the repo (relative path from the
# project root) so GitHub Actions — and teammates — can run this without
# needing OneDrive. Keep your OneDrive copy as the master; just also commit
# updates to this path before pushing.
my_bibfile <- "content/publication_list/LifeWork.bib"
outfold    <- "content/publication"

bibtex_2academic(
  my_bibfile,
  outfold,
  abstract  = TRUE,
  overwrite = TRUE
)
