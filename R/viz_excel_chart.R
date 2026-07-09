

#' Layered-axis time-series line or bar chart written as a native Excel chart
#'
#' Writes an `.xlsx` containing a native (fully editable) Excel chart from a
#' tidy data frame. Supports standard line or column (bar) charts.
#' The chart has a two-level category axis (period inner tier, year outer tier)
#' and works with any period labels: quarters (`Q1`..`Q4`), half-years (`H1`,
#' `H2`), months (`Jan`..`Dec`, `M1`..`M12`), etc. Alternatively, a single-layer
#' axis can be generated.
#'
#' Incomplete periods are flagged by a leading `*` in the period label (e.g.
#' `"*Q1"`, `"*H2"`, `"*Apr"`); for line charts, their line segments are drawn
#' dotted. There may be one or several and they are detected automatically.
#' Missing values (`NA`) in series are cleanly skipped in the chart and left as
#' empty cells in the worksheet.
#'
#' The chart is hand-built as Office Open XML and injected into an
#' \pkg{openxlsx2} workbook, because charting packages cannot emit multi-level
#' axes, per-point dotted segments, chart user-shapes (the average box and
#' footnote), or selective end-point data labels. The package is re-zipped with
#' [utils::zip()] using standard local headers.
#'
#' @param d A data frame containing key axis columns (`year_col` and `period_col`
#'   by default, or `single_layer_col` if `single_layered = TRUE`) and one numeric
#'   column per series. Every column other than the key columns becomes a
#'   series, in order, and its column name is used as the series name.
#' @param file Output path for the `.xlsx` file.
#' @param title,subtitle Chart title and (smaller, italic) subtitle.
#' @param sheet Worksheet name.
#' @param year_col,period_col Names of the outer (year) and inner (period) axis
#'   columns. Looked up by name; the function errors if they are absent.
#' @param colors Optional character vector of series colours (`"4472C4"` or
#'   `"#4472C4"`), recycled to the number of series. Defaults to a built-in
#'   palette. Each series colour is reused in its line/bar, its end-point label
#'   and its average-box bullet.
#' @param ymin,ymax Value-axis limits. If `NULL` they are inferred from the data
#'   as rounded "nice" bounds.
#' @param digits Number of decimals. Values are rounded to `digits`, and both the
#'   data cells and the end-point labels are formatted to show exactly that many
#'   decimals (so e.g. `18` displays as `18.0`). Default `1`.
#' @param show_avg Logical; draw the rounded-rectangle average box. Default `TRUE`.
#' @param avg_label Heading shown inside the average box.
#' @param avg_window Number of trailing *complete* periods averaged per series
#'   (8 quarters = 2 years; use 4 for half-yearly, 12 for monthly). Default `8`.
#' @param add_footnote Logical; if `TRUE` *and* at least one period is incomplete
#'   (`*`), a footnote is added to the chart. If there is no `*` in the data the
#'   footnote is never shown. Default `TRUE`.
#' @param footnote_msg Footnote text, used only when the footnote is shown.
#' @param bullet_type Average-box bullet glyph: `"option1"` (default, Wingdings
#'   diamond U+F076), `"option2"` (circled bullet U+29BF), `"option3"` (filled
#'   circle U+2022), or `"custom"` (use `custom_bullet_hex`).
#' @param custom_bullet_hex Unicode code point (hex, e.g. `"29BF"`, `"25CF"`)
#'   used only when `bullet_type = "custom"`. Default `"29BF"`.
#' @param enable_gridline Logical; draw the faint major gridlines. Default `TRUE`.
#' @param single_layered Logical; if `TRUE`, uses a single-level category axis
#'   based on `single_layer_col` instead of the two-level year/period axis. Default `FALSE`.
#' @param single_layer_col Character; name of the single axis column used when
#'   `single_layered = TRUE`. Default `"time"`.
#' @param if_single_series_disable_legend Logical; if `TRUE` and there is exactly
#'   one series to plot, the chart legend is hidden. Default `TRUE`.
#' @param chart_type Character; the style of chart to render: `"line"` (default)
#'   or `"bar"` (clustered column chart).
#' @param legend_type Character; styling style for end-point data labels:
#'   `"type1"` (default, colored text matching series without border) or
#'   `"type2"` (white rectangular box with colored border and black text).
#' @param legend_on_data_till Numeric/Integer; number of trailing observations
#'   (from the right) on which to display data labels. Can be any number or `Inf`;
#'   automatically bounded between `0` and the length of the dataset. Default `1`.
#' @param add_points Logical; if `TRUE`, adds circular hollow data point markers
#'   to line charts. Default `FALSE`.
#'
#' @return (Invisibly) the normalised path to the written file.
#'
#' @details Requires the \pkg{openxlsx2} package and a system `zip` program on the
#'   `PATH` for [utils::zip()] (bundled with Rtools on Windows). LibreOffice
#'   under-renders the dotted segments; Microsoft Excel renders them correctly.
#'
#' @examples
#' \dontrun{
#' d <- data.frame(
#'   year = c(rep("2024-25", 4), rep("2025-26", 4), "2026-27"),
#'   period = c("Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4","*Q1"),
#'   GVA  = c(6.2,7.1,8.4,7.9,6.6,5.8,7.2,8.1,6.8),
#'   GDP  = c(6.5,7.3,8.6,8.0,6.8,6.0,7.5,8.2,7.0),
#'   check.names = FALSE)
#'
#' # Standard Line Chart with markers and Boxed Labels on last 3 points
#' viz_excel_chart_layered_axis(d, "chart_line.xlsx", title = "Real GVA & GDP",
#'                          subtitle = "(%)", legend_type = "type2",
#'                          legend_on_data_till = 3, add_points = TRUE)
#' }
#'
#' @keywords internal
viz_excel_chart_layered_axis <- function(
    d, file,
    title             = "Time Series",
    subtitle          = "(y-o-y, per cent)",
    sheet             = "Data",
    year_col          = "year",
    period_col        = "period",
    colors            = NULL,
    ymin              = NULL,
    ymax              = NULL,
    digits            = 1L,
    show_avg          = TRUE,
    avg_label         = "Last 2-year Average:",
    avg_window        = 8L,
    add_footnote      = TRUE,
    footnote_msg      = "Note: * denotes incomplete data.",
    bullet_type       = c("option1", "option2", "option3", "custom"),
    custom_bullet_hex = "29BF",
    enable_gridline   = TRUE,
    single_layered    = FALSE,
    single_layer_col  = "time",
    if_single_series_disable_legend = TRUE,
    chart_type        = c("line", "bar"),
    legend_type       = c("type1", "type2"),
    legend_on_data_till = 1,
    add_points        = FALSE) {

  ## ---- validate inputs ----------------------------------------------------
  if (!is.data.frame(d)) stop("`d` must be a data frame.")
  if (nrow(d) < 2L)      stop("`d` must have at least 2 rows.")
  if (!requireNamespace("openxlsx2", quietly = TRUE))
    stop("Package 'openxlsx2' is required.")

  if (isTRUE(single_layered)) {
    if (!single_layer_col %in% names(d))
      stop(sprintf("`d` must contain column '%s'.", single_layer_col))
  } else {
    if (!all(c(year_col, period_col) %in% names(d)))
      stop(sprintf("`d` must contain columns '%s' and '%s'.", year_col, period_col))
  }

  bullet_type <- match.arg(bullet_type)
  chart_type  <- match.arg(chart_type)
  legend_type <- match.arg(legend_type)
  digits      <- as.integer(digits)
  if (is.na(digits) || digits < 0L) stop("`digits` must be a non-negative integer.")
  avg_window  <- as.integer(avg_window)
  if (is.na(avg_window) || avg_window < 1L) stop("`avg_window` must be a positive integer.")

  if (isTRUE(single_layered)) {
    tm     <- as.character(d[[single_layer_col]])
    if(any(c(year_col, period_col) %in% names(d))){
      del_cols <- setdiff(c(year_col, period_col), single_layer_col)
      d <- d[setdiff(names(d), del_cols)]
    }
    snames <- setdiff(names(d), single_layer_col)
  } else {
    yr     <- as.character(d[[year_col]])
    tm     <- as.character(d[[period_col]])
    if(any(c(single_layer_col) %in% names(d))){
      del_cols <- setdiff(single_layer_col, c(year_col, period_col))
      d <- d[setdiff(names(d), del_cols)]
    }
    snames <- setdiff(names(d), c(year_col, period_col))
  }

  ns <- length(snames)
  if (ns < 1L) stop("`d` has no series columns (need at least one beyond the key columns).")
  series <- lapply(snames, function(cn) {
    v <- suppressWarnings(as.numeric(d[[cn]]))
    if (all(is.na(v))) stop(sprintf("Series column '%s' is not numeric.", cn))
    round(v, digits)
  })
  n <- nrow(d)

  ## ---- preprocess legend_on_data_till bounds -----------------------------
  legend_on_data_till <- suppressWarnings(as.numeric(legend_on_data_till)[1L])
  if (is.na(legend_on_data_till) || legend_on_data_till < 0) {
    legend_on_data_till <- 0L
  } else if (is.infinite(legend_on_data_till) || legend_on_data_till > n) {
    legend_on_data_till <- as.integer(n)
  } else {
    legend_on_data_till <- as.integer(legend_on_data_till)
  }

  ## ---- colours, number format, incomplete flags --------------------------
  palette <- c("4472C4","C00000","ED7D31","70AD47","7030A0",
               "FFC000","255E91","9E480E","636363","997300")
  cols <- if (is.null(colors)) palette[((seq_len(ns) - 1L) %% length(palette)) + 1L]
  else rep(colors, length.out = ns)
  cols <- toupper(sub("^#", "", cols))
  fmt  <- if (digits > 0L) paste0("0.", strrep("0", digits)) else "0"

  inc     <- which(startsWith(tm, "*")) - 1L      # 0-based
  has_inc <- length(inc) > 0L

  if (!isTRUE(single_layered)) {
    gs        <- c(TRUE, yr[-1L] != yr[-n])
    yr_sparse <- ifelse(gs, yr, NA_character_)
  }

  ## ---- helpers & chart type config ----------------------------------------
  nm   <- function(x) vapply(x, function(z) sprintf("%g", z), character(1L))
  fnum <- function(x) if (is.na(x)) "N/A" else formatC(round(x, digits), format = "f", digits = digits)
  xesc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  col_letter <- function(i) {
    s <- ""
    while (i > 0L) {
      i <- i - 1L
      s <- paste0(LETTERS[i %% 26L + 1L], s)
      i <- i %/% 26L
    }
    s
  }
  bullet_def <- function(bt, hexin) {
    tbl <- list(option1 = c("Wingdings","f076"), option2 = c("Cambria Math","29bf"), option3 = c("Arial","2022"))
    if (bt %in% names(tbl)) { list(font = tbl[[bt]][1], char = sprintf("&#x%s;", tbl[[bt]][2])) }
    else {
      hx <- tolower(gsub("^(0x|u\\+|#)", "", tolower(hexin)))
      cp <- suppressWarnings(strtoi(hx, 16L))
      if (is.na(cp)) stop("`custom_bullet_hex` is not valid hex: ", hexin)
      list(font = if (cp >= 61440L && cp <= 61695L) "Wingdings" else "Cambria Math",
           char = sprintf("&#x%s;", hx))
    }
  }

  ct_cfg <- switch(chart_type,
                   line = list(
                     open     = '<c:lineChart><c:grouping val="standard"/><c:varyColors val="0"/>',
                     close    = '<c:smooth val="0"/><c:axId val="1903716975"/><c:axId val="1903717391"/></c:lineChart>',
                     ser_sppr = '<a:ln w="28575" cap="rnd"><a:solidFill><a:srgbClr val="###HEX###"/></a:solidFill><a:round/></a:ln><a:effectLst/>',
                     ser_mk   = if (isTRUE(add_points)) '<c:marker><c:symbol val="circle"/><c:size val="5"/><c:spPr><a:solidFill><a:srgbClr val="FFFFFF"/></a:solidFill><a:ln w="19050"><a:solidFill><a:srgbClr val="###HEX###"/></a:solidFill></a:ln></c:spPr></c:marker>###DPTS###' else '<c:marker><c:symbol val="none"/></c:marker>###DPTS###',
                     ser_sm   = '<c:smooth val="0"/>'
                   ),
                   bar = list(
                     open     = '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:varyColors val="0"/>',
                     close    = '<c:gapWidth val="150"/><c:axId val="1903716975"/><c:axId val="1903717391"/></c:barChart>',
                     ser_sppr = '<a:solidFill><a:srgbClr val="###HEX###"/></a:solidFill><a:ln><a:noFill/></a:ln><a:effectLst/>',
                     ser_mk   = '',
                     ser_sm   = ''
                   )
  )

  lt_cfg <- switch(legend_type,
                   type1 = list(
                     sppr  = '<a:noFill/><a:ln><a:noFill/></a:ln><a:effectLst/>',
                     txclr = '<a:srgbClr val="###HEX###"/>'
                   ),
                   type2 = list(
                     sppr  = '<a:solidFill><a:srgbClr val="FFFFFF"/></a:solidFill><a:ln w="15875"><a:solidFill><a:srgbClr val="###HEX###"/></a:solidFill></a:ln><a:effectLst/>',
                     txclr = '<a:srgbClr val="000000"/>'
                   )
  )

  ## ---- value-axis limits --------------------------------------------------
  if (is.null(ymin) || is.null(ymax)) {
    allv <- unlist(series)
    lo <- min(allv, na.rm = TRUE)
    hi <- max(allv, na.rm = TRUE)
    span <- hi - lo
    if (span == 0) span <- 1
    raw <- span / 5
    mag <- 10^floor(log10(raw))
    step <- mag * 10
    for (m in c(1,2,2.5,5,10)) if (raw <= m * mag) {
      step <- m * mag
      break
    }
    if (is.null(ymin)) ymin <- floor(lo / step) * step
    if (is.null(ymax)) ymax <- ceiling(hi / step) * step
  }

  ## ---- per-series averages over last avg_window COMPLETE periods ----------
  comp <- which(!startsWith(tm, "*"))
  win  <- if (length(comp) >= avg_window) utils::tail(comp, avg_window) else comp
  avgs <- vapply(seq_len(ns), function(j) mean(series[[j]][win], na.rm = TRUE), numeric(1L))

  ## ---- caches -------------------------------------------------------------
  qpts <- paste0(sprintf('<c:pt idx="%d"><c:v>%s</c:v></c:pt>', 0:(n-1L), xesc(tm)), collapse = "")
  if (isTRUE(single_layered)) {
    catref  <- sprintf("%s!$A$2:$A$%d", sheet, n + 1L)
    cat_xml <- sprintf('<c:cat><c:strRef><c:f>%s</c:f><c:strCache><c:ptCount val="%d"/>%s</c:strCache></c:strRef></c:cat>', catref, n, qpts)
  } else {
    catref   <- sprintf("%s!$A$2:$B$%d", sheet, n + 1L)
    yi       <- which(gs) - 1L
    ypts     <- paste0(sprintf('<c:pt idx="%d"><c:v>%s</c:v></c:pt>', yi, xesc(yr[gs])), collapse = "")
    catcache <- sprintf('<c:multiLvlStrCache><c:ptCount val="%d"/><c:lvl>%s</c:lvl><c:lvl>%s</c:lvl></c:multiLvlStrCache>', n, qpts, ypts)
    cat_xml  <- sprintf('<c:cat><c:multiLvlStrRef><c:f>%s</c:f>%s</c:multiLvlStrRef></c:cat>', catref, catcache)
  }

  numcache <- function(v) {
    ok  <- !is.na(v)
    idx <- which(ok) - 1L
    p   <- paste0(sprintf('<c:pt idx="%d"><c:v>%s</c:v></c:pt>', idx, nm(v[ok])), collapse = "")
    sprintf('<c:numCache><c:formatCode>%s</c:formatCode><c:ptCount val="%d"/>%s</c:numCache>', fmt, length(v), p)
  }

  ## ---- embedded templates -------------------------------------------------
  head_tpl <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<c:chartSpace xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:c16r2=\"http://schemas.microsoft.com/office/drawing/2015/06/chart\"><c:date1904 val=\"0\"/><c:lang val=\"en-GB\"/><c:roundedCorners val=\"0\"/><mc:AlternateContent xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\"><mc:Choice Requires=\"c14\" xmlns:c14=\"http://schemas.microsoft.com/office/drawing/2007/8/2/chart\"><c14:style val=\"102\"/></mc:Choice><mc:Fallback><c:style val=\"2\"/></mc:Fallback></mc:AlternateContent>",
    "<c:clrMapOvr bg1=\"lt1\" tx1=\"dk1\" bg2=\"lt2\" tx2=\"dk2\" accent1=\"accent1\" accent2=\"accent2\" accent3=\"accent3\" accent4=\"accent4\" accent5=\"accent5\" accent6=\"accent6\" hlink=\"hlink\" folHlink=\"folHlink\"/><c:chart><c:title><c:tx><c:rich><a:bodyPr rot=\"0\" spcFirstLastPara=\"1\" vertOverflow=\"ellipsis\" vert=\"horz\" wrap=\"square\" anchor=\"ctr\" anchorCtr=\"1\"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz=\"1400\" b=\"0\" i=\"0\" u=\"none\" strike=\"noStrike\" kern=\"1200\" spc=\"0\" baseline=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"65000\"/><a:lumOff val=\"35000\"/></a:schemeClr></a:solidFill><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:pPr><a:r><a:rPr lang=\"en-IN\" sz=\"1800\" b=\"1\" i=\"0\" baseline=\"0\"><a:solidFill>",
    "<a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:solidFill><a:effectLst/></a:rPr><a:t>###TITLE###</a:t></a:r><a:br><a:rPr lang=\"en-IN\" sz=\"1800\" b=\"1\" i=\"0\" baseline=\"0\"><a:solidFill><a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:solidFill><a:effectLst/></a:rPr></a:br><a:r><a:rPr lang=\"en-IN\" sz=\"1000\" b=\"0\" i=\"1\" baseline=\"0\"><a:solidFill><a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:solidFill><a:effectLst/></a:rPr><a:t>###SUBTITLE###</a:t></a:r><a:endParaRPr lang=\"en-GB\" sz=\"1000\" b=\"0\" i=\"1\"><a:solidFill><a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:solidFill><a:effectLst/></a:endParaRPr></a:p></c:rich></c:tx><c:overlay val=\"0\"/><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr rot=\"0\" spcFirstLastPara=\"1\" vertOverflow=\"ellipsis\" vert=\"horz\" wrap=\"square\" anchor=\"ctr\" anchorCtr=\"1\"/><a:lstStyle/><a:p><a:pPr>",
    "<a:defRPr sz=\"1400\" b=\"0\" i=\"0\" u=\"none\" strike=\"noStrike\" kern=\"1200\" spc=\"0\" baseline=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"65000\"/><a:lumOff val=\"35000\"/></a:schemeClr></a:solidFill><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:pPr><a:endParaRPr lang=\"en-US\"/></a:p></c:txPr></c:title><c:autoTitleDeleted val=\"0\"/><c:plotArea><c:layout/>###CHART_OPEN###"
  )
  ser_tpl <- paste0(
    "<c:ser><c:idx val=\"###SIDX###\"/><c:order val=\"###SIDX###\"/><c:tx><c:strRef><c:f>###NAMEREF###</c:f><c:strCache><c:ptCount val=\"1\"/><c:pt idx=\"0\"><c:v>###SNAME###</c:v></c:pt></c:strCache></c:strRef></c:tx><c:spPr>###SER_SPPR###</c:spPr>###SER_MK###<c:dLbls>###DLBLS_ITEMS###<c:spPr>###DLBL_SPPR###</c:spPr><c:txPr><a:bodyPr rot=\"0\" spcFirstLastPara=\"1\" vertOverflow=\"ellipsis\" vert=\"horz\" wrap=\"square\" lIns=\"38100\" tIns=\"19050\" rIns=\"38100\" bIns=\"19050\" anchor=\"ctr\" anchorCtr=\"1\"><a:spAutoFit/></a:bodyPr><a:lstStyle/><a:p><a:pPr><a:defRPr sz=\"900\" b=\"0\" i=\"0\" u=\"none\" strike=\"noStrike\" kern=\"1200\" baseline=\"0\"><a:solidFill>###DLBL_TXCLR###</a:solidFill><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:pPr><a:endParaRPr lang=\"en-US\"/></a:p></c:txPr><c:showLegendKey val=\"0\"/><c:showVal val=\"0\"/><c:showCatName val=\"0\"/><c:showSerName val=\"0\"/><c:showPercent val=\"0\"/><c:showBubbleSize val=\"0\"/><c:extLst><c:ext uri=\"{CE6537A1-D6FC-4f65-9D91-7224C49458BB}\" xmlns:c15=\"http://schemas.microsoft.com/office/drawing/2012/chart\"><c15:showLeaderLines val=\"1\"/><c15:leaderLines><c:spPr><a:ln w=\"9525\" cap=\"flat\" cmpd=\"sng\" algn=\"ctr\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"35000\"/><a:lumOff val=\"65000\"/></a:schemeClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr></c15:leaderLines></c:ext></c:extLst></c:dLbls>###CATXML###<c:val><c:numRef><c:f>###VALREF###</c:f>###NUMCACHE###</c:numRef></c:val>###SER_SM###</c:ser>"
  )
  tail_tpl <- paste0(
    "<c:dLbls><c:showLegendKey val=\"0\"/><c:showVal val=\"0\"/><c:showCatName val=\"0\"/><c:showSerName val=\"0\"/><c:showPercent val=\"0\"/><c:showBubbleSize val=\"0\"/></c:dLbls>###CHART_CLOSE###<c:catAx><c:axId val=\"1903716975\"/><c:scaling><c:orientation val=\"minMax\"/></c:scaling><c:delete val=\"0\"/><c:axPos val=\"b\"/>###CATGRID###<c:numFmt formatCode=\"General\" sourceLinked=\"1\"/><c:majorTickMark val=\"out\"/><c:minorTickMark val=\"out\"/><c:tickLblPos val=\"nextTo\"/><c:spPr><a:noFill/><a:ln w=\"9525\" cap=\"flat\" cmpd=\"sng\" algn=\"ctr\"><a:solidFill><a:sysClr val=\"window\" lastClr=\"FFFFFF\"><a:lumMod val=\"50000\"/></a:sysClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr>",
    "<c:txPr><a:bodyPr rot=\"-60000000\" spcFirstLastPara=\"1\" vertOverflow=\"ellipsis\" vert=\"horz\" wrap=\"square\" anchor=\"ctr\" anchorCtr=\"1\"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz=\"900\" b=\"0\" i=\"0\" u=\"none\" strike=\"noStrike\" kern=\"1200\" baseline=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"65000\"/><a:lumOff val=\"35000\"/></a:schemeClr></a:solidFill><a:effectLst><a:glow rad=\"127000\"><a:schemeClr val=\"bg1\"><a:alpha val=\"50000\"/></a:schemeClr></a:glow></a:effectLst><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:pPr><a:endParaRPr lang=\"en-US\"/></a:p></c:txPr>",
    "<c:crossAx val=\"1903717391\"/><c:crosses val=\"autoZero\"/><c:auto val=\"1\"/><c:lblAlgn val=\"ctr\"/><c:lblOffset val=\"100\"/><c:noMultiLvlLbl val=\"0\"/></c:catAx><c:valAx><c:axId val=\"1903717391\"/><c:scaling><c:orientation val=\"minMax\"/><c:max val=\"###YMAX###\"/><c:min val=\"###YMIN###\"/></c:scaling><c:delete val=\"0\"/><c:axPos val=\"l\"/>###VALGRID###<c:numFmt formatCode=\"0\" sourceLinked=\"0\"/><c:majorTickMark val=\"out\"/><c:minorTickMark val=\"none\"/><c:tickLblPos val=\"nextTo\"/><c:spPr><a:noFill/><a:ln><a:solidFill><a:sysClr val=\"window\" lastClr=\"FFFFFF\"><a:lumMod val=\"50000\"/></a:sysClr></a:solidFill></a:ln><a:effectLst/></c:spPr>",
    "<c:txPr><a:bodyPr rot=\"-60000000\" spcFirstLastPara=\"1\" vertOverflow=\"ellipsis\" vert=\"horz\" wrap=\"square\" anchor=\"ctr\" anchorCtr=\"1\"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz=\"900\" b=\"0\" i=\"0\" u=\"none\" strike=\"noStrike\" kern=\"1200\" baseline=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"65000\"/><a:lumOff val=\"35000\"/></a:schemeClr></a:solidFill><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:pPr><a:endParaRPr lang=\"en-US\"/></a:p></c:txPr>",
    "<c:crossAx val=\"1903716975\"/><c:crosses val=\"autoZero\"/><c:crossBetween val=\"between\"/></c:valAx><c:spPr><a:noFill/><a:ln w=\"25400\"><a:noFill/></a:ln><a:effectLst/></c:spPr></c:plotArea>###LEGEND###",
    "<c:plotVisOnly val=\"1\"/><c:dispBlanksAs val=\"gap\"/><c:extLst><c:ext uri=\"{56B9EC1D-385E-4148-901F-78D8002777C0}\" xmlns:c16r3=\"http://schemas.microsoft.com/office/drawing/2017/03/chart\"><c16r3:dataDisplayOptions16><c16r3:dispNaAsBlank val=\"1\"/></c16r3:dataDisplayOptions16></c:ext></c:extLst><c:showDLblsOverMax val=\"0\"/></c:chart><c:spPr><a:solidFill><a:schemeClr val=\"bg1\"/></a:solidFill><a:ln w=\"9525\" cap=\"flat\" cmpd=\"sng\" algn=\"ctr\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"15000\"/><a:lumOff val=\"85000\"/></a:schemeClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/></a:defRPr></a:pPr><a:endParaRPr lang=\"en-US\"/></a:p></c:txPr><c:printSettings><c:headerFooter/><c:pageMargins b=\"0.75\" l=\"0.7\" r=\"0.7\" t=\"0.75\" header=\"0.3\" footer=\"0.3\"/><c:pageSetup/></c:printSettings><c:userShapes r:id=\"rId4\"/></c:chartSpace>"
  )
  avg_head_tpl <- paste0(
    "<cdr:relSizeAnchor xmlns:cdr=\"http://schemas.openxmlformats.org/drawingml/2006/chartDrawing\"><cdr:from><cdr:x>0.50564</cdr:x><cdr:y>0.18232</cdr:y></cdr:from><cdr:to><cdr:x>0.99681</cdr:x><cdr:y>###TOY###</cdr:y></cdr:to><cdr:sp macro=\"\" textlink=\"\"><cdr:nvSpPr><cdr:cNvPr id=\"2\" name=\"TextBox 1\"><a:extLst xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:ext uri=\"{FF2B5EF4-FFF2-40B4-BE49-F238E27FC236}\"><a16:creationId xmlns:a16=\"http://schemas.microsoft.com/office/drawing/2014/main\" id=\"{0D00774E-B57E-D54C-FFD6-A3EA791592AB}\"/></a:ext></a:extLst></cdr:cNvPr><cdr:cNvSpPr txBox=\"1\"/></cdr:nvSpPr>",
    "<cdr:spPr><a:xfrm xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:off x=\"2510735\" y=\"754822\"/><a:ext cx=\"2438839\" cy=\"###CY###\"/></a:xfrm><a:prstGeom xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" prst=\"roundRect\"><a:avLst><a:gd name=\"adj\" fmla=\"val 13074\"/></a:avLst></a:prstGeom><a:ln xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:noFill/></a:ln></cdr:spPr><cdr:txBody><a:bodyPr xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" wrap=\"square\" rtlCol=\"0\"/><a:lstStyle xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:lvl1pPr marL=\"0\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl1pPr>",
    "<a:lvl2pPr marL=\"457200\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl2pPr><a:lvl3pPr marL=\"914400\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl3pPr><a:lvl4pPr marL=\"1371600\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl4pPr><a:lvl5pPr marL=\"1828800\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl5pPr><a:lvl6pPr marL=\"2286000\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl6pPr>",
    "<a:lvl7pPr marL=\"2743200\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl7pPr><a:lvl8pPr marL=\"3200400\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl8pPr><a:lvl9pPr marL=\"3657600\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl9pPr></a:lstStyle>",
    "<a:p xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:r><a:rPr lang=\"en-IN\" b=\"1\" i=\"1\" dirty=\"0\"><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/></a:rPr><a:t>###AVG_LABEL###</a:t></a:r><a:endParaRPr lang=\"en-GB\" sz=\"1200\" dirty=\"0\"><a:latin typeface=\"Times New Roman\" panose=\"02020603050405020304\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Times New Roman\" panose=\"02020603050405020304\" pitchFamily=\"18\" charset=\"0\"/></a:endParaRPr></a:p>"
  )
  bullet_tpl <- r"----(<a:p xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><a:pPr marL="342900" lvl="0" indent="-342900"><a:buFont typeface="###BUFONT###" panose="05000000000000000000" pitchFamily="2" charset="2"/><a:buChar char="###BUCHAR###"/></a:pPr><a:r><a:rPr lang="en-IN" b="1" i="1" dirty="0"><a:solidFill><a:srgbClr val="###HEX###"/></a:solidFill><a:latin typeface="Cambria" panose="02040503050406030204" pitchFamily="18" charset="0"/><a:ea typeface="Cambria" panose="02040503050406030204" pitchFamily="18" charset="0"/></a:rPr><a:t>###BNAME###: ###BVAL###%</a:t></a:r><a:endParaRPr lang="en-GB" sz="1200" dirty="0"><a:solidFill><a:srgbClr val="###HEX###"/></a:solidFill><a:latin typeface="Times New Roman" panose="02020603050405020304" pitchFamily="18" charset="0"/><a:ea typeface="Times New Roman" panose="02020603050405020304" pitchFamily="18" charset="0"/></a:endParaRPr></a:p>)----"
  avg_tail_tpl <- r"----(</cdr:txBody></cdr:sp></cdr:relSizeAnchor>)----"
  foot_tpl <- paste0(
    "<cdr:relSizeAnchor xmlns:cdr=\"http://schemas.openxmlformats.org/drawingml/2006/chartDrawing\"><cdr:from><cdr:x>0.00928</cdr:x><cdr:y>0.93385</cdr:y></cdr:from><cdr:to><cdr:x>0.50045</cdr:x><cdr:y>1</cdr:y></cdr:to><cdr:sp macro=\"\" textlink=\"\"><cdr:nvSpPr><cdr:cNvPr id=\"3\" name=\"TextBox 1\"><a:extLst xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:ext uri=\"{FF2B5EF4-FFF2-40B4-BE49-F238E27FC236}\"><a16:creationId xmlns:a16=\"http://schemas.microsoft.com/office/drawing/2014/main\" id=\"{B2BC695D-7D6B-49F0-715F-6870393C64A1}\"/></a:ext></a:extLst></cdr:cNvPr><cdr:cNvSpPr txBox=\"1\"/></cdr:nvSpPr>",
    "<cdr:spPr><a:xfrm xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:off x=\"51470\" y=\"3679518\"/><a:ext cx=\"2725266\" cy=\"260641\"/></a:xfrm><a:prstGeom xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" prst=\"roundRect\"><a:avLst><a:gd name=\"adj\" fmla=\"val 13074\"/></a:avLst></a:prstGeom><a:ln xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:noFill/></a:ln></cdr:spPr><cdr:txBody><a:bodyPr xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" wrap=\"square\" rtlCol=\"0\"/><a:lstStyle xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:lvl1pPr marL=\"0\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl1pPr>",
    "<a:lvl2pPr marL=\"457200\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl2pPr><a:lvl3pPr marL=\"914400\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl3pPr><a:lvl4pPr marL=\"1371600\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl4pPr><a:lvl5pPr marL=\"1828800\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl5pPr><a:lvl6pPr marL=\"2286000\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl6pPr>",
    "<a:lvl7pPr marL=\"2743200\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl7pPr><a:lvl8pPr marL=\"3200400\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl8pPr><a:lvl9pPr marL=\"3657600\" indent=\"0\"><a:defRPr sz=\"1100\"><a:latin typeface=\"+mn-lt\"/><a:ea typeface=\"+mn-ea\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:lvl9pPr></a:lstStyle>",
    "<a:p xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:r><a:rPr lang=\"en-IN\" sz=\"800\" b=\"1\" i=\"1\" dirty=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"50000\"/><a:lumOff val=\"50000\"/></a:schemeClr></a:solidFill><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/></a:rPr><a:t>###FOOTMSG###</a:t></a:r><a:endParaRPr lang=\"en-GB\" sz=\"800\" dirty=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"50000\"/><a:lumOff val=\"50000\"/></a:schemeClr></a:solidFill><a:latin typeface=\"Times New Roman\" panose=\"02020603050405020304\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Times New Roman\" panose=\"02020603050405020304\" pitchFamily=\"18\" charset=\"0\"/></a:endParaRPr></a:p></cdr:txBody></cdr:sp></cdr:relSizeAnchor>"
  )
  grid_cat <- r"----(<c:majorGridlines><c:spPr><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:solidFill><a:sysClr val="window" lastClr="FFFFFF"><a:lumMod val="50000"/><a:alpha val="5000"/></a:sysClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr></c:majorGridlines>)----"
  grid_val <- r"----(<c:majorGridlines><c:spPr><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:solidFill><a:sysClr val="window" lastClr="FFFFFF"><a:lumMod val="50000"/><a:alpha val="5000"/></a:sysClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr></c:majorGridlines>)----"
  draw1_xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><xdr:twoCellAnchor><xdr:from><xdr:col>6</xdr:col><xdr:colOff>89335</xdr:colOff><xdr:row>2</xdr:row><xdr:rowOff>5915</xdr:rowOff></xdr:from><xdr:to><xdr:col>14</xdr:col><xdr:colOff>151454</xdr:colOff><xdr:row>23</xdr:row><xdr:rowOff>145599</xdr:rowOff></xdr:to><xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr><xdr:cNvPr id=\"4\" name=\"Chart 3\"><a:extLst><a:ext uri=\"{FF2B5EF4-FFF2-40B4-BE49-F238E27FC236}\"><a16:creationId xmlns:a16=\"http://schemas.microsoft.com/office/drawing/2014/main\" id=\"{502BAF1C-E407-2EB2-ABFD-42B82C6B9262}\"/></a:ext></a:extLst></xdr:cNvPr><xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr>",
    "<xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/chart\"><c:chart xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"rId1\"/></a:graphicData></a:graphic></xdr:graphicFrame><xdr:clientData/></xdr:twoCellAnchor></xdr:wsDr>"
  )
  us_open  <- '<c:userShapes xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart">'
  us_close <- '</c:userShapes>'

  ## ---- series XML ---------------------------------------------------------
  dpt_one <- '<c:dPt><c:idx val="%d"/><c:bubble3D val="0"/><c:spPr><a:ln w="28575" cap="rnd"><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:prstDash val="sysDot"/><a:round/></a:ln><a:effectLst/></c:spPr></c:dPt>'
  col_off <- if (isTRUE(single_layered)) 1L else 2L
  build_ser <- function(j) {
    L <- col_letter(col_off + j); hexc <- cols[j]
    dpts <- if (has_inc) paste0(sprintf(dpt_one, inc, hexc), collapse = "") else ""

    lbl_idxs <- if (legend_on_data_till > 0L) seq(from = n - legend_on_data_till, to = n - 1L, by = 1L) else integer(0)
    lbl_idxs <- lbl_idxs[!is.na(series[[j]][lbl_idxs + 1L])]
    dlbl_one <- '<c:dLbl><c:idx val="%d"/><c:layout/><c:showLegendKey val="0"/><c:showVal val="1"/><c:showCatName val="0"/><c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/></c:dLbl>'
    dlbls_items <- paste0(sprintf(dlbl_one, lbl_idxs), collapse = "")

    ser_sppr_val   <- gsub("###HEX###", hexc, ct_cfg$ser_sppr, fixed = TRUE)
    ser_mk_val     <- gsub("###DPTS###", dpts, ct_cfg$ser_mk, fixed = TRUE)
    ser_mk_val     <- gsub("###HEX###", hexc, ser_mk_val, fixed = TRUE)
    dlbl_sppr_val  <- gsub("###HEX###", hexc, lt_cfg$sppr, fixed = TRUE)
    dlbl_txclr_val <- gsub("###HEX###", hexc, lt_cfg$txclr, fixed = TRUE)

    repl <- c("###SIDX###"=as.character(j-1L), "###NAMEREF###"=sprintf("%s!$%s$1",sheet,L),
              "###SNAME###"=xesc(snames[j]), "###HEX###"=hexc,
              "###SER_SPPR###"=ser_sppr_val, "###SER_MK###"=ser_mk_val, "###SER_SM###"=ct_cfg$ser_sm,
              "###DLBLS_ITEMS###"=dlbls_items, "###DLBL_SPPR###"=dlbl_sppr_val, "###DLBL_TXCLR###"=dlbl_txclr_val,
              "###CATXML###"=cat_xml, "###VALREF###"=sprintf("%s!$%s$2:$%s$%d",sheet,L,L,n+1L),
              "###NUMCACHE###"=numcache(series[[j]]))
    s <- ser_tpl
    for (k in names(repl)) s <- gsub(k, repl[[k]], s, fixed = TRUE)
    s
  }
  sers <- paste0(vapply(seq_len(ns), build_ser, character(1L)), collapse = "")

  ## ---- assemble chart XML (with gridline toggle) --------------------------
  legend_xml <- if (isTRUE(if_single_series_disable_legend) && ns == 1L) {
    ""
  } else {
    "<c:legend><c:legendPos val=\"b\"/><c:overlay val=\"0\"/><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr rot=\"0\" spcFirstLastPara=\"1\" vertOverflow=\"ellipsis\" vert=\"horz\" wrap=\"square\" anchor=\"ctr\" anchorCtr=\"1\"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz=\"900\" b=\"0\" i=\"0\" u=\"none\" strike=\"noStrike\" kern=\"1200\" baseline=\"0\"><a:solidFill><a:schemeClr val=\"tx1\"><a:lumMod val=\"65000\"/><a:lumOff val=\"35000\"/></a:schemeClr></a:solidFill><a:latin typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:ea typeface=\"Cambria\" panose=\"02040503050406030204\" pitchFamily=\"18\" charset=\"0\"/><a:cs typeface=\"+mn-cs\"/></a:defRPr></a:pPr><a:endParaRPr lang=\"en-US\"/></a:p></c:txPr></c:legend>"
  }
  tail_xml <- gsub("###LEGEND###", legend_xml, tail_tpl, fixed = TRUE)
  tail_xml <- gsub("###CHART_CLOSE###", ct_cfg$close, tail_xml, fixed = TRUE)
  tail_xml <- gsub("###YMIN###", nm(ymin), gsub("###YMAX###", nm(ymax), tail_xml, fixed = TRUE), fixed = TRUE)
  tail_xml <- gsub("###CATGRID###", if (isTRUE(enable_gridline)) grid_cat else "", tail_xml, fixed = TRUE)
  tail_xml <- gsub("###VALGRID###", if (isTRUE(enable_gridline)) grid_val else "", tail_xml, fixed = TRUE)
  chart_xml <- paste0(gsub("###CHART_OPEN###", ct_cfg$open,
                           gsub("###SUBTITLE###", xesc(subtitle),
                                gsub("###TITLE###", xesc(title), head_tpl, fixed = TRUE), fixed = TRUE), fixed = TRUE),
                      sers, tail_xml)

  ## ---- user-shapes: average box and/or footnote ---------------------------
  need_foot <- isTRUE(add_footnote) && has_inc
  need_us   <- isTRUE(show_avg) || need_foot
  if (need_us) {
    inner <- ""
    if (isTRUE(show_avg)) {
      bd <- bullet_def(bullet_type, custom_bullet_hex)
      nlines <- ns + 1L
      cy  <- as.integer(round(218963 * nlines))
      toy <- round(0.18232 + 0.0529 * nlines, 5)
      ah <- gsub("###TOY###", as.character(toy),
                 gsub("###CY###", as.character(cy),
                      gsub("###AVG_LABEL###", xesc(avg_label), avg_head_tpl, fixed = TRUE), fixed = TRUE), fixed = TRUE)
      bullets <- paste0(vapply(seq_len(ns), function(j) {
        b <- gsub("###BUFONT###", bd$font, bullet_tpl, fixed = TRUE)
        b <- gsub("###BUCHAR###", bd$char, b, fixed = TRUE)
        b <- gsub("###HEX###",    cols[j], b, fixed = TRUE)
        b <- gsub("###BNAME###",  xesc(snames[j]), b, fixed = TRUE)
        gsub("###BVAL###", fnum(avgs[j]), b, fixed = TRUE)
      }, character(1L)), collapse = "")
      inner <- paste0(inner, ah, bullets, avg_tail_tpl)
    }
    if (need_foot) inner <- paste0(inner, gsub("###FOOTMSG###", xesc(footnote_msg), foot_tpl, fixed = TRUE))
    draw2_xml <- paste0(us_open, inner, us_close)
  } else {
    chart_xml <- gsub('<c:userShapes r:id="rId4"/>', '', chart_xml, fixed = TRUE)
  }

  ## ---- 1. data sheet via openxlsx2 (+ cell number format) -----------------
  if (isTRUE(single_layered)) {
    out <- data.frame(tm, stringsAsFactors = FALSE, check.names = FALSE)
    for (j in seq_len(ns)) out[[col_off + j]] <- series[[j]]
    names(out) <- c(single_layer_col, snames)
  } else {
    out <- data.frame(yr_sparse, tm, stringsAsFactors = FALSE, check.names = FALSE)
    for (j in seq_len(ns)) out[[col_off + j]] <- series[[j]]
    names(out) <- c(year_col, period_col, snames)
  }

  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet(sheet) |>
    openxlsx2::wb_add_data(sheet, x = out, col_names = TRUE, na.strings = NULL)
  data_dims <- sprintf("%s2:%s%d", col_letter(col_off + 1L), col_letter(col_off + ns), n + 1L)
  wb <- openxlsx2::wb_add_numfmt(wb, sheet, dims = data_dims, numfmt = fmt)

  file_abs <- normalizePath(file, winslash = "/", mustWork = FALSE)
  openxlsx2::wb_save(wb, file_abs, overwrite = TRUE)

  ## ---- 2. inject chart parts ----------------------------------------------
  tmp <- tempfile("eclax_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(file_abs, exdir = tmp)
  wp <- function(...) file.path(tmp, ...)
  dir.create(wp("xl","charts","_rels"),   showWarnings = FALSE, recursive = TRUE)
  dir.create(wp("xl","drawings","_rels"), showWarnings = FALSE, recursive = TRUE)
  wf <- function(path, txt) writeLines(enc2utf8(txt), wp(path), sep = "", useBytes = TRUE)

  wf(file.path("xl","charts","chart1.xml"),     chart_xml)
  wf(file.path("xl","drawings","drawing1.xml"), draw1_xml)
  wf(file.path("xl","drawings","_rels","drawing1.xml.rels"),
     paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
            '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart" Target="../charts/chart1.xml"/>',
            '</Relationships>'))
  if (need_us) {
    wf(file.path("xl","drawings","drawing2.xml"), draw2_xml)
    wf(file.path("xl","charts","_rels","chart1.xml.rels"),
       paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
              '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
              '<Relationship Id="rId4" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/chartUserShapes" Target="../drawings/drawing2.xml"/>',
              '</Relationships>'))
  }

  shfile <- list.files(wp("xl","worksheets"), pattern = "\\.xml$")[1L]
  dir.create(wp("xl","worksheets","_rels"), showWarnings = FALSE)
  shrels <- wp("xl","worksheets","_rels", paste0(shfile, ".rels"))
  drel <- '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing" Target="../drawings/drawing1.xml"/>'
  if (file.exists(shrels)) {
    rx  <- paste(readLines(shrels, warn = FALSE), collapse = "")
    ids <- as.integer(gsub("\\D", "", regmatches(rx, gregexpr('Id="rId[0-9]+"', rx))[[1L]]))
    rid <- paste0("rId", if (length(ids)) max(ids) + 1L else 1L)
    rx  <- sub("</Relationships>", paste0(sprintf(drel, rid), "</Relationships>"), rx, fixed = TRUE)
    wf(file.path("xl","worksheets","_rels", paste0(shfile, ".rels")), rx)
  } else {
    rid <- "rId1"
    wf(file.path("xl","worksheets","_rels", paste0(shfile, ".rels")),
       paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
              '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
              sprintf(drel, rid), '</Relationships>'))
  }

  shpath <- wp("xl","worksheets", shfile)
  sx <- paste(readLines(shpath, warn = FALSE), collapse = "")
  if (!grepl("xmlns:r=", sx, fixed = TRUE))
    sx <- sub("<worksheet ", '<worksheet xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ', sx, fixed = TRUE)
  if (!grepl("<drawing ", sx, fixed = TRUE))
    sx <- sub("</worksheet>", sprintf('<drawing r:id="%s"/></worksheet>', rid), sx, fixed = TRUE)
  wf(file.path("xl","worksheets", shfile), sx)

  ct <- paste(readLines(wp("[Content_Types].xml"), warn = FALSE), collapse = "")
  ov <- paste0('<Override PartName="/xl/charts/chart1.xml" ContentType="application/vnd.openxmlformats-officedocument.drawingml.chart+xml"/>',
               '<Override PartName="/xl/drawings/drawing1.xml" ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>')
  if (need_us) ov <- paste0(ov, '<Override PartName="/xl/drawings/drawing2.xml" ContentType="application/vnd.openxmlformats-officedocument.drawingml.chartshapes+xml"/>')
  ct <- sub("</Types>", paste0(ov, "</Types>"), ct, fixed = TRUE)
  wf("[Content_Types].xml", ct)

  ## ---- 3. re-zip with standard headers (Excel-safe) -----------------------
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)
  if (file.exists(file_abs)) unlink(file_abs)
  ok <- tryCatch({
    utils::zip(zipfile = file_abs,
               files   = list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE),
               flags   = "-X9q")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  setwd(old)
  if (!isTRUE(ok) || !file.exists(file_abs))
    stop("Re-zipping failed. utils::zip() needs a system 'zip' on PATH ",
         "(install Rtools on Windows, or set R_ZIPCMD).")

  invisible(file_abs)
}

