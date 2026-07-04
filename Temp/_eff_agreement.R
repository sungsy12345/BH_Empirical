## Inter-rater agreement on EFFICIENCY (space + time) between Anthony and Mark, 69-student main study.
g <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/2input_data/01studentlab_scores/main/Grading/"
options(stringsAsFactors = FALSE)

read_anth <- function(f) {
  d <- read.csv(paste0(g, f), check.names = FALSE)
  d <- d[, 1:4]; names(d) <- c("name","read","time","space")
  d$N <- as.integer(sub("^q[12]_", "", d$name))
  d <- d[!is.na(d$N), ]
  ord <- function(x) c("No solution"=0, "Suboptimal"=1, "Optimal"=2)[trimws(x)]
  d$time_o <- ord(d$time); d$space_o <- ord(d$space)
  d
}
read_mark <- function(f) {
  d <- read.csv(paste0(g, f), check.names = FALSE)
  names(d)[1:4] <- c("name","read","time","space")
  d$N <- as.integer(d$name); d <- d[!is.na(d$N), ]
  d$time <- suppressWarnings(as.numeric(d$time))
  d$space <- suppressWarnings(as.numeric(d$space))
  d
}

run <- function(q) {
  a <- read_anth(sprintf("Anthony_%s_readability_efficiency.csv", q))
  m <- read_mark(sprintf("Mark_%s.csv", q))
  j <- merge(a[, c("N","time_o","space_o")], m[, c("N","time","space")], by = "N")
  j <- j[complete.cases(j), ]
  cat(sprintf("\n===== %s  (matched n = %d) =====\n", q, nrow(j)))
  for (dim in c("space","time")) {
    av <- j[[paste0(dim,"_o")]]; mv <- j[[dim]]
    cat(sprintf("  %-6s : Pearson r = %.2f | Spearman rho = %.2f | Anthony SD = %.2f (lvls: %s) | Mark SD = %.2f\n",
      toupper(dim),
      suppressWarnings(cor(av, mv)),
      suppressWarnings(cor(av, mv, method = "spearman")),
      sd(av), paste(sort(unique(av)), collapse="/"), sd(mv)))
  }
  cat("  Anthony space level counts: "); print(table(j$space_o))
  cat("  Mark    space dist:          "); print(table(j$space))
}
for (q in c("Q1","Q2")) run(q)
