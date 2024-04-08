hh <- readRDS("ACERUB.rds")

lsf <- list.files(
    "compute_canada/2024-04-08/output",
    pattern = "rds$",
    full.names = TRUE
)

for (f in lsf) {
    spc <- f |>
        fs::path_file() |>
        fs::path_ext_remove()
    hh <- readRDS(f)

    cli::cli_alert_info(spc)
    png(
        file.path("fig_time", paste0(spc, ".png")),
        width = 10, height = 18, units = "in", res = 300
    )
    par(mfcol = c(5, 2))
    for (i in c("nb", "pa")) {
        for (j in c("b", "o", "l", "lpr", "pl")) {
            ls_par <- get_means(hh,
                perturb = j, type = i,
                var = c("eff", "peak", "var")
            )
            print(c(ls_par, title = paste0(i, " -- ", j)))

            do.call(what = plot_effect, c(ls_par, main = paste0(i, " -- ", j)))
        }
    }
    dev.off()
}

hh <- readRDS("ACERUB.rds")

par(mfcol = c(5, 2))
for (i in c("nb", "pa")) {
    for (j in c("b", "o", "l", "lpr", "pl")) {
        ls_par <- get_means(hh,
            perturb = j, type = i,
            var = c("eff", "peak", "var")
        )
        print(c(ls_par, title = paste0(i, " -- ", j)))

        do.call(what = plot_effect, c(ls_par, main = paste0(i, " -- ", j)))
    }
}
