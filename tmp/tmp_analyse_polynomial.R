lsf <- list.files(
    "compute_canada/2024-06-04-gaussian/res",
    pattern = "rds$",
    full.names = TRUE
)

l_res <- list()
i <- 0
for (f in lsf) {
    spc <- f |>
        fs::path_file() |>
        fs::path_ext_remove()
    hh <- l_res[[fs::path_file(f) |> fs::path_ext_remove()]] <- readRDS(f)

    cli::cli_alert_info(spc)
    png(
        file.path("tmp", "fig_time_norm", paste0(spc, ".png")),
        width = 10, height = 18, units = "in", res = 300
    )
    par(mfcol = c(5, 2))
    for (i in c("nb", "pa")) {
        for (j in c("b", "o", "l", "lpr", "pl")) {
            ls_par <- get_means(hh,
                perturb = j, type = i,
                var = c("coef1", "coef2", "coef3")
            )
            print(c(ls_par, title = paste0(i, " -- ", j)))
            do.call(what = plot_effect_polynomial, c(ls_par, main = paste0(i, " -- ", j)))
        }
    }
    dev.off()
}

hh <- lsf[[1]] |> readRDS()
par(mfcol = c(5, 2))
for (i in c("nb", "pa")) {
    for (j in c("b", "o", "l", "lpr", "pl")) {
        ls_par <- get_means(hh,
            perturb = j, type = i,
            var = c("coef1", "coef2", "coef3")
        )
        print(c(ls_par, title = paste0(i, " -- ", j)))

        do.call(what = plot_effect_polynomial, c(ls_par, main = paste0(i, " -- ", j)))
    }
}
