fig_effect <- function(path = "compute_canada/2024-07-02-ageclasses2/res") {
    # read files
    v_fl <- list.files(path, pattern = "\\.rds$", full.names = TRUE)
    # create vector of species code from file names
    v_sp <- v_fl |>
        fs::path_ext_remove() |>
        fs::path_file()
    # create data frame (long format)
    l_res_all <- v_fl |> lapply(readRDS)
    d_res_sum <- mapply(
        \(x, y) {
            tmp <- x$BUGSoutput$summary
            out <- cbind(
                data.frame(
                    species_code = y,
                    parameter = row.names(tmp)
                ),
                tmp
            )
            row.names(out) <- NULL
            out |>
                dplyr::mutate(
                    parameter_submodel = dplyr::case_when(
                        grepl("^nb_", parameter) ~ "abundance", 
                        grepl("^pa_", parameter) ~ "presence", 
                        .default = "Other"
                    ), 
                    parameter_category = dplyr::case_when(
                        parameter == "deviance" ~ "deviance",
                        grepl("intercept", parameter) ~ "intercept",
                        grepl("soil", parameter) ~ "soil",
                        grepl("epmatorg", parameter) ~ "soil",
                        grepl("ph$", parameter) ~ "soil",
                        grepl("tmean", parameter) ~ "climate",
                        grepl("cmi", parameter) ~ "climate",
                        grepl("ba$", parameter) ~ "climate",
                        grepl("_l\\[", parameter) ~ "disturbance clearcut",
                        grepl("_lpr\\[", parameter) ~ "disturbance CPRS",
                        grepl("_pl\\[", parameter) ~ "disturbance partial cut",
                        grepl("_b\\[", parameter) ~ "disturbance fire",
                        grepl("_o\\[", parameter) ~ "disturbance outbreak",
                    ),
                    parameter_time_category = dplyr::case_when(
                        grepl("intercept", parameter) ~ "none",
                        grepl("\\[1\\]", parameter) ~ "none",
                        grepl("\\[2\\]", parameter) ~ "0-10",
                        grepl("\\[3\\]", parameter) ~ "10-20",
                        grepl("\\[4\\]", parameter) ~ "20-40",
                        grepl("\\[5\\]", parameter) ~ "40-60",
                        grepl("\\[6\\]", parameter) ~ "60-80",
                        grepl("\\[7\\]", parameter) ~ "80+year",
                        .default = "none"
                    )
                )
        },
        x = l_res_all,
        y = v_sp,
        SIMPLIFY = FALSE
    ) |>
        do.call(what = rbind)

    v_par <- c(
        "soil", "climate", "disturbance clearcut", "disturbance partial cut",
        "disturbance CPRS", "disturbance fire", "disturbance outbreak"
    )

    dir.create("fig", showWarnings = FALSE)
    # create figure
    for (i in c("abundance", "presence")) {
        for (j in v_par) {
            ff = d_res_sum |> 
                dplyr::filter(parameter_submodel == i) |>
                dplyr::filter(parameter_category == j)
            print(i)
            print(j)
            print(dim(ff))
            p <- d_res_sum |> 
                dplyr::filter(parameter_submodel == i) |>
                dplyr::filter(parameter_category == j) |>
                ggplot(aes(x = parameter, y = mean)) +
                geom_point() +
                geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                    width = .1,
                    position = position_dodge(0.05)
                ) + 
                geom_hline(yintercept = 0, linetype = "dashed", color = "#af306f") +
                facet_grid(species_code ~ .)
            fl <- paste0("fig_", i, "_", gsub(" +", "_", j), ".png")
            ggsave(filename = file.path("fig", fl), plot = p, width = 10, height = 20)
        }
    }
}