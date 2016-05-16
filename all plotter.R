#plotting



g.all <- all %>% filter(GRADE.LEVEL == 6) %>%
        mutate(
                Age = as.period(interval(start = BIRTH.DATE,
                                         end = today())),
                Years.at.XIS = as.period(interval(start = ENTRY.DAY.1,
                                                  end = today())))
ind <- paste("career_pct")
dep <- paste("Math_Percentile")

all.4countries <- all %>% filter(NATIONALITY == "Taiwan" | NATIONALITY == "Korean" | NATIONALITY =="HK" | NATIONALITY == "USA")

thing <- lm(Math_RITScore ~ avg.CriMean.t2, all)
browser()
ggplot(all, aes(x=Math_RITScore, y=avg.CriMean.t2)) +
        geom_point() +    # Use hollow circles
        #scale_fill_brewer( palette = "Blues") + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    fullrange=TRUE) + # Extend regression lines
        geom_text(x = 8, y = 6, label = lm_eqn(thing), parse = TRUE)

+
        facet_wrap(~GRADE.LEVEL)

ggsave("figures/mathRIT-T2CriMean-bygrade.png", width = 6, height = 6)
