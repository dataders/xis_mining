#plotting



g.all <- all %>% filter(GRADE.LEVEL == 6) %>%
        mutate(
                Age = as.period(interval(start = BIRTH.DATE,
                                         end = today())),
                Years.at.XIS = as.period(interval(start = ENTRY.DAY.1,
                                                  end = today())))
ind <- paste("career_pct")
dep <- paste("Math_Percentile")

thing <- lm(career_pct ~ Math_Percentile, all)

ggplot(all, aes(x=career_pct, y=Math_Percentile)) +
        geom_point() +    # Use hollow circles
        scale_fill_brewer( palette = "Blues") + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    fullrange=TRUE) + # Extend regression lines
        geom_text(x = 200000000, y = 255,
                  label = lm_eqn(thing), parse = TRUE) + 
        facet_wrap(~GRADE.LEVEL)

ggsave("figures/lang_rit_bygrade.png", width = 6, height = 6)

lm_eqn = function(m) {
        
        l <- list(a = format(coef(m)[1], digits = 2),
                  b = format(abs(coef(m)[2]), digits = 2),
                  r2 = format(summary(m)$r.squared, digits = 3));
        
        if (coef(m)[2] >= 0)  {
                eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        } else {
                eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
        }
        
        as.character(as.expression(eq));                 
}