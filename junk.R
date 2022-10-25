edat <- read_tsv(c(adults[3]),
                 col_types = cols_only(ID = col_integer(),
                                       TrialId = col_integer(),
                                       CursorX = col_integer(),
                                       CursorY = col_integer(),
                                       TimestampSec = col_integer(),
                                       TimestampMicrosec = col_integer(),
                                       UserDefined_1 = col_character()),
                 id = "filename") %>%
  mutate(sub_id = substr(filename, 20, 22) %>% as.integer(),
         sec = TimestampSec + TimestampMicrosec / 1000000) %>%
  select(sub_id, TrialId, ID,
         TimestampSec, TimestampMicrosec,
         sec, CursorX, CursorY,
         UserDefined_1)


#### Calculate probabilities

```{r}
#| label: probs-exist
#| eval: false
compute_probs <- function(x) {
  x %>%
    unnest(data) %>%
    count(group, crit, f_c, role, name = "Y", .drop = FALSE) %>%
    group_by(group, crit, f_c) %>%
    mutate(N = sum(Y),
           p = Y / N) %>% 
    ungroup()
}

bootstrap <- function(x) {
  ## x is a nested dataset
  bs_child <- x %>%
    filter(group == "child") %>%
    slice_sample(n = nrow(.), replace = TRUE)
  
  bs_adult <- x %>%
    filter(group == "adult") %>%
    slice_sample(n = nrow(.), replace = TRUE)
  
  bind_rows(bs_adult, bs_child)  
}

trial_cond <- trials %>%
  inner_join(stimuli, "iv_id") %>%
  inner_join(subjects, "sub_id") %>%
  select(sub_id, group, t_id, ctype, crit)

prob_sub <- pog_cts %>%
  inner_join(trial_cond, c("sub_id", "t_id")) %>% 
  count(sub_id, group, crit, f_c, role,
        name = "Y", .drop = FALSE) %>% 
  group_by(sub_id, group, crit, f_c) %>%
  mutate(N = sum(Y),
         p = Y/N) %>%
  ungroup()

prob_all <- prob_sub %>%
  group_by(group, crit, f_c, role, .drop = FALSE) %>%
  summarize(p = mean(p), .groups = "drop")

ggplot(prob_all, aes(f_c, p, colour = role)) +
  geom_line() +
  facet_grid(group ~ crit)

# pe_nest <- prob_sub %>%
#   nest(data = c(-group, -sub_id))

## probs_exist <- compute_probs(pe_nest)

## pe_nest %>%
##  bootstrap() %>%
##  compute_probs()

# pe_nest %>%
#   bootstrap() %>%
#   pull(sub_id) %>%
#   unique() %>%
#   length()

# n_mcarlo <- 10L
# bs_pexist <- map_dfr(seq_len(n_mcarlo), 
#                      function(.x) {
#                        pe_nest %>%
#                          bootstrap() %>%
#                          compute_probs() %>%
#                          mutate(bs_samp = .x)
#   })
```

#### Calculate bootstrapped confidence intervals

```{r}

```


#### Plot by condition

```{r}
#| label: plot-probs-exist-cond
# ggplot(probs_exist %>% filter(role != "(blank)"),
#        aes(f_c, p, colour = role)) +
#   geom_line() +
#   facet_grid(group~crit)
```

#### Plot by role

```{r}
#| label: plot-probs-exist-role

# ggplot(probs_exist, 
#        aes(f_c, p, colour = crit)) +
#   geom_line() +
#   facet_grid(group~role) +
#   theme(legend.position = "top")
```


### Novel competitors

#### Calculate probabilities

```{r}
#| label: probs-novel

# probs_novel <- trial_cond %>%
#   filter(ctype == "novel") %>%
#   mutate(crit = fct_relevel(crit, "untrained", 
#                             "competitor-day2", 
#                             "competitor-day1")) %>%
#   inner_join(pog_cts, c("sub_id", "t_id")) %>% 
#   count(group, crit, f_c, role, name = "Y", .drop = FALSE) %>% 
#   group_by(group, crit, f_c) %>%
#   mutate(N = sum(Y),
#          p = Y / N) %>%
#   ungroup()
```

#### Plot

