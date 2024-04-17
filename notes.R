col <- c("red", "yellow", "blue", "green")
n <- c(1, 2, 3, 4)
cat <- c("warm", " ", "cool", " ")

df <- tibble(col, n, cat)

df2 <- df %>% 
  mutate(cat2 = case_when(
    
    col == "red" | col == "yellow" ~ "warm",
    col == "blue" | col == "green" ~ "cool"))

y <- c(1, 2, NA, NA, 6)
z <- c(NA, NA, 3, 4, 5)
coalesce(y, z)

stuff <- links %>% 
  group_by(parasitic) %>% 
  count()


# 873 links > 1383
# 501 nodes > 544