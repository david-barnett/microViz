library(phyloseq)
data("enterotype", package = "phyloseq")

local_edition(3)

x <- enterotype
y <- data.frame(
  ID_var = sample_names(enterotype)[c(1:50, 101:150)],
  SeqTech = sample_data(enterotype)[c(1:50, 101:150), "SeqTech"],
  arbitrary_info = rep(c("A", "B"), 50)
)

for (j in c("full", "inner", "left", "anti", "semi")) {
  test_that(paste(j, "join stays the same"), {
    expect_snapshot(
      ps_join(x = x, y = y, match_sample_names = "ID_var", type = j)
    )
  })
}

test_that("right join equivalent to left join", {
  lefty <-
    ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var", "SeqTech"), type = "left") %>%
    ps_select(-Sample_ID)

  righty <-
    ps_join(y = x, x = y, by = c("ID_var" = "Sample_ID", "SeqTech"), type = "right") %>%
    ps_select(dplyr::all_of(sample_variables(lefty)))

  expect_equal(lefty, righty)
})
