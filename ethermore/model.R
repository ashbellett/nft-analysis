library("data.table")
library("dplyr")

row_names_to_column <- function(data) {
    names <- rownames(data)
    rownames(data) <- NULL
    results <- cbind(names, data)
    return(results)
}

tokens <- read.csv(
    paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("TOKENS")),
    colClasses=c(
        transaction_mean="numeric",
        token_rarity_score="numeric",
        attribute_alignment="factor",
        attribute_race="factor",
        attribute_race_variant="factor",
        attribute_class="factor",
        attribute_sub_class="factor",
        attribute_title="factor",
        attribute_background="factor",
        attribute_item="factor"
    )) %>%
    select(
        token_id,
        transaction_mean,
        token_rarity_score,
        token_rarity_rank,
        attribute_alignment,
        attribute_race,
        attribute_race_variant,
        attribute_class,
        attribute_sub_class,
        attribute_title,
        attribute_background,
        attribute_item
    ) %>%
    mutate(
        token_id=strtoi(token_id),
        token_rarity_rank=strtoi(token_rarity_rank)
    )

model_alignment <- lm(
    formula=scale(transaction_mean) ~ attribute_alignment,
    data=tokens
) %>%
    coef() %>%
    as.data.frame() %>%
    setDT(keep.rownames=TRUE)
colnames(model_alignment) <- c("alignment", "coefficient")
model_alignment$alignment <- substring(model_alignment$alignment, 20)
model_alignment[-c(1),] %>%
    write.csv(
        paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("MODEL_ALIGNMENT")),
        row.names=FALSE
    )

model_race <- lm(
    formula=scale(transaction_mean) ~ attribute_race,
    data=tokens
) %>%
    coef() %>%
    as.data.frame() %>%
    setDT(keep.rownames=TRUE)
colnames(model_race) <- c("race", "coefficient")
model_race$race <- substring(model_race$race, 15)
model_race[-c(1),] %>%
    write.csv(
        paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("MODEL_RACE")),
        row.names=FALSE
    )

model_class <- lm(
    formula=scale(transaction_mean) ~ attribute_class,
    data=tokens
) %>%
    coef() %>%
    as.data.frame() %>%
    setDT(keep.rownames=TRUE)
colnames(model_class) <- c("class", "coefficient")
model_class$class <- substring(model_class$class, 16)
model_class[-c(1),] %>%
    write.csv(
        paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("MODEL_CLASS")),
        row.names=FALSE
    )

model_title <- lm(
    formula=scale(transaction_mean) ~ attribute_title,
    data=tokens
) %>%
    coef() %>%
    as.data.frame() %>%
    setDT(keep.rownames=TRUE)
colnames(model_title) <- c("title", "coefficient")
model_title$title <- substring(model_title$title, 16)
model_title[-c(1),] %>%
    write.csv(
        paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("MODEL_TITLE")),
        row.names=FALSE
    )

model_background <- lm(
    formula=scale(transaction_mean) ~ attribute_background,
    data=tokens
) %>%
    coef() %>%
    as.data.frame() %>%
    setDT(keep.rownames=TRUE)
colnames(model_background) <- c("background", "coefficient")
model_background$background <- substring(model_background$background, 21)
model_background[-c(1),] %>%
    write.csv(
        paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("MODEL_BACKGROUND")),
        row.names=FALSE
    )

model_item <- lm(
    formula=scale(transaction_mean) ~ attribute_item,
    data=tokens
) %>%
    coef() %>%
    as.data.frame() %>%
    setDT(keep.rownames=TRUE)
colnames(model_item) <- c("item", "coefficient")
model_item$item <- substring(model_item$item, 15)
model_item[-c(1),] %>%
    write.csv(
        paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("MODEL_ITEM")),
        row.names=FALSE
    )
