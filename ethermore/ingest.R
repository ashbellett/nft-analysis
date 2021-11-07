library("dplyr")
library("rjson")
library("tidyr")

extract_metadata <- function(json, key) {
    data <- fromJSON(json)
    frame <- data.frame(
        token_url=data$external_url,
        token_image=data$image,
        token_rarity_score=as.numeric(data$attributes[[9]]$value),
        token_rarity_rank=strtoi(data$attributes[[10]]$value),
        attribute_alignment=as.factor(data$attributes[[1]]$value),
        attribute_race=as.factor(data$attributes[[2]]$value),
        attribute_race_variant=as.factor(data$attributes[[3]]$value),
        attribute_class=as.factor(data$attributes[[4]]$value),
        attribute_sub_class=as.factor(data$attributes[[5]]$value),
        attribute_title=as.factor(data$attributes[[6]]$value),
        attribute_background=as.factor(data$attributes[[7]]$value),
        attribute_item=as.factor(data$attributes[[8]]$value)
    )
    frame$key <- key
    return(frame)
}

token_owners <- read.csv(
    paste0(Sys.getenv("DATA_RAW"), Sys.getenv("TOKEN_OWNERS")),
    colClasses=c(token_address="character", metadata="character")
) %>%
    select(token_id, token_address, metadata) %>%
    mutate(token_id=strtoi(token_id))

token_ownership <- read.csv(
    paste0(Sys.getenv("DATA_RAW"), Sys.getenv("TOKEN_OWNERSHIP")),
    colClasses=c(owner_of="character", count="integer")
) %>%
    select(owner_of, count)

token_transactions <- read.csv(
    paste0(Sys.getenv("DATA_RAW"), Sys.getenv("TOKEN_TRANSACTIONS")),
    colClasses=c(
        EVENT_FROM="character",
        EVENT_TO="character",
        PLATFORM_FEE="numeric",
        PRICE="numeric",
        PRICE_USD="numeric",
        TX_CURRENCY="character",
        BLOCK_TIMESTAMP="Date"
    )) %>%
    select(
        TOKEN_ID,
        EVENT_FROM,
        EVENT_TO, PRICE,
        PRICE_USD,
        PLATFORM_FEE,
        TX_CURRENCY,
        BLOCK_TIMESTAMP
    ) %>%
    rename(
        token_id=TOKEN_ID,
        event_from=EVENT_FROM,
        event_to=EVENT_TO,
        price=PRICE,
        price_usd=PRICE_USD,
        platform_fee=PLATFORM_FEE,
        currency=TX_CURRENCY,
        timestamp=BLOCK_TIMESTAMP
    ) %>%
    mutate(token_id=strtoi(token_id))

twitter_summary <- read.csv(
    paste0(Sys.getenv("DATA_RAW"), Sys.getenv("TWITTER_SUMMARY")),
    colClasses=c(
        date="Date",
        followers="integer",
        following="integer",
        tweets="integer",
        favorites="integer"
    )) %>%
    select(date, followers, following, tweets, favorites)

twitter_followers <- read.csv(
    paste0(Sys.getenv("DATA_RAW"), Sys.getenv("TWITTER_FOLLOWERS")),
    colClasses=c(
        screen_name="character",
        follower_count="integer"
    )) %>%
    select(screen_name, follower_count)

twitter_tweets <- read.csv(paste0(Sys.getenv("DATA_RAW"), Sys.getenv("TWITTER_TWEETS")),
    colClasses=c(
        created_at="Date",
        text="character",
        favorite_count="integer",
        retweet_count="integer"
    )) %>%
    select(created_at, text, favorite_count, retweet_count)

token_value <- token_transactions %>%
    select(
        token_id,
        price_usd
    ) %>%
    group_by(token_id) %>%
    summarise(
        transaction_count=n(),
        transaction_mean=mean(price_usd, na.rm=TRUE)
    )

tokens <- merge(x=token_value, y=token_owners, by="token_id")%>%
    select(
        token_id,
        transaction_count,
        transaction_mean,
        metadata
    )

tokens_full <- drop_na(tokens)

token_metadata <- mapply(
    extract_metadata,
    tokens_full$metadata,
    tokens_full$token_id,
    SIMPLIFY=FALSE
    )

token_metadata <- data.table::rbindlist(token_metadata)

tokens <- merge(
    x=tokens,
    y=token_metadata,
    by.x="token_id",
    by.y="key",
    all.x=TRUE
) %>%
    subset(select=-c(metadata))

write.csv(
    tokens,
    paste0(Sys.getenv("DATA_RESULTS"), Sys.getenv("TOKENS")),
    row.names=FALSE
)
