library(readr)
library(plyr)
library(dplyr)

in_dir <- ""
out_dir <- ""

raw_data_dir2 <- ""
raw_data_dir <- ""


load("pew_histograms_with_standardized_breaks.RData") # pew hists

setwd(in_dir)

df <- read.table(
    "demog_v4.tsv",
    sep = ",",
    colClasses = rep("character", 23),
    header = T
)
names(df) <- c("id","num_tweets","age","gender","race","party","state","cd","zip","county_fips","partisan_score","registration_status","first_tweet","last_tweet","statuses_count","favourites_count","followers_count","friends_count","user_created_at","verified")
df <- subset(df, registration_status == 1)
df$race[df$race=="Uncoded"] <- NA

behavior <- read.table(
    file="behavior.tsv",
    sep = "\t",
    colClasses = rep("character", 7),
    header = F
    )
names(behavior) <- c('id','n_impeachment_tweets','n_retweets','n_impeachment_retweets','n_replies','n_replies_alt','n_tweets')


get_hist <- function(x, log = "yes", .breaks = NULL) {
    if (class(x) != "numeric") {
        x <- as.numeric(as.character(x))
        }
    if (is.null(.breaks)) {
        .breaks <- "Sturges"
    }
    if (log == "yes") {
        x <- log10(x)
        x[!is.finite(x)] <- -1
    }
    if (.breaks != "Sturges") {
    if (max(.breaks) < max(x, na.rm=T)) {
        .breaks[length(.breaks)] <- max(x)
    }
        .breaks[.breaks == 0] <- -0.5
    if (.breaks[1] != -1) {
        .breaks <- c(-1, .breaks)
        }
    }
    ##
    my_hist <- hist(x, breaks = .breaks, right = T, include.lowest=T)
    if (.breaks == "Sturges") {
        my_hist <- hist(x, breaks = c(-1, my_hist$breaks), right = T, include.lowest=T)
        my_hist$mids[1] <- 0
        }
    ##
    my_hist <- data.frame(
        breaks = my_hist$breaks,
        mids = c(NA, my_hist$mids),
        counts = c(NA, my_hist$counts)
    )
    return(my_hist)
}

hist_statuses <- get_hist(
    df$statuses_count,
    .breaks = atp_tweets$breaks
)

hist_favorites <- get_hist(
    df$favourites_count,
    .breaks = atp_likes$breaks
)

hist_followers <- get_hist(
    df$followers_count,
    .breaks = atp_followers$breaks
)

hist_friends <- get_hist(
    df$friends_count,
    .breaks = atp_followings$breaks
)

setwd(out_dir)

save(
    hist_statuses,
    hist_favorites,
    hist_followers,
    hist_friends,
    file = "poq_neu_age_metadata_activity_histograms_replicate.RData"
)



#### VRA and closed primary analysis

closed_primary <- c("CT","DE","FL","KS","KY","ME","MD","DC","NE","NM","NY","PA","WY")
vra <- c("AL","GA","LA","MS","SC","VA","AK","AZ","TX")

round(prop.table(table(df$race)), 2)
round(prop.table(table(subset(df, state %in% vra)$race)), 2)

round(prop.table(table(df$party)), 2)
round(prop.table(table(subset(df, state %in% closed_primary)$party)), 2)
mean(subset(df, state %in% closed_primary)$party %in% c("Independent","No Party","Unaffiliated"))


#### unique name analysis

## read all raw files
thefiles <- paste0(
    raw_data_dir,
    grep(
        "\\.zip",
        list.files(raw_data_dir),
        value=T
    )
)
big_states <- c("ca","tx","fl","ny","il","pa","oh","mi","nc","ga")                     #unzip doesn't work for > 4gb
##
thefiles <- thefiles[!grepl(paste0("_", big_states, collapse = "|"), thefiles)]
##

thefiles2 <- paste0(
    raw_data_dir, ## "unzipped/"),
    grep(
        "\\.csv",
        list.files(raw_data_dir),
        value=T
    )
)
##

data_list <- list()
for (thefile in c(thefiles2)) {
    cat(
        "\n",
        gsub(raw_data_dir, "", thefile),
        "..\n"
    )
    data_list[[thefile]] <- read_delim(
        thefile,
        delim="\t", quote="\"", comment="", trim_ws=T,
        col_types=cols(
            .default=col_skip(),
            tsmart_first_name = col_character(),
            tsmart_last_name = col_character(),
            voterbase_age = col_character(),
            voterbase_gender = col_character(),
            voterbase_race = col_character(),
            vf_party = col_character(),
            tsmart_state = col_character(),
            voterbase_registration_status = col_character()
            )
    )
}
raw_data_registered <- subset(
    ldply(data_list, data.frame),
    voterbase_registration_status == "Registered" # seems to include previously registered
    ) %>% select(-voterbase_registration_status)
nrow(raw_data_registered)

raw_data_registered$voterbase_race[raw_data_registered$voterbase_race=="Uncoded"] <- NA

round(prop.table(table(raw_data_registered$voterbase_race)), 2)
round(prop.table(table(subset(raw_data_registered, tsmart_state %in% vra)$voterbase_race)), 2)

round(prop.table(table(subset(raw_data_registered, tsmart_state %in% closed_primary)$vf_party)), 2)
mean(subset(raw_data_registered, tsmart_state %in% closed_primary)$vf_party %in% c("Independent","No Party","Unaffiliated"))

## age_breaks <- seq(1900, 2000, 5)
age_breaks <- c(18, 30, 50, 65, 150)
raw_data_registered <- raw_data_registered %>%
    mutate(
        age_group = cut(
            as.numeric(voterbase_age),
            breaks=age_breaks,
            right=F
        )
    )

raw_data_registered <- raw_data_registered %>%
        group_by(tsmart_first_name, tsmart_last_name, tsmart_state) %>%
            add_tally() %>%
                rename(n_unique_name = n)

round(prop.table(table(subset(raw_data_registered, n_unique_name == 1 & tsmart_state %in% closed_primary)$vf_party)), 2)
mean(subset(raw_data_registered, n_unique_name == 1 & tsmart_state %in% closed_primary)$vf_party %in% c("Independent","No Party","Unaffiliated"))

round(prop.table(table(subset(raw_data_registered, n_unique_name == 1 & tsmart_state %in% vra)$voterbase_race)), 2)

prop.table(table(subset(raw_data_registered, n_unique_name == 1)$voterbase_gender))
prop.table(table(subset(raw_data_registered, n_unique_name == 1)$voterbase_age_group))
