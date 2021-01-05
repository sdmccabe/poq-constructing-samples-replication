import os
import ujson as json
import pandas as pd
import time
import re
import requests
import string
import itertools as it

os.environ["PYSPARK_PYTHON"] = "python3"

import findspark

findspark.init()
findspark.os.environ["PYSPARK_PYTHON"] = "python3"

from pyspark.sql import SparkSession
from pyspark import SparkContext, SparkConf
import pydoop.hdfs as hdfs
from pyspark.sql.types import *
from pyspark.sql.functions import *
from pyspark.sql.window import Window
from pyspark import StorageLevel
from pyspark.ml.feature import StopWordsRemover, Tokenizer, RegexTokenizer
from definitions import OTHER_DATA_DIR

START_DATE = "2019-10-01"
END_DATE = "2019-12-01"

## STOP WORDS
# choice of Glasgow or Snowball
# STOP_WORDS = requests.get('http://ir.dcs.gla.ac.uk/resources/linguistic_utils/stop_words').text.split()
STOP_WORDS = requests.get(
    "https://raw.githubusercontent.com/igorbrigadir/stopwords/master/en/snowball_original.txt"
).text.split()

## REGEXES
# remove punctuation, keep emoji, hashtags, and at-mentions.
EMOJI_REGEXP = r"""(\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff])"""
# IMPEACHMENT_REGEXP = r"""^(?:#)?(?:trump)?[Ii][Mm][Pp][Ee][Aa][Cc][Hh](?:ment|ed|proceedings|hearings|inquiry|trump|now|eve|witchunt|witchhunt|march)*"""
PUNCT_REGEXP = r"""[…’‘—“”]"""
RETWEET_REGEXP = re.compile(r"""^RT @""")
REPLY_REGEXP = re.compile(r"""^@[\w\d_]+""")
# Define the function you want to return
def extract(s):
    if "impeach" in s.lower():
        return 1
    else:
        return 0


# Create the UDF, note that you need to declare the return schema matching the returned type
extract_udf = udf(extract, IntegerType())


def is_rt(s):
    if RETWEET_REGEXP.match(s):
        return 1
    return 0


rt_udf = udf(is_rt, IntegerType())


def is_reply(s):
    if REPLY_REGEXP.match(s):
        return 1
    return 0


reply_udf = udf(is_reply, IntegerType())


if __name__ == "__main__":
    spark = (
        SparkSession.builder.appName("Impeachment")
        .config("PYSPARK_PYTHON", "python")
        .config("spark.blacklist.enabled", "true")
        .config("spark.blacklist.killBlacklistedExecutors", "true")
        .config("spark.blacklist.application.fetchFailure.enabled", "true")
        .getOrCreate()
    )

    read = spark.read.json(
        "hdfs://megatron.ccs.neu.edu/user/kjoseph/panel_tweets/2019_*/*",
        schema=StructType.fromJson(json.load(open(f"{OTHER_DATA_DIR}/tweet_schema.json"))),
    ).select(
        [
            "created_at",
            "user.id_str",
            "full_text",
            "retweeted_status",
            "in_reply_to_status_id",
        ]
    )

    # filter on date range and create day-level variable
    read = read.withColumn(
        "ts", to_timestamp(read.created_at, "EEE MMM dd HH:mm:ss ZZZZZ yyyy")
    )
    read = read.withColumn("date", date_trunc("day", read.ts))

    dates = (START_DATE, END_DATE)
    date_from, date_to = [to_date(lit(s)).cast(TimestampType()) for s in dates]
    tweets = read.where((read.ts >= date_from) & (read.ts < date_to))

    tweets = (
        tweets.withColumn("retweet", rt_udf(tweets.full_text))
        .withColumn("impeachment", extract_udf(tweets.full_text))
        .withColumn(
            "impeachment_retweet",
            when((col("retweet") == 1) & (col("impeachment") == 1), lit(1)).otherwise(
                lit(0)
            ),
        )
        .withColumn(
            "reply",
            when(col("in_reply_to_status_id").isNotNull(), lit(1)).otherwise(lit(0)),
        )
        .withColumn("reply_alt", reply_udf(tweets.full_text))
        .drop("retweeted_status", "full_text", "created_at", "in_reply_to_status_id")
    )

    tweets = tweets.groupby("id_str").agg(
        sum(col("impeachment")),
        sum(col("retweet")),
        sum(col("impeachment_retweet")),
        sum(col("reply")),
        sum(col("reply_alt")),
        count(col("*")),
    )
    panel_data = spark.read.csv(
        "hdfs://megatron.ccs.neu.edu/user/smccabe/TSmart-cleaner-Oct2017-rawFormat.csv",
        header="true",
        inferSchema="true",
    ).select(["twProfileID"])

    panel_tweets = tweets.join(
        panel_data, panel_data.twProfileID == tweets.id_str, "inner"
    ).drop("twProfileID")

    panel_tweets.write.csv(
        "hdfs://megatron.ccs.neu.edu/user/smccabe/behavior", sep="\t"
    )

    spark.stop()
