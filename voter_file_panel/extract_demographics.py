import os
import ujson as json
import pandas as pd
import time

os.environ["PYSPARK_PYTHON"] = "python"

import findspark

findspark.init()
findspark.os.environ["PYSPARK_PYTHON"] = "python"

from pyspark.sql import SparkSession
from pyspark import SparkContext, SparkConf
import pydoop.hdfs as hdfs
from pyspark.sql.types import *
from pyspark.sql.functions import *
from pyspark.sql.window import Window
from pyspark import StorageLevel

if __name__ == "__main__":
    spark = (
        SparkSession.builder.appName("Demographics")
        .config("PYSPARK_PYTHON", "python")
        .config("spark.blacklist.enabled", "true")
        .config("spark.blacklist.killBlacklistedExecutors", "true")
        .config("spark.blacklist.application.fetchFailure.enabled", "true")
        .config("spark.sql.legacy.timeParserPolicy", "LEGACY")
        .getOrCreate()
    )

    tweets = spark.read.json(
        "hdfs://megatron.ccs.neu.edu/user/kjoseph/panel_tweets/*/*",
        schema=StructType.fromJson(json.load(open("../tweet_schema.json"))),
    ).select(
        [
            "created_at",
            "user.id_str",
            "user.location",
            "user.url",
            "user.description",
            "user.name",
            "user.screen_name",
            "user.statuses_count",
            "user.favourites_count",
            "user.followers_count",
            "user.friends_count",
            col("user.created_at").alias("user_created_at"),
            "user.verified",
        ]
    )
    tweets = tweets.withColumn(
        "ts", unix_timestamp(tweets.created_at, "EEE MMM dd HH:mm:ss ZZZZZ yyyy")
    )

    schema = StructType(
        [
            StructField("id_str_panel", StringType(), True),
            StructField("since_id", StringType(), True),
        ]
    )

    panel_id_df = spark.read.csv(
        "hdfs://megatron.ccs.neu.edu/user/kjoseph/latest_sinceid_file.tsv",
        sep="\t",
        header=False,
        schema=schema,
    ).drop("since_id")

    panel_data = (
        spark.read.csv(
            "hdfs://megatron.ccs.neu.edu/user/smccabe/TSmart-cleaner-Oct2017-rawFormat.csv",
            header="true",
            inferSchema="true",
        )
        .select(
            [
                "twProfileID",
                "voterbase_age",
                "voterbase_gender",
                "voterbase_race",
                "vf_party",
                "tsmart_state",
                "tsmart_cd",
                "tsmart_zip",
                "tsmart_county_code",
                "tsmart_partisan_score",
                "voterbase_registration_status",
            ]
        )
        .withColumn(
            "registration_status",
            when(
                col("voterbase_registration_status") == "Registered", lit(1)
            ).otherwise(lit(0)),
        )
        .drop("voterbase_registration_status")
    )

    # TODO: this is a very inefficient join that reproduces the voter file
    # columns unnecessarily
    panel_tweets = tweets.join(
        panel_data, panel_data.twProfileID == tweets.id_str, "left"
    )

    # The final output of the script consists of three elements:
    # 1. a user's twitter ID (used as unique identifier)
    # 2. demographic information (drawn from the voter file)
    # 3. metadata about their twitter activity (drawn from the panel)

    # extract the demographic (voter-file-based) information
    demographics = panel_tweets.groupby(panel_tweets.id_str).agg(
        count("id_str"),
        first("voterbase_age"),
        first("voterbase_gender"),
        first("voterbase_race"),
        first("vf_party"),
        first("tsmart_state"),
        first("tsmart_cd"),
        first("tsmart_zip"),
        first("tsmart_county_code"),
        first("tsmart_partisan_score"),
        first("registration_status"),
        min("ts"),
        max("ts"),
    )

    # extract the twitter-based information. all of these work by
    # extracting information from the user profile embedded in their
    # most recent tweet, which could be quite old.
    window = Window().partitionBy("id_str").orderBy(desc("ts"))

    twitter_metadata = (
        panel_tweets.withColumn("statuses_count", first("statuses_count").over(window))
        .withColumn("favourites_count", first("favourites_count").over(window))
        .withColumn("followers_count", first("followers_count").over(window))
        .withColumn("friends_count", first("friends_count").over(window))
        .withColumn("user_created_at", first("user_created_at").over(window))
        .withColumn("verified", first("verified").over(window))
        .withColumn("screen_name", first("screen_name").over(window))
        .select(
            [
                "id_str",
                "statuses_count",
                "favourites_count",
                "followers_count",
                "friends_count",
                "user_created_at",
                "verified",
                "screen_name",
            ]
        )
    ).dropDuplicates(["id_str"])

    output = demographics.join(twitter_metadata, "id_str", "left")

    output.write.csv("hdfs://megatron.ccs.neu.edu/user/smccabe/demog_v5/", sep="\t")

    spark.stop()
