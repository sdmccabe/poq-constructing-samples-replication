import os
import numpy as np

DATA_DIR = "/net/data/twitter-voters/pew_collab_data"
SPARK_DATA_DIR = f"{DATA_DIR}/from_spark"
EXTERNAL_DATA_DIR = f"{DATA_DIR}/external"
OTHER_DATA_DIR = f"{DATA_DIR}/other"
INTERMEDIATE_DATA_DIR = f"{DATA_DIR}/intermediate"
VF_HISTOGRAMS_DIR = f"{EXTERNAL_DATA_DIR}/vf_histograms"

DEMOG_FILE = f"{SPARK_DATA_DIR}/demog_v4.tsv"
DEMOG_SCHEMA = {
    "id": "str",
    "num_tweets": np.int64,
    "age": np.float64,
    "gender": "str",
    "race": "str",
    "party": "str",
    "state": "str",
    "cd": np.float64,
    "zip": "str",
    "county_fips": "str",
    "partisan_score": np.float64,
    "registration_status": np.float64,
    "first_tweet": np.int64,
    "last_tweet": np.int64,
    "statuses_count": np.float64,
    "favourites_count": np.float64,
    "followers_count": np.float64,
    "friends_count": np.float64,
    "user_created_at": "str",
    "verified": bool,
}

BEHAVIOR_FILE = f"{SPARK_DATA_DIR}/behavior.tsv"
BEHAVIOR_SCHEMA = {
    'id': 'str',
    'n_impeachment_tweets': np.int64,
    'n_retweets': np.int64,
    'n_impeachment_retweets': np.int64,
    'n_replies': np.int64,
    'n_replies_alt': np.int64,
    'n_tweets': np.int64,
}

WEIGHTS_FILE = f"{INTERMEDIATE_DATA_DIR}/voters_demo_weights.csv"
WEIGHTS_SCHEMA = {"id": "str", "weight": np.float64}

FIPS_MAPPING_FILE = f"{EXTERNAL_DATA_DIR}/state_fips_mapping.tsv"

FOLLOWINGS_FILE = f"{INTERMEDIATE_DATA_DIR}/followings.tsv"
FOLLOWINGS_SCHEMA = {
    "id": str,
    "aoc": np.int64,
    "senatedems": np.int64,
    "senategop": np.int64,
    "housedemocrats": np.int64,
    "housegop": np.int64,
    "tedcruz": np.int64,
}

WRU_IMPUTATIONS_FILE = f"{INTERMEDIATE_DATA_DIR}/wru_imputations_tract.tsv"

URBAN_CODES_FILE = f"{EXTERNAL_DATA_DIR}/rural_urban_codes_omb_2013.csv"
ZILLOW_FILE = f"{EXTERNAL_DATA_DIR}/Zip_MedianValuePerSqft_AllHomes.csv"

PARTY_CODINGS = {
    "Unaffiliated": "Independent",
    "Democrat": "Democrat",
    "Republican": "Republican",
    np.nan: "Unknown/Other",
    "No Party": "Independent",
    "Independent": "Independent",
    "Other": "Unknown/Other",
    "Libertarian": "Unknown/Other",
    "Green": "Unknown/Other",
    "Conservative": "Unknown/Other",
    "Working Fam": "Unknown/Other",
    "Unknown": "Unknown/Other",
}

