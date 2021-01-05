import numpy as np
import pandas as pd

# each account must have a file called "{account}_followers.txt" with one
# id per line
ACCOUNTS = ["aoc", "senatedems", "senategop", "housedemocrats", "housegop", "tedcruz"]


def main():
    SCHEMA = {
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
        "first_tweet": np.int64,
        "last_tweet": np.int64,
        "statuses_count": np.float64,
        "favourites_count": np.float64,
        "followers_count": np.float64,
        "friends_count": np.float64,
        "user_created_at": "str",
        "verified": bool,
    }

    df = pd.read_csv(
        "../demog/demog_v3.tsv",
        header=None,
        sep="\t",
        names=SCHEMA.keys(),
        dtype=SCHEMA,
    )
    # df = df.loc[:, "id"]

    print(df.head())

    for account in ACCOUNTS:
        with open(f"{account}_followers.txt", "r") as fin:
            followers = set([x.strip() for x in fin.readlines()])
            df[account] = df["id"].isin(followers).astype(int)

    df = df.loc[:, ["id"] + ACCOUNTS]
    df.to_csv("followings.tsv", sep="\t", index=False)


if __name__ == "__main__":
    main()
