#!/bin/bash

INPUT_DATA_FOLDER="/net/data-backedup/twitter-voters/voter-data/targetsmart_oct2017/unzipped/"
NUM_CORES=16
RESERVE_MEM="33%"
OUTPUT_FOLDER="/net/data/twitter-voters/pew_collab_data/external/vf_histograms"

# histogram takes a sample awk call (passed through GNU parallel), runs the awk script in parallel,
# sorts the output, and outputs the counts.
# the GNU parallel bit makes the interpretation a little trickier. note that the awk call must end with "%",
# representing the input file. symbols (including the dollar sign) must be escaped, unlike with normal awk.
function histogram () {
	find $INPUT_DATA_FOLDER -name "*.csv" | parallel -I% -j $NUM_CORES --max-args 1 --lb $1 | sort -S $RESERVE_MEM --parallel=$NUM_CORES | uniq -c | sed "s/^\s*//"
}

function histogram_closed_primary_states () {
	find $INPUT_DATA_FOLDER -regextype posix-egrep -regex ".*(?:CT|DE|FL|KS|KY|ME|MD|DC|NE|NM|NY|PA|WY)\.csv" -name "*.csv" | parallel -I% -j $NUM_CORES --max-args 1 --lb $1 | sort -S $RESERVE_MEM --parallel=$NUM_CORES | uniq -c | sed "s/^\s*//"
}

[ ! -f $OUTPUT_FOLDER/county.txt ] && histogram "awk -F '\t' 'FNR>1{print \$8 \" \" \$25}' %" > $OUTPUT_FOLDER/county.txt
[ ! -f $OUTPUT_FOLDER/age_reg.txt ] && histogram "awk -F '\t' 'FNR>1{print \$8}' %" > $OUTPUT_FOLDER/state.txt
[ ! -f $OUTPUT_FOLDER/partisan_score.txt ] && histogram "awk -F '\t' 'FNR>1{print \$83}' %" > $OUTPUT_FOLDER/partisan_score.txt
[ ! -f $OUTPUT_FOLDER/party.txt ] && histogram "awk -F '\t' 'FNR>1{print \$23}' %" > $OUTPUT_FOLDER/party.txt
[ ! -f $OUTPUT_FOLDER/age.txt ] && histogram "awk -F '\t' 'FNR>1{print \$20}' %" > $OUTPUT_FOLDER/age.txt
[ ! -f $OUTPUT_FOLDER/race.txt ] && histogram "awk -F '\t' 'FNR>1{print \$22}' %" > $OUTPUT_FOLDER/race.txt
[ ! -f $OUTPUT_FOLDER/gender.txt ] && histogram "awk -F '\t' 'FNR>1{print \$21}' %" > $OUTPUT_FOLDER/gender.txt
[ ! -f $OUTPUT_FOLDER/registration_status.txt ] && histogram "awk -F '\t' 'FNR>1{print \$18}' %" > $OUTPUT_FOLDER/registration_status.txt
[ ! -f $OUTPUT_FOLDER/county_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$8 \" \" \$25}' %" > $OUTPUT_FOLDER/county_RV.txt
[ ! -f $OUTPUT_FOLDER/age_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$20}' %" > $OUTPUT_FOLDER/age_RV.txt
[ ! -f $OUTPUT_FOLDER/race_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$22}' %" > $OUTPUT_FOLDER/race_RV.txt
[ ! -f $OUTPUT_FOLDER/gender_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$21}' %" > $OUTPUT_FOLDER/gender_RV.txt
[ ! -f $OUTPUT_FOLDER/state_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$8}' %" > $OUTPUT_FOLDER/state_RV.txt
[ ! -f $OUTPUT_FOLDER/partisan_score_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$83}' %" > $OUTPUT_FOLDER/partisan_score_RV.txt
[ ! -f $OUTPUT_FOLDER/party_RV.txt ] && histogram "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$23}' %" > $OUTPUT_FOLDER/party_RV.txt

[ ! -f $OUTPUT_FOLDER/party_closed_pimary_RV.txt ] && histogram_closed_primary_states "awk -F '\t' 'FNR>1{if (\$18==\"Registered\") print \$23}' %" > $OUTPUT_FOLDER/party_closed_primary_RV.txt
[ ! -f $OUTPUT_FOLDER/party_closed_primary.txt ] && histogram_closed_primary_states "awk -F '\t' 'FNR>1{print \$23}' %" > $OUTPUT_FOLDER/party_closed_primary.txt
