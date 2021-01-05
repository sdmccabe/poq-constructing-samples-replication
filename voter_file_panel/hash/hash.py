import hashlib
import re
import sys

NON_DIGITS = re.compile("\D+")

def main():
    output = []
    with open(sys.argv[1], "r") as fin:
        for line in fin.readlines():
            line = line.strip()
            line = NON_DIGITS.sub("", line)
            salted_line = line + sys.argv[2]
            encoded_line = salted_line.encode("utf-8")
            hashed_line = hashlib.sha256(encoded_line)
            digest = hashed_line.hexdigest()
            output.append(f"{line}\t{digest}")
    sorted_output = sorted(output)
    with open(sys.argv[3], "w") as fout:
        fout.write("\n".join(output) + "\n")
    return
 

if __name__ == "__main__":
    if len(sys.argv) != 4:
        raise ValueError("usage: python hash_v2.py <input file> <salt> <output file>")

    main()
