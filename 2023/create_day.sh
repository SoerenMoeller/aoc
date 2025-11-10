#!/bin/zsh

if [[ -z "$1" ]]; then
  echo "Usage: $0 <day>"
  exit 1
fi

DAY=$(printf "%02d" $1) 
CABAL_FILE="aoc2023.cabal"
BASE_FILE="base.hs"
DAY_FOLDER="day$DAY"

# Check if the Cabal file exists
if [[ ! -f "$CABAL_FILE" ]]; then
  echo "Error: File '$CABAL_FILE' not found."
  exit 1
fi

# Check if the base .hs file exists
if [[ ! -f "$BASE_FILE" ]]; then
  echo "Error: File '$BASE_FILE' not found."
  exit 1
fi

# Use sed to prepend the folder to input paths
sed -i.bak \
    -e "s|let inputPath *= *\"input.txt\"|let inputPath = \"$DAY_FOLDER/input.txt\"|" \
    -e "s|let inputPathExample *= *\"input-example.txt\"|let inputPathExample = \"$DAY_FOLDER/input-example.txt\"|" \
    "$BASE_FILE"

# Define the new executable entry
NEW_EXECUTABLE=$(cat <<EOF
executable day$DAY
  main-is: Main.hs
  hs-source-dirs: day$DAY
  build-depends:
      base >=4.14 && <5,
      aoc2023
  default-language: Haskell2010
EOF
)

# Insert the new executable entry above the marker line
ESCAPED_EXECUTABLE=$(echo "$NEW_EXECUTABLE" | sed ':a;N;$!ba;s/\n/\\n/g')
sed -i "/-- Add executables here/i\\
$ESCAPED_EXECUTABLE\\
" "$CABAL_FILE"

# Print success message
echo "Executable entry for day $DAY added to $CABAL_FILE."

# create folder and files
mkdir -p "$DAY_FOLDER"
mv base.hs "$DAY_FOLDER/Main.hs"
mv base.hs.bak base.hs
touch "day$DAY/input.txt"
touch "day$DAY/input-example.txt"
