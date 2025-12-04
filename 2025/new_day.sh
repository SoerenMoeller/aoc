#!/bin/bash
set -e

day=$(printf "%02d" "$1")

src="src/day$day.rs"
input="inputs/day$day.txt"
main="src/main.rs"

# --- portable sed append that auto-escapes slashes ---
sed_append() {
    local pattern="$1"
    local text="$2"
    local file="$3"

    # Escape / in pattern for sed
    local escaped_pattern
    escaped_pattern=$(printf "%s" "$pattern" | sed 's/\//\\\//g')

    if sed --version >/dev/null 2>&1; then
        # GNU sed
        sed -i "/$escaped_pattern/a $text" "$file"
    else
        # BSD sed
        sed -i '' "/$escaped_pattern/a \\
$text" "$file"
    fi
}

# 1. Create source + input files
if [ ! -f "$src" ]; then
    cat > "$src" <<EOF
pub fn run() {
    let input = std::fs::read_to_string("inputs/day$day.txt").unwrap();
    println!("Day $day – Part 1: {}", part1(&input));
    println!("Day $day – Part 2: {}", part2(&input));
}

fn part1(input: &str) -> i64 {
    0
}

fn part2(input: &str) -> i64 {
    0
}
EOF
    echo "Created $src"
else
    echo "$src already exists"
fi

touch "$input"
echo "Created $input"

# 2. Insert `mod dayXX;` under the `// imports` marker
if ! grep -q "mod day$day;" "$main"; then
    sed_append "// imports" "mod day$day;" "$main"
    echo "Added mod day$day; to $main"
fi

# 3. Insert match arm under the `// days` marker
if ! grep -q "$day =>" "$main"; then
    sed_append "// days" "        $((10#$day)) => day$day::run()," "$main"
    echo "Added match arm for day $day"
fi

echo "Day $day setup complete."
