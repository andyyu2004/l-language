path="tests/test_scripts"

tests=("adt" "general")

for test in "${tests[@]}"; do
    echo "testing $test..."
    cargo run ${path}/"${test}"_tests.l > ${path}/"${test}"_output.txt
    cmp --silent ${path}/"${test}"_output.txt ${path}/"${test}"_expected.txt || echo "ADT Failed" && diff ${path}/"${test}"_output.txt ${path}/"${test}"_expected.txt
done

