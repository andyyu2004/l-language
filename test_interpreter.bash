path="tests/test_scripts"

tests=("adt" "general" "recursion" "scoping" "tuple" "record" "lambda")

for test in "${tests[@]}"; do
    echo "testing $test..."
    cargo run --release ${path}/"${test}"_tests.l > ${path}/"${test}"_output.txt
    cmp --silent ${path}/"${test}"_output.txt ${path}/"${test}"_expected.txt || echo "${test} test failed"; diff ${path}/"${test}"_output.txt ${path}/"${test}"_expected.txt
done

