param(
    [Parameter(mandatory)]
    [int]
    $seed
)

$in = ".\data\in\in{0:000}.txt" -f $seed
$env:DURATION_MUL = "0.6"
Get-Content $in | cargo run --bin tessoku-book-c20 --release > out.txt
