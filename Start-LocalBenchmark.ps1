Write-Host "[Compile]"
cargo build --release --bin tessoku-book-c20
Move-Item ../target/release/tessoku-book-c20.exe . -Force
Write-Host "[Run]"
$env:DURATION_MUL = "0.6"
dotnet marathon run-local