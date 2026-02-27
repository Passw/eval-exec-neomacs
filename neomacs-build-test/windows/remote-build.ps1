[CmdletBinding()]
param(
    [ValidateSet("rust-only", "full")]
    [string]$Mode = "rust-only",

    [string]$SourcePath = "\\host.lan\Data",

    [string]$WorkDir = "C:\Users\Docker\work\neomacs",

    [string]$LogPath = "C:\Users\Docker\neomacs-build.log"
)

$ErrorActionPreference = "Stop"

function Write-Step {
    param([string]$Message)
    $stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    Write-Host "[$stamp] $Message"
}

function Require-Command {
    param(
        [string]$Name,
        [string]$Hint
    )

    if (-not (Get-Command $Name -ErrorAction SilentlyContinue)) {
        throw "Missing required command '$Name'. $Hint"
    }
}

function Ensure-Rust {
    if (Get-Command "cargo" -ErrorAction SilentlyContinue) {
        return
    }

    Write-Step "cargo not found, installing Rust via rustup"
    $rustupExe = Join-Path $env:TEMP "rustup-init.exe"

    Invoke-WebRequest -Uri "https://win.rustup.rs/x86_64" -OutFile $rustupExe
    & $rustupExe -y --profile minimal --default-toolchain stable
    if ($LASTEXITCODE -ne 0) {
        throw "rustup installation failed with exit code $LASTEXITCODE"
    }

    $cargoBin = Join-Path $env:USERPROFILE ".cargo\bin"
    if (Test-Path $cargoBin) {
        $env:Path = "$cargoBin;$env:Path"
    }

    if (-not (Get-Command "cargo" -ErrorAction SilentlyContinue)) {
        throw "cargo is still unavailable after rustup install."
    }
}

function Ensure-Msys2 {
    $bashPath = "C:\msys64\usr\bin\bash.exe"
    if (Test-Path $bashPath) {
        return
    }

    Require-Command "winget" "winget is required to bootstrap MSYS2 in this VM."
    Write-Step "Installing MSYS2 via winget"

    & winget install --id MSYS2.MSYS2 --exact --source winget --accept-source-agreements --accept-package-agreements --disable-interactivity --silent
    if ($LASTEXITCODE -ne 0) {
        throw "MSYS2 installation failed with exit code $LASTEXITCODE"
    }

    if (-not (Test-Path $bashPath)) {
        throw "MSYS2 install completed but $bashPath is still missing."
    }
}

function Ensure-GnuLinker {
    Ensure-Msys2

    $bashPath = "C:\msys64\usr\bin\bash.exe"
    Write-Step "Installing MinGW UCRT64 GCC toolchain"
    & $bashPath -lc "pacman --noconfirm -Sy --needed mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-pkgconf"
    if ($LASTEXITCODE -ne 0) {
        throw "MinGW toolchain installation failed with exit code $LASTEXITCODE"
    }

    $ucrtBin = "C:\msys64\ucrt64\bin"
    if (Test-Path $ucrtBin) {
        $env:Path = "$ucrtBin;$env:Path"
    }

    if (-not (Get-Command "gcc.exe" -ErrorAction SilentlyContinue)) {
        throw "gcc.exe is still unavailable after MinGW install."
    }
}

function Ensure-RustGnuToolchain {
    Write-Step "Installing Rust GNU toolchain"
    & rustup toolchain install stable-x86_64-pc-windows-gnu
    if ($LASTEXITCODE -ne 0) {
        throw "rustup GNU toolchain install failed with exit code $LASTEXITCODE"
    }
}

function Convert-ToMsysPath {
    param([string]$Path)

    $normalized = $Path -replace "\\", "/"
    if ($normalized -match "^([A-Za-z]):/(.*)$") {
        $drive = $Matches[1].ToLowerInvariant()
        $tail = $Matches[2]
        return "/$drive/$tail"
    }

    return $normalized
}

function Copy-Source {
    param(
        [string]$From,
        [string]$To
    )

    if (-not (Test-Path $From)) {
        throw "Source path '$From' is not reachable. Verify the compose bind mount and that \\host.lan\Data is accessible."
    }

    if (Test-Path $To) {
        Remove-Item -Path $To -Recurse -Force
    }
    New-Item -ItemType Directory -Path $To -Force | Out-Null
    Write-Step "Copying source from $From to $To"

    & robocopy $From $To /MIR /XD ".git" "neomacs-build-test" /R:2 /W:2 /NFL /NDL /NJH /NJS /NP | Out-Host
    $code = $LASTEXITCODE
    if ($code -ge 8) {
        throw "robocopy failed with exit code $code"
    }
}

function Invoke-RustOnlyBuild {
    param([string]$Path)

    Ensure-Rust
    Ensure-GnuLinker
    Ensure-RustGnuToolchain

    Push-Location $Path
    try {
        Write-Step "Running rust-only build smoke test for neomacs-display"
        & cargo +stable-x86_64-pc-windows-gnu build --release --target x86_64-pc-windows-gnu --manifest-path "rust/neomacs-display/Cargo.toml" --no-default-features --features "neo-term"
        if ($LASTEXITCODE -ne 0) {
            throw "cargo build failed with exit code $LASTEXITCODE"
        }
    }
    finally {
        Pop-Location
    }
}

function Invoke-FullBuild {
    param([string]$Path)

    Require-Command "bash" "Install an MSYS2-like toolchain and ensure bash is in PATH."
    $msysPath = Convert-ToMsysPath -Path $Path

    Push-Location $Path
    try {
        Write-Step "Running full build attempt through bash/autotools"
        $command = "set -euo pipefail; cd '$msysPath'; ./autogen.sh; ./configure --with-neomacs --with-neovm-core-backend=emacs-c; make -j4"
        & bash -lc $command
        if ($LASTEXITCODE -ne 0) {
            throw "full build failed with exit code $LASTEXITCODE"
        }
    }
    finally {
        Pop-Location
    }
}

$exitCode = 0
Start-Transcript -Path $LogPath -Force | Out-Null

try {
    Write-Step "Mode: $Mode"
    Write-Step "Source: $SourcePath"
    Write-Step "Work directory: $WorkDir"

    Require-Command "robocopy" "robocopy is required and should be available on Windows."
    Copy-Source -From $SourcePath -To $WorkDir

    if ($Mode -eq "rust-only") {
        Invoke-RustOnlyBuild -Path $WorkDir
    }
    else {
        Invoke-FullBuild -Path $WorkDir
    }

    Write-Step "Build test completed successfully."
}
catch {
    $exitCode = 1
    Write-Error $_
    Write-Step "Build test failed."
}
finally {
    Stop-Transcript | Out-Null
}

exit $exitCode
