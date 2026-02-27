@echo off
setlocal

echo [OEM] Configuring OpenSSH Server...
powershell -NoLogo -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference = 'Stop';" ^
  "Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0 | Out-Null;" ^
  "Set-Service -Name sshd -StartupType Automatic;" ^
  "Start-Service -Name sshd;" ^
  "if (-not (Get-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -ErrorAction SilentlyContinue)) { New-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -DisplayName 'OpenSSH Server (sshd)' -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort 22 | Out-Null };" ^
  "New-Item -Path 'HKLM:\SOFTWARE\OpenSSH' -Force | Out-Null;" ^
  "New-ItemProperty -Path 'HKLM:\SOFTWARE\OpenSSH' -Name 'DefaultShell' -Value 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe' -PropertyType String -Force | Out-Null;"
if errorlevel 1 (
  echo [OEM] OpenSSH configuration failed.
  exit /b 1
)

echo [OEM] OpenSSH is ready.
exit /b 0
