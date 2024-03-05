# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

$LATEST_VERSION=Get-ChildItem -Path "C:\Program Files (x86)\Intel\oneAPI\compiler\" -Name | Select-String -NotMatch latest | %{$_.Line} | Sort-Object | Select-Object -Last 1

Remove-Item "C:\Program Files (x86)\Intel\oneAPI\compiler\$LATEST_VERSION\windows\compiler\lib\ia32_win" -Force  -Recurse -ErrorAction SilentlyContinue
Remove-Item "C:\Program Files (x86)\Intel\oneAPI\compiler\$LATEST_VERSION\windows\bin\intel64_ia32" -Force  -Recurse -ErrorAction SilentlyContinue
Remove-Item "C:\Program Files (x86)\Intel\oneAPI\compiler\$LATEST_VERSION\windows\lib\emu" -Force  -Recurse -ErrorAction SilentlyContinue
Remove-Item "C:\Program Files (x86)\Intel\oneAPI\compiler\$LATEST_VERSION\windows\lib\oclfpga" -Force  -Recurse -ErrorAction SilentlyContinue
Remove-Item "C:\Program Files (x86)\Intel\oneAPI\compiler\$LATEST_VERSION\windows\lib\ocloc" -Force  -Recurse -ErrorAction SilentlyContinue
Remove-Item "C:\Program Files (x86)\Intel\oneAPI\compiler\$LATEST_VERSION\windows\lib\x86" -Force  -Recurse -ErrorAction SilentlyContinue

exit 0
