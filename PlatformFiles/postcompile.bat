del ..\DistPackages\Windows\DWSIM.vshost.*
del ..\DistPackages\Windows\*.tmp
del ..\DistPackages\Windows\*.dylib
del ..\DistPackages\Windows\*.so

del ..\DistPackages\macOS\DWSIM.vshost.*
del ..\DistPackages\macOS\*.tmp

del ..\DistPackages\Linux\DWSIM.vshost.*
del ..\DistPackages\Linux\*.tmp
del ..\DistPackages\Linux\*.dylib
del ..\DistPackages\Linux\libCEF.dll
del ..\DistPackages\Linux\*.pak
del ..\DistPackages\Linux\*.dat
del ..\DistPackages\Linux\*.dat

del ..\DistPackages\Windows_32\DWSIM.vshost.*
del ..\DistPackages\Windows_32\*.tmp
del ..\DistPackages\Windows_32\*.dylib
del ..\DistPackages\Windows_32\*.so

del ..\DistPackages\Linux\ExcelDna.Integration.dll
del ..\DistPackages\Windows\ExcelDna.Integration.dll
del ..\DistPackages\Windows_32\ExcelDna.Integration.dll
del ..\DistPackages\macOS\ExcelDna.Integration.dll

del ..\DistPackages\Windows\plugins\*Skia*
del ..\DistPackages\Windows\plugins\*Eto*
rmdir /s /q ..\DistPackages\Windows\plugins\x86
rmdir /s /q ..\DistPackages\Windows\plugins\x64
rmdir /s /q ..\DistPackages\Windows\x86
rmdir /s /q ..\DistPackages\Windows\x64

del ..\DistPackages\Windows_32\plugins\*Skia*
del ..\DistPackages\Windows_32\plugins\*Eto*
rmdir /s /q ..\DistPackages\Windows_32\plugins\x86
rmdir /s /q ..\DistPackages\Windows_32\plugins\x64

del ..\DistPackages\macOS\plugins\*Skia*
del ..\DistPackages\macOS\plugins\*Eto*
rmdir /s /q ..\DistPackages\macOS\plugins\x86
rmdir /s /q ..\DistPackages\macOS\plugins\x64

del ..\DistPackages\Linux\plugins\*Skia*
del ..\DistPackages\Linux\plugins\*Eto*
rmdir /s /q ..\DistPackages\Linux\plugins\x86
rmdir /s /q ..\DistPackages\Linux\plugins\x64
rmdir /s /q ..\DistPackages\Linux\x86
rmdir /s /q ..\DistPackages\Linux\x64

xcopy "..\DWSIM\Lib\*" "..\DistPackages\Windows\Lib\*" /E /Y /F /D
xcopy "..\DWSIM\Lib\*" "..\DistPackages\Windows_32\Lib\*" /E /Y /F /D
xcopy "..\DWSIM\Lib\*" "..\DistPackages\macOS\Lib\*" /E /Y /F /D
xcopy "..\DWSIM\Lib\*" "..\DistPackages\Linux\Lib\*" /E /Y /F /D
xcopy "..\DWSIM\Lib\*" "..\DistPackages\Raspberry\Lib\*" /E /Y /F /D

xcopy "..\DistPackages\Windows\*" "..\DistPackages\Windows_Plus\*" /E /Y /F /D

rmdir /s /q ..\DistPackages\Windows_Plus\x86
rmdir /s /q ..\DistPackages\Windows_Plus\x64

rmdir /s /q ..\DistPackages\Windows_32\x86
rmdir /s /q ..\DistPackages\Windows_32\x64

rmdir /s /q ..\DistPackages\macOS\samples
rmdir /s /q ..\DistPackages\Linux\samples
rmdir /s /q ..\DistPackages\Windows\samples
rmdir /s /q ..\DistPackages\Windows_32\samples
rmdir /s /q ..\DistPackages\Windows_Plus\samples

xcopy "Common\samples\*" "..\DistPackages\macOS\samples\*" /E /Y /F /D
xcopy "Common\samples\*" "..\DistPackages\Windows\samples\*" /E /Y /F /D
xcopy "Common\samples\*" "..\DistPackages\Windows_32\samples\*" /E /Y /F /D
xcopy "Common\samples\*" "..\DistPackages\Windows_Plus\samples\*" /E /Y /F /D
xcopy "Common\samples\*" "..\DistPackages\Linux\samples\*" /E /Y /F /D