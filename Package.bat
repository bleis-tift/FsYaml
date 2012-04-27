set PATH=%PATH%;C:\Windows\Microsoft.NET\Framework\v4.0.30319
rmdir /S /Q bin

rmdir /S /Q FsYaml\obj
rmdir /S /Q FsYaml\bin
msbuild FsYaml/FsYaml.fsproj /p:Configuration=Release;Platform=AnyCPU /t:Rebuild
mkdir bin bin\net4.0
move FsYaml\bin\Release\*.dll bin\net4.0

nuget pack Package.nuspec
move *.nupkg bin\
