#!/usr/bin/env bash
set -euo pipefail

packaging_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd "$packaging_dir/../.." && pwd)"
task_dotnet="${DOTNET:-dotnet}"
skip_tests=false
if [[ "${1:-}" == "--skip-tests" && $# == 1 ]]; then
    skip_tests=true
elif [[ $# != 0 ]]; then
    echo "Usage: bash src/Packaging/release.sh [--skip-tests]" >&2
    exit 2
fi
cd "$repo_dir"

version="$("$task_dotnet" msbuild "$packaging_dir/Mbus.Codec/Mbus.Codec.csproj" -getProperty:BetaPackageVersion -nologo)"
sdk_dir="$("$task_dotnet" msbuild "$packaging_dir/Mbus.Codec/Mbus.Codec.csproj" -getProperty:MSBuildBinPath -nologo)"
feed_dir="$repo_dir/artifacts/nuget"
mkdir -p "$feed_dir"
sources=(--source "$feed_dir" --source "${BETA_NUGET_SOURCE:-https://api.nuget.org/v3/index.json}")
if [[ -d "$sdk_dir/FSharp/library-packs" ]]; then
    sources+=(--source "$sdk_dir/FSharp/library-packs")
fi

if [[ "$skip_tests" == false ]]; then
    "$task_dotnet" restore "$packaging_dir/Mbus.Beta.slnf" "${sources[@]}" -m:1 --verbosity minimal
    "$task_dotnet" test "$packaging_dir/Mbus.Beta.slnf" -c Release --no-restore -m:1 \
        --logger trx --results-directory "$repo_dir/artifacts/test-results/mbus-beta" --verbosity minimal
fi

commit="$(git rev-parse HEAD)"
for package in Mbus.Codec Mbus.Devices; do
    project="$packaging_dir/$package/$package.csproj"
    "$task_dotnet" restore "$project" "${sources[@]}" -m:1 --verbosity minimal
    "$task_dotnet" pack "$project" -c Release --no-restore -m:1 --verbosity minimal \
        -p:RepositoryCommit="$commit" -o "$feed_dir"
done
python3 "$packaging_dir/verify-packages.py" "$feed_dir" "$version" "$commit"

# A copied consumer and fresh package cache prevent source references or stale beta
# packages from making the smoke test pass. No repository projects are referenced.
mkdir -p "$repo_dir/artifacts/consumer"
consumer_dir="$(mktemp -d "$repo_dir/artifacts/consumer/mbus-beta.XXXXXX")"
cp "$repo_dir/src/Examples/MbusDevice.PackageSmoke/Program.cs" "$consumer_dir/Program.cs"
cat > "$consumer_dir/Consumer.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <OutputType>Exe</OutputType>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Metering.Mbus.Devices" Version="$version" />
  </ItemGroup>
</Project>
EOF
"$task_dotnet" restore "$consumer_dir/Consumer.csproj" "${sources[@]}" \
    --packages "$consumer_dir/packages" --verbosity minimal
"$task_dotnet" run --project "$consumer_dir/Consumer.csproj" -c Release --no-restore
echo "Beta $version verified. Local NuGet feed: $feed_dir"
