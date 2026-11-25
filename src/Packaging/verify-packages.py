"""Verify the published package graph, assembly ownership and source revision."""

import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from zipfile import ZipFile

feed, version, commit = sys.argv[1:]
packages = {
    "Metering.Mbus.Codec": (
        {
            "MbusCodec.dll", "Protocol.dll", "PublicTypes.dll", "BaseWriters.dll",
            "ByteReaders.dll", "Decoders.dll", "Io.dll", "Parsers.dll",
            "Cryptography.dll", "Utility.dll", "Validators.dll",
        },
        {"FSharp.Core": "10.1.201"},
    ),
    "Metering.Mbus.Devices": (
        {"MbusDevices.dll", "Devices.dll"},
        {"Metering.Mbus.Codec": version},
    ),
}
owned = set()
for package_id, (expected_dlls, expected_dependencies) in packages.items():
    path = Path(feed) / f"{package_id}.{version}.nupkg"
    with ZipFile(path) as archive:
        files = archive.namelist()
        manifest_name, = [name for name in files if name.endswith(".nuspec")]
        metadata = ET.fromstring(archive.read(manifest_name)).find("{*}metadata")
        assert metadata.findtext("{*}id") == package_id, path
        assert metadata.findtext("{*}version") == version, path
        assert metadata.find("{*}repository").get("commit") == commit, path
        dependencies = {
            dependency.get("id"): dependency.get("version")
            for dependency in metadata.findall(".//{*}dependency")
        }
        assert dependencies == expected_dependencies, (path, dependencies)
        dll_paths = [name for name in files if name.endswith(".dll")]
        assert all(name.startswith("lib/net10.0/") for name in dll_paths), dll_paths
        dlls = {Path(name).name for name in dll_paths}
        assert len(dll_paths) == len(dlls), path
        assert dlls == expected_dlls, (path, dlls)
        assert not owned.intersection(dlls), (path, owned.intersection(dlls))
        owned.update(dlls)
        assert "README.md" in files, path
        assert not any(name.endswith((".pdf", ".pfx", ".snk")) for name in files), path
    print(f"Verified {path.name}: {len(dlls)} assemblies, dependencies {dependencies}")

assert not list(Path(feed).glob(f"Metering.Foundation.{version}.nupkg")), "Unexpected Foundation package"
