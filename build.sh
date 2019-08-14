#!/bin/sh

echo Generating src/version.ads ...
./gen-version-ads.sh

echo Building ksum ...
gprbuild -p -P ksum.gpr -j0 -XLIBKECCAK_BUILD=default
echo Done