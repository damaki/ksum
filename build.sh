#!/bin/sh

echo Generating src/version.ads ...
./gen-version-ads.sh

if [ $? != 0 ]
then
  echo Failed to generate version. Exiting.
  exit $?
fi

echo Building ksum ...
gprbuild -p -P ksum.gpr -j0 -XLIBKECCAK_BUILD=default
echo Done