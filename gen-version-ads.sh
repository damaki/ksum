#!/bin/sh

# This script generates a file src/version.ads which contains a
# version string based on the output of git describe.

VERSION=`git describe --tags`

# Check if git describe was successful
if [ $? = 0 ]
then
  echo "package Version is" > src/version.ads
  echo "   Version_String : constant String := \"$VERSION\";" >> src/version.ads
  echo "end Version;" >> src/version.ads

  echo Done
else
  exit $?
fi