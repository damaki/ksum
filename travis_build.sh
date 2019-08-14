#!/bin/sh
sh gen-version-ads.sh
docker run -v $PWD:/app -w /app jklmnn/gnat:gpl.2019.spark /bin/sh -c "cd libkeccak && make install && cd .. && gprbuild -p -P ksum.gpr -j0 -XLIBKECCAK_BUILD=default"
cd tests
python3 run_tests.py --verbose
