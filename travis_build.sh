#!/bin/sh
docker run -v $PWD:/app -w /app jklmnn/gnat:gpl.2019.spark /bin/sh -c "cd libkeccak && make install && cd .. && sh build.sh"
cd tests
python3 run_tests.py --verbose
