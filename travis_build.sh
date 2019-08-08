#!/bin/sh
git clone https://github.com/damaki/libkeccak.git
docker run -v $PWD:/app -w /app jklmnn/gnat:gpl.2019.spark /bin/sh -c "cd libkeccak && make install && cd .. && gprbuild -p -P ksum.gpr -f -j0 -XLIBKECCAK_BUILD=default"
