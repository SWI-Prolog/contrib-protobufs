set -e # required to propagate errors to cmake
make -j1 SWIPL=$1 -C $2 clean
make -j1 SWIPL=$1 -C $2 test
make -j1 SWIPL=$1 -C $2 run_addressbook
