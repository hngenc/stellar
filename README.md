Stellar
=======================

Stellar is a platform for designing and building dense and sparse accelerators which includes a DSL for describing hardware accelerators and a compiler which converts your hardware accelerator descriptions to synthesizable Verilog.

Setup
-----

Stellar is designed to work as part of the [Chipyard](https://github.com/ucb-bar/chipyard) platform. Stellar provides high-level DSLs and optimization passes while Chipyard provides a rich library of hardware components (all the way up to full SoCs) and build flows which we rely on for the hardware generation process.
```
git clone https://github.com/ucb-bar/chipyard.git
cd chipyard/
git checkout stellar

cd generators/
git clone https://github.com/hngenc/stellar.git
cd stellar/
git submodule update --init --recursive

cd ../..
./build-setup.sh riscv-tools
```

It is also possible to run Stellar as a standalone project. To do so, you can clone this repo directly without installing Chipyard and then run the `./standalone.sh` script. You will not be able to generate Verilog for full SoCs, but you will still be able to generate RTL for spatial arrays, memory buffers, and so on. Furthermore, when Stellar is a standalone repo, you can also generate 3D visualizations of your spatial arrays, which can be useful for debugging (Scala library incompatibilities prevent us from enabling 3D visualizations in Chipyard-integrated installations).

If you want to see 3D visualizations of your hardware designs, you must pass the `-XstartOnFirstThread` option to the JVM.
If you're running directly from SBT, then this has already been handled for you, but if you're running from inside IntelliJ, then you'll need to add this yourself to the `VM Options` of your `Run/Debug Configuration`.

Alternatively, if you don't care about 3D visualizations, then you can simply pass `shouldRender=false` to the `.elaborate()` method of your spatial arrays when running outside of Chipyard.

Running Tests
------------------------
Run these commands to build Stellar software binaries, generate Verilog for a small matmul accelerator, and build a VCS simulator that your tests can run on.

```
cd chipyard/
source env.sh

cd generators/stellar/software
pip install scipy
./build.sh
cd -

cd sims/vcs

export JAVA_HEAP_SIZE="128G"

for config in StellarRocketConfig LargeStellarRocketConfig SparseDenseStellarRocketConfig; do
    make CONFIG=$config
done

make CONFIG=StellarRocketConfig run-binary BINARY=../../generators/stellar/software/build/small/matmul-baremetal
```

If a test succeeds, you should see the word `SUCCEEDED` printed onto the screen.

Other tests are also found in the `software/` directory. For example:
```
make CONFIG=LargeStellarRocketConfig run-binary BINARY=../../generators/stellar/software/build/large/matmul-baremetal
make CONFIG=SparseDenseStellarRocketConfig run-binary BINARY=../../generators/stellar/software/build/small/sparse_dense_matmul-baremetal
make CONFIG=SCNNStellarRocketConfig run-binary BINARY=../../generators/stellar/software/build/large/scnn-baremetal
make CONFIG=MidOuterSpaceMatmulStellarRocketConfig BINARY=../../generators/stellar/software/build/large/outerspace_matmul-baremetal
```

FAQ
---

* Why do I get weird Chisel/FIRRTL errors when elaborating my designs?
    - Unfortunately, Stellar-generated Chisel/FIRRTL can be very different from how human developers would ordinarily write it. We've found that, in practice, this sometimes means that FIRRTL passes (optimized and designed for human developer output) can produce unintelligible errors when elaborating Stellar designs. For example, FIRRTL's combinational loop checker may segfault (irrespective of whether combinational loops do or don't exist in your accelerator), or FIRRTL passes which are simply meant to strip debugging info from the final generated Verilog can fail. The easiest workaround for such errors is to skip or disable failing FIRRTL passes in `common.mk`, for example with the `--disable-check-comb-cycles` option.

Acknowledgements
----------------

* We use [Chisel](https://www.chisel-lang.org/) to generate Verilog.

* We use [Slack3D](https://github.com/simerplaha/Slack3D) to generate 3D images.

