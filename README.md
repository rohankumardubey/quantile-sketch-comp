# Quantile-Sketch-Comp

A comparison of two digests against raw data generated from a distribution.  Value errors are calculated for a given number of samples for both digest types.

To run:

1. clone the [momentsketch repo](https://github.com/stanford-futuredata/momentsketch) alongside this repo
2. In the momentsketch repo: `mvn package`
3. Run sbt shell
4. `run 1000`   // for 1000 samples