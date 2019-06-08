import com.github.stanfordfuturedata.momentsketch._
import com.tdunning.math.stats.TDigest

import scala.util.Random

//scalastyle:off
object QuantileTester extends App {
  val OutlierMean = 2E9   // Outlier = 5 seconds in nanos
  val OutlierSpread = 3E9
  val DefaultSpread = 50000.0

  /**
   * Create a simulated set of latencies having certain characteristics, based on some average, and having a certain
   * fraction of outliers.
   */
  def makeLatencies(count: Int, outliersFrac: Double = 0.03, mean: Double = 100000.0): Seq[Double] = {
    (0 until count).map { _ =>
      if (Random.nextDouble < outliersFrac) {
        // Outlier
        OutlierMean + Random.nextDouble * OutlierSpread
      } else {
        Math.max(util.Random.nextGaussian * DefaultSpread + mean, 100)
      }
    }
  }

  /**
   * Sort and compute exact quantiles from raw measurements, which is simply the (q*size)'th item
   */
  def realQuantiles(latencies: Seq[Double], quantiles: Seq[Double]): Seq[Double] = {
    val sorted = latencies.sorted.toArray
    quantiles.map { q =>
      require(q >= 0.0 && q < 1.0)
      sorted((sorted.size * q).toInt)
    }
  }

  import collection.JavaConverters._

  def createTDigest(latencies: Seq[Double]): TDigest = {
    val dig = TDigest.createDigest(100)    // Default compression factor/q = 50
    latencies.foreach(dig.add)
    println(s"Created T-Digest from ${latencies.length} samples: ${dig.centroids.asScala.toSeq.length} centroids")
    dig
  }

  // Adds logs of latencies to the sketch for better summary for large ranges of values
  def createMSketch(latencies: Seq[Double]): MomentStruct = {
    val ms = new MomentStruct(10)   // k=8 moments
    latencies.foreach(l => ms.add(Math.log(l)))
    ms
  }

  // Get count from arg1
  val nSamples = args(0).toInt
  val quantiles = Seq(0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)
  val numLoops = 100

  var tDigSumRelErr = 0.0
  var tDigMaxRelErr = 0.0
  var tDigSumMaxRelErr = 0.0
  var msSumRelErr = 0.0
  var msMaxRelErr = 0.0
  var msSumMaxRelErr = 0.0

  for { _ <- 0 until numLoops } {
    val latencies = makeLatencies(nSamples)
    // println(s"First 100 samples:")
    // latencies.take(100).foreach(println)

    val realQs = realQuantiles(latencies, quantiles)

    val tDig = createTDigest(latencies)
    val tDigQs = quantiles.map(q => tDig.quantile(q))

    var batchSumRelErr = 0.0
    var batchMaxRelErr = 0.0

    realQs.zip(tDigQs).foreach { case (realRank, tDigestRank) =>
      val err = Math.abs((tDigestRank - realRank) / realRank)
      batchSumRelErr += err
      batchMaxRelErr = Math.max(batchMaxRelErr, err)
    }

    tDigMaxRelErr = Math.max(tDigMaxRelErr, batchMaxRelErr)
    tDigSumRelErr += batchSumRelErr
    tDigSumMaxRelErr += batchMaxRelErr

    val mSketch = createMSketch(latencies)
    val solver = new MomentSolver(mSketch)
    solver.setGridSize(1024)
    solver.solve()
    // Recover using exp to restore original value
    val mSketchQs = quantiles.map(q => Math.exp(solver.getQuantile(q)))

    batchSumRelErr = 0.0
    batchMaxRelErr = 0.0
    realQs.zip(mSketchQs).foreach { case (realRank, mSketchRank) =>
      val err = Math.abs((mSketchRank - realRank) / realRank)
      batchSumRelErr += err
      batchMaxRelErr = Math.max(batchMaxRelErr, err)
    }

    msMaxRelErr = Math.max(msMaxRelErr, batchMaxRelErr)
    msSumRelErr += batchSumRelErr
    msSumMaxRelErr += batchMaxRelErr
  }

  println(s"nSamples: $nSamples   Loops: $numLoops  quantiles: $quantiles")
  println(s"T-Digest average rel error, all buckets: ${tDigSumRelErr / (numLoops * quantiles.length)}")
  println(s"T-Digest avg max rel error in iteration: ${tDigSumMaxRelErr / numLoops}")
  println(s"T-Digest max rel error, all iterations : $tDigMaxRelErr")
  println(s"M-Sketch average rel error, all buckets: ${msSumRelErr / (numLoops * quantiles.length)}")
  println(s"M-Sketch avg max rel error in iteration: ${msSumMaxRelErr / numLoops}")
  println(s"M-Sketch max rel error, all iterations : $msMaxRelErr")

  // println(s"T-Digest vs Real Quantiles:")
  // realQs.zip(tDigQs).foreach { case (realRank, tDigestRank) =>
  //   val err = (tDigestRank - realRank) / realRank
  //   println(f"$tDigestRank%9.2f\t$realRank%9.2f\t(rel err: $err)")
  // }


  // println(s"Moment-Sketch vs Real Quantiles:")
  // realQs.zip(mSketchQs).foreach { case (realRank, mSketchRank) =>
  //   val err = (mSketchRank - realRank) / realRank
  //   println(f"$mSketchRank%9.2f\t$realRank%9.2f\t(rel err: $err)")
  // }
}