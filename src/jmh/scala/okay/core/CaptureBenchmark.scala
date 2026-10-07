package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*

/** A CAPTURE THROUGH `depth` MAPS, its `k` applied `shots` times (okay-cont's `delimCaptureDepth`): the frames
 * between the shift and its reset taken as the continuation, and run again per shot */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CaptureBenchmark:

  @Param(Array("1", "16", "256"))
  var depth: Int = 0

  @Param(Array("1", "8"))
  var shots: Int = 0

  def calls(k: Unit => Cont[Pure, Int, Int, Int], i: Int, acc: Int): Cont[Pure, Int, Int, Int] =
    if i == 0 then pure(acc) else k(()).flatMap(r => calls(k, i - 1, acc + r))

  @Benchmark
  def captureDepth(): Int =
    var p: Cont[Pure, Int, Int, Int] = shift[Pure, Int, Int, Unit](k => calls(k, shots, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p.reset[Int].value
