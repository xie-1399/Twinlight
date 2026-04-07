package Twinlight.TLFpu

import Twinlight.TLSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random


case class BasicFloatTools(fp32: Boolean = true, fp16: Boolean = false) {
  val IEEE_FP32 = fp32
  val IEEE_FP16 = fp16

  val gen = new Random()

  // subnormal: s 00000000 f
  // inf      : s 11111111 ==0
  // NaN      : s 11111111 !=0
  def FP2Int(x: Float): Int = {
    // only 32 bits version now
    java.lang.Float.floatToRawIntBits(x)
  }

  def Int2FP(x: Int): Float = {
    java.lang.Float.intBitsToFloat(x)
  }

  def isNan(x: Float): Boolean = {
    val xint = FP2Int(x)
    isNan(xint)
  }

  def isNan(x: Int): Boolean = {
    val xint = x
    if (IEEE_FP32) { // 0111-1111-1000-0000-0000-0000-0000-0000
      val exp = (xint & 0x7F800000) >> 23
      val mantissa = xint & 0x007FFFFF
      exp == 0xFF && mantissa != 0
    } else { // 0111-1100-0000-0000
      val exp = (xint & 0x7C00) >> 10
      val mantissa = xint & 0x3FF
      exp == 0x1F && mantissa != 0
    }
  }

  def genRand(rand_percent: Int = 65, inf_percent: Int = 10, nan_percent: Int = 10, subnormal_percent: Int = 10, zero_percent: Int = 5): (BigInt, Float) = {
    val randMax = rand_percent + inf_percent + nan_percent + subnormal_percent + zero_percent
    val randi = gen.nextInt(randMax)

    val rand_threshold = 0 + rand_percent
    val inf_threshold = rand_threshold + inf_percent
    val nan_threshold = inf_threshold + nan_percent
    val subnormal_threshold = nan_threshold + subnormal_percent
    val zero_threshold = subnormal_threshold + zero_percent

    assert(zero_threshold == randMax)

    val (xint, xfloat) = if (IEEE_FP32) {
      val xx = (if (randi < rand_threshold) { // normal
        BigInt((if (gen.nextBoolean()) -1 else 1) * gen.nextInt(Int.MaxValue))
      } else if (randi < inf_threshold) { // Inf
        BigInt(if (gen.nextBoolean()) "7F800000" else "FF800000", 16)
      } else if (randi < nan_threshold) { // NaN
        BigInt(if (gen.nextBoolean()) "7F800000" else "FF800000", 16) + gen.nextInt((1 << 23) - 1) + 1
      } else if (randi < subnormal_threshold) { // subnormal
        BigInt(if (gen.nextBoolean()) "00000000" else "80000000", 16) + gen.nextInt((1 << 23) - 1) + 1
      } else { // zero
        BigInt(if (gen.nextBoolean()) "00000000" else "80000000", 16)
      }).mod(BigInt(1) << 32)
      (xx, Int2FP(xx.toInt))
    } else {
      val xx = (if (randi < rand_threshold) { // normal
        BigInt((if (gen.nextBoolean()) -1 else 1) * gen.nextInt(Short.MaxValue))
      } else if (randi < inf_threshold) { // Inf
        BigInt(if (gen.nextBoolean()) "7C00" else "FC00", 16)
      } else if (randi < nan_threshold) { // NaN
        BigInt(if (gen.nextBoolean()) "7C00" else "FC00", 16) + gen.nextInt((1 << 10) - 1) + 1
      } else if (randi < subnormal_threshold) { // subnormal
        BigInt(if (gen.nextBoolean()) "0000" else "8000", 16) + gen.nextInt((1 << 10) - 1) + 1
      } else {
        BigInt(if (gen.nextBoolean()) "0000" else "8000", 16)
      }).mod(BigInt(1) << 16)
      (xx, new FP16().int16tofloat(xx.toInt))
    }
    //    println((if (IEEE_FP32) "xint32 = " else "xint16 = ") + xint)
    //    println((if (IEEE_FP32) "fp32 = " else "fp16 = ") + xfloat.toString)
    (xint, xfloat)
  }

  def genRandAlmostNormal(): (BigInt, Float) = {
    genRand(rand_percent = 2000, inf_percent = 1, nan_percent = 1, subnormal_percent = 10, zero_percent = 100)
  }
}