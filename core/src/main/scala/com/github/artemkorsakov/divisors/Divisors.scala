package com.github.artemkorsakov.divisors

import com.github.artemkorsakov.primes.Primes._

class Divisors(number: Long) {

  /** Returns all divisors of n (more than 1 and less than n).
    */
  def divisors: Set[Long] =
    (2L to math.sqrt(number.toDouble).toLong).filter(number % _ == 0).flatMap(i => Set(i, number / i)).toSet

  /** Return the sum of the divisors of n.
    *
    * @see <a href="https://en.wikipedia.org/wiki/Divisor_function">detailed description</a>
    */
  def sumOfDivisors: BigInt = {
    val primeDivisors = number.primeFactorsWithPow
    primeDivisors.keySet.foldLeft(BigInt(1)) { (mul, prime) =>
      val num = BigInt(prime).pow(primeDivisors(prime).toInt + 1) - 1
      val den = BigInt(prime) - 1
      mul * (num / den)
    }
  }

  /** Return the count of divisors of n.
    */
  def countOfDivisors: Long =
    number.primeFactorsWithPow.values.foldLeft(1L)((mul, a) => mul * (a + 1))

}

object Divisors {
  implicit def long2Divisors(i: Long): Divisors = new Divisors(i)
  implicit def int2Divisors(i: Int): Divisors   = new Divisors(i.toLong)

  def gcd(aArray: Array[Long]): Long =
    if (aArray.length <= 1) {
      aArray.headOption.getOrElse(1)
    } else {
      gcd(aArray.head, gcd(aArray.tail))
    }

  /** Return the greatest common divisor.
    */
  def gcd(u: Long, v: Long): Long =
    if (u == v) {
      u
    } else if (u == 0) {
      v
    } else if (v == 0) {
      u
    } else {
      (~u & 1, ~v & 1) match {
        case (1, 1) => gcd(u >> 1, v >> 1) << 1
        case (1, 0) => gcd(u >> 1, v)
        case (0, 1) => gcd(u, v >> 1)
        case (_, _) => if (u > v) gcd(u - v, v) else gcd(v - u, u)
      }
    }

  /** Extended Euclidean algorithm.
    *
    * @see <a href="https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm">detailed description</a>
    */
  def gcdex(a: Long, b: Long): (Long, Long, Long) =
    if (a == 0) {
      (b, 0, 1)
    } else {
      val temp = gcdex(b % a, a)
      (temp._1, temp._3 - (b / a) * temp._2, temp._2)
    }

  /** Modular multiplicative inverse.
    *
    * @see <a href="https://en.wikipedia.org/wiki/Modular_multiplicative_inverse">detailed description</a>
    */
  def gcdInverse(a: Long, m: Long): Long = {
    val extraEuclid = gcdex(a, m)
    if (extraEuclid._1 == 1) {
      (extraEuclid._2 % m + m) % m
    } else {
      -1
    }
  }

}
