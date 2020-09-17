---
layout: docs
title: "Euler's totient function"
realization_link: ../realization/primes/phi_function.html
---

## {{page.title}}

In number theory, [Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function) counts 
the positive integers up to a given integer n that are relatively prime to n.

### phiFunction
Counts the positive integers up to a given integer n that are relatively prime to n.

**Algorithm**
1. Euler's product formula: ![Euler's product formula](http://latex.codecogs.com/svg.latex?{\displaystyle&space;\varphi&space;(n)=n\prod&space;_{p\mid&space;n}\left(1-{\frac&space;{1}{p}}\right)})
     
**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#phiFunction" | downcase }})

**Sources** 
- [wiki: Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function)

**Using**
```scala
import com.github.artemkorsakov.primes.PhiFunction._

val res = 87109.phiFunction
// res0: Long = 79180
```

---

### phiFunctionArray
Euler's totient function's array

**Algorithm**
1. Use this formula: 
![formula](http://latex.codecogs.com/svg.latex?%7B%5Cdisplaystyle%20%5Cvarphi%20(mn)=%5Cvarphi%20(m)%5Cvarphi%20(n)%5Ccdot%20%7B%5Cfrac%20%7Bd%7D%7B%5Cvarphi%20(d)%7D%7D%5Cquad%20%7B%5Ctext%7Bwhere%20%7D%7Dd=%5Coperatorname%20%7Bgcd%7D%20(m,n)%7D)
2. A consequence of this formula is the formula:
![formula](http://latex.codecogs.com/svg.latex?%7B%5Cdisplaystyle%20%5Cvarphi%20(p*n)=%7B%5Cbegin%7Bcases%7Dp*%5Cvarphi%20(n)&%7B%5Ctext%7Bif%20n%20%25%20p%20=%200%7D%7D%5C%5C(p-1)*%5Cvarphi%20(n)&%7B%5Ctext%7Bif%20n%20%25%20p%20!=%200%7D%7D%5Cend%7Bcases%7D%7D%7D)   
     
**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#phiFunctionArray" | downcase }})

**Sources** 
- [wiki: Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function)

**Using**
```scala
import com.github.artemkorsakov.primes.PhiFunction._

val res = phiFunctionArray(15)
// res0: Array[Long] = Array(0, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8)
```

---
