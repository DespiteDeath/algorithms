---
layout: docs
title: "Digits"
realization_link: ../realization/digital/digits.html
---

## {{page.title}}

### containTheSameDigits
Do two numbers contain the same digits?

It is the same as [Operations with strings - areAnagrams](../str/string_algorithms#areanagrams)

**Algorithm**

**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#containTheSameDigits" | downcase }})

**Sources** 

**Using**
```scala mdoc
import com.github.artemkorsakov.digital.Digits._
containTheSameDigits(127035954683L, 589323567104L)
```

---

### digits
Returns the digits of a number.

**Algorithm**

**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#digits" | downcase }})

**Sources** 

**Using**
```scala mdoc
1406357289L.digits
```

---

### sumOfDigits
Returns the sum of the digits of a number.

**Algorithm**

**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#sumOfDigits" | downcase }})

**Sources** 

**Using**
```scala mdoc
1406357289L.sumOfDigits
```

---

### hasEqualDigits
Does the number have at least two of the same digits?

**Algorithm**

**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#hasEqualDigits" | downcase }})

**Sources** 

**Using**
```scala mdoc
56993.hasEqualDigits
56993.hasEqualDigits(2, 9)
56993.hasEqualDigits(3, 9)
```

---

### possibleNumbers
Returns all numbers made up of given digits.

**Algorithm**

**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#possibleNumbers" | downcase }})

**Sources** 

**Using**
```scala mdoc
possibleNumbers(Seq(1, 1, 5, 6))
```

---
