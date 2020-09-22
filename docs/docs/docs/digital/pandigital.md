---
layout: docs
title: "Pandigital"
realization_link: ../realization/digital/pandigital.html
---

## {{page.title}}

### isPandigital
- Is a number an integer that has among its significant digits each digit used exactly once?
- Is a number an integer that has among its significant digits (from 1 to n) each digit used exactly once?
- Is a number an integer that has among its significant digits (from digits list) each digit used exactly once?

**Algorithm**

**Complexity**
     
[**Algorithm realization**]({{ page.realization_link }}{{ "#isPandigital" | downcase }})

**Sources** 

**Using**
```scala
import com.github.artemkorsakov.digital.Pandigital._

val res0 = 192384576L.isPandigital
// res0: Boolean = true

val res1 = 2134.isPandigital(4)
// res1: Boolean = true

val res2 = 1406357289L.isPandigital(Array(9, 8, 7, 6, 5, 4, 3, 2, 1, 0))
// res2: Boolean = true
```

---