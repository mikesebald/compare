Vergleich der Validierungs- (und Matching-) ergebnisse zweier unterschiedlicher CDH-Versionen
========================================================
author: Tim Kettenacker, Michael Sebald
date: 01.02.2016

Aufgabenstellung
========================================================

Im Rahmen eines Produktupgrades soll der Nachweis geführt werden, dass sich die Ergebnisse im Hinblick
auf postalische und Namensvalidierung im Vergleich zur Produktvorversion nicht verschlechtert haben.

Der CDH liefert hierzu bereits **zahlreiche** Kennzahlen. Da der CDH jedoch selbst Gegenstand des Vergleichs ist, macht es Sinn, den eigentlichen Vergleich auch *außerhalb* des Produktstacks durchzuführen.

Dieser "externe" Vergleich ergänzt die vom CDH gelieferten Qualitätsmetriken (Kennzahlen und Regressionstestergebnisse) und stützt diese.


Lösungsansatz
========================================================

1. Aufsetzen zweier Testumgebungen mit den zu vergleichenden Software-Ständen incl. Referenztabellen
1. Befüllen der beiden Umgebungen mit dem selben Datenbestand
1. Export der validierten Datensätze aus den beiden Systemen
1. Vergleich der Validierungsergebnisse
1. Aufbereitung der Vergleichsergebnisse


Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](Compare-figure/unnamed-chunk-2-1.png)
