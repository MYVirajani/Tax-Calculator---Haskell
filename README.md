# Tax Calculator – Functional Programming Mini Project (Haskell)

## Group Members
- EG/2020/4183 Samasthi H.G.S.R
- EG/2020/4115 Poornima K.N.
- EG/2020/4254 Virajani M.Y.
- EG/2020/4257 Wahalathanthri W.A.S  

---

## Problem Description

Income tax calculation is a critical component of financial and governmental systems. Manual or poorly designed tax computation software can lead to incorrect results, lack of transparency, and difficulty in auditing. This project implements a **Tax Calculator** using **pure functional programming principles in Haskell**. The system calculates personal income tax based on progressive tax brackets and provides a detailed tax breakdown for each slab. The application demonstrates how functional programming ensures **correctness, reliability, and maintainability** in real-world financial software.

---

## Instructions to Run the Program

### Prerequisites
- GHC (Glasgow Haskell Compiler)
- VS Code (recommended) or any terminal

### Compile the Program
Open a terminal in the project directory and run:

```bash
ghc Main.hs DataTypes.hs Processing.hs IOHandler.hs Utils.hs



## Run the Program (Interactive Mode)
.\Main.exe

Run in Batch Mode (Optional – CSV Input)
.\Main.exe -f taxpayers.csv


CSV format:

Name,Income
Yasodha,1400000
Amesha,750000
Sayuri,120000
Nethmi,45000
```
---

## Sample Input / Output

### Sample Input
```
Enter your name:
Yasodha
Enter your annual income (numbers only):
1400000
```
### Sample Output
```
Tax summary for: Yasodha
Annual Income: Rs 1400000.0
Total Tax: Rs 145000.0
Breakdown by slab:
  Taxed Rs 500000.0 @ 5.00% => Rs 25000.0
  Taxed Rs 500000.0 @ 10.00% => Rs 50000.0
  Taxed Rs 400000.0 @ 20.00% => Rs 80000.0
```
---

## Functional Programming Concepts Used
1. Pure Functions

Functions always produce the same output for the same input.

Example:

calculateTax :: [TaxBracket] -> Double -> Double

2. Recursion

Tax is calculated by recursively processing each tax slab.

Example:

go (TaxBracket lim r : bs) remaining acc

3. Algebraic Data Types (ADT)

Custom data structures model real-world entities clearly.

Example:

data TaxBracket = TaxBracket {
  limit :: Double,
  rate  :: Double
}

4. Higher-Order Functions

Functions like map, foldl' are used for data processing.

Example:

sum (map slabTax breakdown)

5. Immutability

All values are immutable; new values are created instead of modifying existing ones, improving safety and predictability.

