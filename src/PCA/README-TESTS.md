# PCA Unit Tests - Study Guide

## Overview

The file `PCAUnitTests.agda` contains executable tests that help you understand:
- How PCAs work concretely
- The S and K combinators
- Church encodings
- The connection to Evidenced Frames

## How to Use These Tests

### In Emacs with Agda Mode:

1. **Open the file**: `src/PCA/PCAUnitTests.agda`

2. **Load/compile it**: Press `C-c C-l` (Control-c Control-l)
   - If it compiles successfully, ALL tests pass!
   - Agda verifies each test by computation

3. **Study each test**:
   - Read the comments explaining what each test demonstrates
   - Look at the type signature (the `test-name : ...` line)
   - See that the implementation is just `refl` (reflexivity)
   - This means Agda computed both sides and proved they're equal!

4. **Experiment**:
   - Try changing a test to make it fail
   - See what error Agda gives you
   - This helps you understand what's really happening

### From Command Line:

```bash
cd /home/jackwidman/EvidencedFrames
agda src/PCA/PCAUnitTests.agda
```

If you see "Checking PCA.PCAUnitTests ..." and no errors, all tests passed!

## What Each Part Teaches

### Part 1: Concrete PCA
- Builds a simple PCA using explicit syntax trees
- Uses K and S combinators with application
- Shows how reduction actually works with a step counter

**Key Learning**: PCAs are concrete - they're just a way to compute!

### Part 2: PCA Laws
- Tests 1-4 verify the K law: `K x y = x`
- Shows that K ignores its second argument

**Key Learning**: The laws ensure consistent computation

### Part 3: Identity Combinator
- Tests 4-6 prove `I = S K K`
- Shows that identity works on any input

**Key Learning**: You can build NEW combinators from S and K alone!

### Part 4: Church Encodings
- Tests 7-8 show booleans as functions
- `true = K` (takes first argument)
- `false = K I` (takes second argument)

**Key Learning**: Data can be represented as pure functions

### Part 5: Church Numerals
- Tests 9-10 show how to encode numbers
- `0 = K I` (apply f zero times)
- `1 = I` (apply f once)

**Key Learning**: Even numbers are just functions!

### Part 6: Uniformity
- Tests 11-13 show I works the same on ALL inputs
- The SAME code `I` returns x for ANY x

**Key Learning**: Uniformity is crucial for realizability - we can't peek at inputs!

### Part 7: Connection to Evidenced Frames
- Shows how propositions are sets of codes
- Evidence is a code that transforms realizers
- `evidence-identity-preserves-K` proves I preserves the "is-K" property

**Key Learning**: PCAs naturally form evidenced frames!

### Part 8: Composition
- Test 12 shows the B combinator composes functions
- `B f g x = f (g x)`

**Key Learning**: Evidence can be composed (transitivity)

### Part 9: Summary
- Connects everything back to your research
- Explains how Evidenced Frames generalize PCAs

## Interactive Exercises

Try these experiments to deepen your understanding:

### Exercise 1: Break a test
Change `test-identity-K` to:
```agda
test-identity-K : I-combinator apply K-code ≡ just S-code
test-identity-K = refl
```

Load the file and see the error. What does Agda tell you?

### Exercise 2: Add a new test
Try adding a test that `K K S = K`:
```agda
test-k-k-s : (App K-code K-code) apply S-code ≡ just K-code
test-k-k-s = refl
```

Does it pass? Why or why not?

### Exercise 3: Trace a reduction
Pick one test and manually trace through the reduction steps.
For example, for `I K`:
```
I K = (S K K) K
    = (K K (K K))    [S law]
    = K              [K law]
```

### Exercise 4: Build a new combinator
Try to define:
```agda
-- W combinator: W f x = f x x  (duplicates argument)
W-combinator : Code
W-combinator = ???  -- Can you build this from S and K?
```

Hint: `W = S S (S K)`

## Connection to Your Research

These tests show the TRADITIONAL PCA model. Your research with Liron Cohen is about EXTENDING this to:

1. **Effectful computation**:
   - State (mutable memory)
   - Non-determinism (multiple possible outcomes)
   - Failure (computations that can fail)

2. **Evidenced Frames**:
   - Abstract away from specific PCAs
   - Keep only the "evidence relation"
   - Allow different computational systems

3. **Homotopy Type Theory**:
   - Extend to cubical Agda
   - Work with higher inductive types
   - Your unique contribution!

The tests here give you the FOUNDATION. Understanding how traditional PCAs work will help you see what's new and powerful about Evidenced Frames.

## Questions to Answer After Studying

Can you answer these after working through the tests?

1. Why do we need BOTH S and K? What does each one do?
2. How does `I = S K K` work? Trace through the reduction.
3. Why is uniformity important for realizability?
4. What does it mean for a code to be "evidence" of an entailment?
5. How do Church encodings work? Why can we represent data as functions?
6. What's the difference between a PCA and an Evidenced Frame?

## Next Steps

After mastering these tests:

1. **Read the paper**: `evidenced_frames.pdf` - you'll understand it much better now!
2. **Study `PCA-Tutorial.agda`**: More detailed explanations
3. **Try `PCA-Exercises.agda`**: Hands-on practice
4. **Explore effects**: Think about how to add state/non-determinism to these tests

Good luck with your research! 🚀
