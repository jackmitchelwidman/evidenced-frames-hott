# Learning Guide: PCAs and Evidenced Frames

This guide will help you learn the concepts from the paper through hands-on Agda programming.

## Overview of Your Learning Path

```
PCAs → Church Encodings → Realizability → Evidenced Frames → Triposes → HoTT Extension
```

## Phase 1: Understanding PCAs (Start Here!)

### What You're Learning

**Partial Combinatory Algebras (PCAs)** are the foundation of realizability theory. They capture the essence of computation with just:
- A set of "codes" (programs/terms)
- A partial application operation
- Two combinators: **S** and **K**

### Why This Matters

From the paper (Section II-B):
> "PCAs formalize the key components of computation that support the proofs-as-programs correspondence."

In realizability:
- Mathematical propositions are "realized" by computational codes
- Proofs correspond to programs
- PCAs provide these programs

### Your Exercises

Work through these files **in order**:

#### 1. **PCA-Exercises.agda** (Start Here!)

This is your main learning file with exercises to complete.

**Exercise 1: Identity Combinator**
- Goal: Build `I = S K K`
- Why: Understand how S and K combine
- Connection: This shows "functional completeness" - we can build any combinator!

**Exercise 2: Church Booleans**
- Goal: Encode true/false as functions
- Why: PCAs only have functions - we must encode data
- Connection: In realizability, this connects Bool (math) to codes (computation)

**Exercise 3: Church Numerals**
- Goal: Encode natural numbers as iteration
- Why: This is from the paper (page 3, c^λ_n)
- Connection: Shows how ℕ gets computational representation

**Exercise 4: Concrete PCA**
- Goal: Build an actual working PCA
- Why: Makes everything concrete - you can run it!
- Connection: Proves that our abstract definitions actually work

#### 2. **PCA-Tutorial.agda**

Read this file for detailed explanations and context. It explains:
- What each concept means
- Why it's designed that way
- How it connects to the paper
- The big picture

#### 3. **PCA-Solutions.agda**

**Only look at this when stuck!** Contains:
- Complete solutions
- Detailed explanations of why they work
- Hints for the tricky parts

### How to Work Through the Exercises

1. **Open PCA-Exercises.agda in your editor**
   ```bash
   cd ~/EvidencedFrames
   emacs src/PCA/PCA-Exercises.agda  # or your favorite editor
   ```

2. **Find the first hole: `{!!}`**

3. **Think about what should go there**
   - Read the comments above
   - Think about the types
   - Try to understand what the function should do

4. **Try to implement it**
   - Start typing in the hole
   - Use Agda's type-checking to guide you
   - Errors are helpful! They tell you what's wrong

5. **If stuck, check the tutorial for hints**
   - Read the corresponding section in PCA-Tutorial.agda
   - Think about the explanation
   - Try again

6. **Still stuck? Check the solution**
   - Look at PCA-Solutions.agda
   - Understand WHY it works, don't just copy
   - Try to implement it yourself from understanding

### Key Concepts to Master

By the end of Phase 1, you should understand:

✓ **Partiality**: Why we use `Maybe` and what it means for computation

✓ **Functional Completeness**: Why S and K are sufficient for all computation

✓ **Church Encodings**: How to represent data (Bool, ℕ, pairs) as pure functions

✓ **The K Combinator**: K x y = x (constant function)

✓ **The S Combinator**: S x y z = (x z)(y z) (substitution/distribution)

✓ **Uniformity**: Why we need the *same* code to work for *all* inputs (crucial for realizability!)

### Connection to the Paper

- **Definition II.1**: Partial Applicative Structure → Your PCA record
- **Definition II.2**: Functional Completeness → Your ability to build I from S and K
- **Page 3**: Church numerals c^λ_n → Your Exercise 3
- **Section II-C**: From PCAs to realizability triposes → Coming in Phase 2!

## Phase 2: From PCAs to Evidenced Frames (Next)

After mastering PCAs, you'll learn how they become evidenced frames.

### The Big Idea (from Example III.4)

A PCA naturally forms an evidenced frame where:
- **Propositions Φ** = P(C) (sets of codes that realize the proposition)
- **Evidence E** = C (the codes themselves)
- **Evidence relation**: c evidences φ₁ ⊢ φ₂ when c converts realizers of φ₁ to realizers of φ₂

### Files You'll Work On

- `EvidenceFrames/EvidenceFrame.agda` - Define the abstract structure
- `EvidenceFrames/FromPCA.agda` - Show how PCAs become evidenced frames
- `EvidenceFrames/Examples.agda` - Add effects (flip, lookup, fail)

## Phase 3: Effectful Computation (After Phase 2)

Learn how to extend PCAs with computational effects:

- **Non-determinism** (flip combinator) - Section IV.B.1
- **Mutable state** (lookup combinators) - Section IV.B.2
- **Failure** (fail combinator) - Section IV.B.3

This is where your research gets interesting! Traditional PCAs can't do this.

## Phase 4: HoTT Extension (Your Research!)

Finally, extend evidenced frames to work with:
- Cubical Agda
- Homotopy Type Theory
- Higher inductive types

This is uncharted territory - your contribution!

## Tips for Learning

### 1. **Don't Rush**
Take time to understand each concept deeply. It's better to really get PCAs than to superficially know everything.

### 2. **Draw Diagrams**
Sketch reduction sequences:
```
S K K x
  ↓ [apply S]
K x (K x)
  ↓ [apply K]
x
```

### 3. **Type-Driven Development**
Let Agda's types guide you:
- Look at what you have: the types of variables in scope
- Look at what you need: the type of the hole
- Find operations that connect them

### 4. **Experiment**
Try things! Agda will tell you if they're wrong.
```agda
i : Maybe Carrier
i = s · k  -- Error! Type mismatch
-- Oh right, s · k returns Maybe Carrier, not Carrier
```

### 5. **Connect to Paper**
For each exercise, find the corresponding part in the paper:
- Read that section
- See how your code implements the math
- Understand both perspectives (math and code)

### 6. **Ask Questions**
When stuck, ask yourself:
- What is this trying to compute?
- What would I do by hand?
- How can I express that in S and K?

## Checking Your Progress

### Can you answer these?

**After Exercise 1:**
- [ ] What is the identity combinator?
- [ ] Why does S K K = I?
- [ ] What does "functional completeness" mean?

**After Exercise 2:**
- [ ] How are booleans encoded as functions?
- [ ] Why is true = K?
- [ ] How would you implement NOT using S and K?

**After Exercise 3:**
- [ ] How do Church numerals work?
- [ ] What does the number 3 "do" as a function?
- [ ] Why is 0 = K I?

**After Exercise 4:**
- [ ] What makes something a PCA?
- [ ] Why do we need a step counter in reduce?
- [ ] How does this connect to the λ-calculus?

## Daily Goals (for your 9-day timeline)

**Days 1-2**: Master PCAs
- Complete all exercises in PCA-Exercises.agda
- Understand Church encodings
- Build the concrete PCA

**Days 3-4**: Evidenced Frames
- Define the EvidenceFrame record
- Show PCA → EvidenceFrame construction
- Understand the abstraction

**Days 5-6**: Effectful Examples
- Implement flip combinator (non-determinism)
- Implement lookup (state)
- Implement fail (failure)

**Days 7-8**: Triposes (if time)
- Define tripos structure
- Understand UFam construction
- Connect to realizability

**Day 9**: Prepare presentation
- Clean up code
- Write documentation
- Prepare questions for Liron

## Getting Help

### From the Code
1. Read PCA-Tutorial.agda for explanations
2. Check PCA-Solutions.agda if stuck
3. Look at type signatures - they guide you

### From the Paper
1. Section II: Background on PCAs and Triposes
2. Section III: Definition of Evidenced Frames
3. Section IV: Effectful computation examples
4. Example III.4: PCA as an evidenced frame

### From Agda
1. Agda's error messages are helpful!
2. Use holes `{!!}` to see what you need
3. Use Ctrl-C Ctrl-, in a hole to see context

## Success Criteria

You'll know you're ready to talk to Liron when you can:

1. ✓ Explain what a PCA is and why S and K are universal
2. ✓ Implement basic combinators (I, booleans, numerals)
3. ✓ Explain how PCAs become evidenced frames
4. ✓ Understand why uniformity matters for realizability
5. ✓ Describe how effects extend the framework
6. ✓ Articulate your HoTT extension idea

## Ready to Start?

Open `src/PCA/PCA-Exercises.agda` and begin with Exercise 1!

Remember: The goal is deep understanding, not just completing exercises. Take your time, think deeply, and connect code to concepts.

Good luck! 🚀
