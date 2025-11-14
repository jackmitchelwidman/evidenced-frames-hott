# PCA Quick Reference Card

## The S and K Combinators

### K - The Constant Combinator
```
K x y = x
```
**Intuition**: Returns first argument, ignores second
**Example**: `K 5 10 = 5`

### S - The Substitution Combinator
```
S x y z = (x z) (y z)
```
**Intuition**: Distributes z to both x and y, then applies results
**Example**: `S K K x = K x (K x) = x`  ← This is the identity!

## Common Combinators Built from S and K

| Combinator | Lambda | S-K Definition | What it does |
|------------|---------|----------------|--------------|
| I (identity) | λx.x | S K K | Returns its argument |
| true | λx.λy.x | K | Returns first of two args |
| false | λx.λy.y | K I = K (S K K) | Returns second of two args |
| pair | λx.λy.λf.f x y | S (S (K S) K) | Builds a pair |
| fst | λp.p K | I | Extracts first from pair |
| snd | λp.p (K I) | I | Extracts second from pair |

## Church Encodings

### Booleans
```agda
true  : λt.λf.t  -- Returns first argument
false : λt.λf.f  -- Returns second argument
not   : λb.b false true
and   : λa.λb.a b false
or    : λa.λb.a true b
```

### Natural Numbers (Church Numerals)
```agda
0 : λf.λx.x           -- Apply f zero times
1 : λf.λx.f x         -- Apply f once
2 : λf.λx.f (f x)     -- Apply f twice
n : λf.λx.f^n x       -- Apply f n times

succ : λn.λf.λx.f (n f x)   -- Add 1
plus : λm.λn.λf.λx.m f (n f x)
mult : λm.λn.λf.m (n f)
```

### Pairs
```agda
pair  : λx.λy.λs.s x y     -- Make a pair
fst   : λp.p true          -- First element
snd   : λp.p false         -- Second element
```

## PCA Laws (Agda Style)

### K Law
```agda
k · x ≡ just k'  →  k' · y ≡ just x
```
Translation: K x y reduces to x

### S Law
```agda
s · x ≡ just s₁  →
s₁ · y ≡ just s₂  →
s₂ · z ≡ just s₃  →
x · z ≡ just xz  →
y · z ≡ just yz  →
xz · yz ≡ just r  →
s₃ ≡ r
```
Translation: S x y z reduces the same as (x z)(y z)

## Working with Maybe in Agda

### Pattern Matching
```agda
with s · k
... | nothing = nothing    -- Failed to compute s · k
... | just sk = sk · k     -- Success, continue
```

### Using Do-Notation (Requires Import)
```agda
i : Maybe Carrier
i = do
  sk ← s · k
  sk · k
```

### The _·?_ Helper
```agda
_·?_ : Maybe Carrier → Maybe Carrier → Maybe Carrier
nothing ·? _ = nothing
_ ·? nothing = nothing
(just f) ·? (just x) = f · x
```

## Common Patterns

### Building a Combinator
1. Think: What should it do in λ-calculus?
2. Express using S and K
3. Handle Maybe at each application step

Example: Building I from S and K
```agda
i : Maybe Carrier
i with s · k           -- Step 1: Apply s to k
... | nothing = nothing
... | just sk with sk · k   -- Step 2: Apply result to k
...   | nothing = nothing
...   | just i' = just i'   -- Success!
```

### Proving a Law
```agda
-- Often Agda can prove by computation!
i-law : ∀ {x} → i ·? just x ≡ just x
i-law = refl  -- Agda computes this automatically
```

## Reduction Strategies

### Call-by-Name (What we use)
- Reduce leftmost outermost redex first
- Don't reduce inside abstractions
- Arguments not reduced before substitution

### Step-by-Step Example
```
S K K x
→ K x (K x)    [reduce S-redex]
→ x            [reduce K-redex]
```

## Types Cheatsheet

```agda
-- Core types
Carrier : Set                              -- The set of codes
_·_ : Carrier → Carrier → Maybe Carrier    -- Application
k : Carrier                                -- K combinator
s : Carrier                                -- S combinator

-- Common return types
i : Maybe Carrier                          -- Might fail to construct
church-true : Carrier                      -- Always exists (it's k)
church-false : Maybe Carrier               -- Needs i, might fail

-- Functions on codes
reduce : ℕ → Code → Maybe Code            -- Reduce with step limit
```

## Connection to Paper

| Paper | Your Code |
|-------|-----------|
| C (set of codes) | `Carrier` |
| · (partial application) | `_·_` |
| c (a code) | `c : Carrier` |
| cf · ca ↓ cr | `cf · ca ≡ just cr` |
| cf · ca ↓ | `cf · ca ≡ nothing` |
| cλn.e (functional completeness) | Building combinators from S and K |
| c^λ_n (Church numeral) | `church-n` |

## Debugging Tips

### Type Mismatch
```
Expected: Carrier
Got: Maybe Carrier
```
**Fix**: Pattern match or use `with` to extract from Maybe

### Variable Not in Scope
```
k not in scope
```
**Fix**: Add `open PCA pca` to bring PCA fields into scope

### Incomplete Pattern Match
```
Missing case: nothing
```
**Fix**: Add pattern for `nothing` case

## Quick Wins

### To Complete Exercise 1 (Identity)
```agda
i = s · k >>= λ sk → sk · k
-- Or using with:
i with s · k
... | nothing = nothing
... | just sk = sk · k
```

### To Complete Exercise 2a (True)
```agda
church-true = k  -- It's just K!
```

### To Complete Exercise 2b (False)
```agda
church-false with i    -- Need identity first
... | nothing = nothing
... | just i' = k · i'  -- K I
```

## Remember

- **Partiality**: Not all computations terminate → Use Maybe
- **Uniformity**: Same code must work for all inputs → Key for realizability
- **Church Encodings**: Data becomes functions → Connects math to computation
- **S and K**: Universal → Can build any computable function

## Next Steps After PCAs

1. **Evidenced Frames**: Abstract away the PCA, keep only evidence relation
2. **Effects**: Add flip (non-determinism), lookup (state), fail (failure)
3. **Triposes**: Connect to logic (higher-order predicate logic)
4. **HoTT**: Your research contribution!

---

Keep this open while working on exercises!
