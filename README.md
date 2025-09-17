## What is the Multi-Armed Bandit Problem?

Imagine you're in a casino with K slot machines (one-armed bandits), each with an unknown probability of reward. You have T total pulls to maximize your reward. The fundamental challenge: **how do you decide which arm to pull at each time step?**

### The Core Dilemma: Exploration vs Exploitation

- **Exploitation**: Choose options that seem best based on past outcomes ("greedy", "short-sighted")
  - Too much → Regret of missing unexplored "gems"
- **Exploration**: Try options not yet tried or insufficiently sampled ("gaining info", "long-sighted")  
  - Too much → Regret of wasting time on "duds"

### Real-World Examples

| Domain | Exploitation | Exploration |
|--------|-------------|-------------|
| **Restaurant Selection** | Go to your favorite restaurant | Try a new restaurant |
| **Online Advertising** | Show the most successful ad | Test a new advertisement design |
| **Oil Drilling** | Drill at the best known location | Drill at a new location |
| **Game Playing** | Play the move you believe is best | Play an experimental move |
| **Clinical Trials** | Assign patients to best treatment | Test new treatments |
| **Recommendation Systems** | Recommend similar to past clicks | Recommend diverse content |
| **Financial Investment** | Invest in proven strategies | Try new investment approaches |
| **A/B Testing** | Use the winning variant | Test new features |

## Key Concepts

### Regret
The difference between the optimal expected reward and the actual reward received:

**Mathematical Definition:**
- Cumulative Regret: $R(T) = T \cdot p^* - \sum_{t=1}^{T} r_t$
- Average Regret: $\bar{R}(T) = p^* - \frac{1}{T}\sum_{t=1}^{T} r_t$
- Where $p^* = \max_{i \in \{1,...,K\}} p_i$ is the highest expected reward

**Goal:** Minimize $\bar{R}(T) \rightarrow 0$ as $T \rightarrow \infty$

The regret measures how much reward we "lose" by not always pulling the optimal arm. Good algorithms achieve sublinear regret growth.

## Implemented Algorithms

### 1. Epsilon-Greedy (ε-greedy)

The simplest exploration strategy with a tunable exploration rate:

**Algorithm:**

$$a_t = \begin{cases} 
\text{Uniform}(1, K) & \text{with probability } \varepsilon \\
\arg\max_{i \in \{1,...,K\}} \hat{p}_i & \text{with probability } 1-\varepsilon
\end{cases}$$

**Reward Estimate:**

$$\hat{p}_i = \frac{\sum_{s=1}^{t-1} r_s \cdot \mathbb{1}[a_s = i]}{\sum_{s=1}^{t-1} \mathbb{1}[a_s = i]}$$

**Variants**:
- **Decaying ε-greedy**: $\varepsilon_t = \min(1, \frac{c|A|}{d^2t})$ achieves logarithmic regret
- **Optimistic Initialization**: Start with high initial value estimates

**Pros**: Simple to understand and implement  
**Cons**: Constant ε leads to linear regret; exploration is random, not intelligent

### 2. Upper Confidence Bound (UCB)

"Optimism in the face of uncertainty" - selects arms based on upper confidence bounds:

**Algorithm (UCB1):**
- Initialize: Pull each arm once
- For $t = K+1, K+2, ..., T$:

$$a_t = \arg\max_{i \in \{1,...,K\}} \left[\hat{p}_i + \sqrt{\frac{2 \ln(t)}{N_t(i)}}\right]$$

Where:
- $UCB_i(t) = \hat{p}_i + \sqrt{\frac{2 \ln(t)}{N_t(i)}}$
- $N_t(i)$ = number of times arm $i$ was pulled before time $t$
- Confidence radius decreases as $O(\sqrt{\frac{\ln(t)}{N_t(i)}})$

**Key insight**: The term $\sqrt{\frac{2 \ln(t)}{N_t(i)}}$ balances exploration and exploitation

**Pros**: Theoretically grounded with regret bounds - O(K log T) for distribution-dependent, O(√(KT log T)) for worst-case  
**Cons**: Can be slower to converge than Thompson Sampling

### 3. Thompson Sampling

Bayesian approach using posterior sampling from Beta distributions:

**Algorithm:**
- Initialize: $\alpha_i = \beta_i = 1$ for all arms $i \in \{1,...,K\}$
- For $t = 1, 2, ..., T$:
  1. Sample: $\theta_i \sim \text{Beta}(\alpha_i, \beta_i)$ for each arm $i$
  2. Pull arm: $a_t = \arg\max_{i \in \{1,...,K\}} \theta_i$
  3. Observe reward: $r_t \sim \text{Bernoulli}(p_{a_t})$
  4. Update parameters:
     $$\alpha_{a_t} \leftarrow \alpha_{a_t} + r_t$$
     $$\beta_{a_t} \leftarrow \beta_{a_t} + (1 - r_t)$$

**Posterior Distribution:**
- Prior: $p_i \sim \text{Beta}(1, 1) = \text{Uniform}(0, 1)$
- Posterior: $p_i | \text{data} \sim \text{Beta}(\alpha_i, \beta_i)$
  - Where $\alpha_i = 1 + S_i$ (successes) and $\beta_i = 1 + F_i$ (failures)

**Key insight**: Sampling from posterior naturally balances exploration and exploitation

**Pros**: Excellent empirical performance, elegant Bayesian framework  
**Cons**: Requires understanding of Bayesian inference

## Quick Start

### Development Server with JSaddle

```bash
make dev
```

Access the application at http://localhost:8080

### WASM Build (requires wasm32-wasi-ghc)

```bash
make build
make serve  # Serve the built WASM locally
```

## Application Features

- **Real-time Simulation**: Watch algorithms learn and adapt
- **Interactive Controls**: Start/Stop/Reset simulation controls
- **Algorithm Comparison**: Switch between algorithms dynamically
- **Visual Analytics**:
  - Reward accumulation over time
  - Regret curves showing algorithm performance
  - Arm selection frequency visualization
  - Confidence intervals for UCB algorithm
- **Statistical Dashboard**:
  - Selection counts per arm
  - Estimated probabilities
  - Cumulative rewards
  - Average regret metrics

## Default Configuration

- **Number of arms**: 5
- **Arm probabilities**: [0.1, 0.2, 0.3, 0.4, 0.5] (gradual increase for clear demonstration)
- **Default trials**: 1,000 (adjustable up to 10,000)
- **Algorithm parameters**:
  - ε-greedy: ε = 0.15
  - UCB: c = 2.0
- **Update frequency**: Real-time visualization

## Development Commands

```bash
make help         # Show all available commands
make dev          # Start development server (port 8080)
make build        # Build WASM for production
make serve        # Serve WASM build locally
make clean        # Clean build artifacts
make lint         # Run hlint code linter
make format       # Auto-format with ormolu
make format-check # Check code formatting
```

## References

### Academic Materials
1. **Cambridge University - Randomised Algorithms** ([PDF](https://www.cl.cam.ac.uk/teaching/2122/RandAlgthm/lec15_bandits_handout.pdf))
2. **Stanford University - Multi-Armed Bandits** ([PDF](https://web.stanford.edu/class/cme241/lecture_slides/MultiArmedBandits.pdf))
3. **Cornell University - Learning, Games, and Electronic Markets** ([PDF](https://www.cs.cornell.edu/courses/cs683/2007sp/lecnotes/week8.pdf))
4. **Oxford University - Algorithmic Foundations of Learning** ([PDF](https://www.stats.ox.ac.uk/~rebeschi/teaching/AFoL/22/material/lecture15.pdf))
5. [My gist part 1](https://gist.github.com/kiwamizamurai/65b482e5fff06aaf3499a8274911ed07)
6. [My gist part 2](https://gist.github.com/kiwamizamurai/ad94f49d9a85969f0ecaa8cc2bf1925c)
7. [My gist part 3](https://gist.github.com/kiwamizamurai/755bb64881f074e1b32b74df7f071615)

## Future Extensions

### Contextual Bandits
The current implementation focuses on the classical multi-armed bandit problem where actions are independent of context. Future extensions could include:

- **Linear Contextual Bandits (LinUCB)**: Incorporating feature vectors to inform arm selection
- **Neural Contextual Bandits**: Using neural networks to learn complex context-reward relationships
- **Contextual Thompson Sampling**: Bayesian approaches with context-dependent priors
