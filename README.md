# Marketing Attribution & ROI Simulator (Shiny)

An interactive Shiny app for multi-touch attribution (Markov) and a lightweight MMM-style ROI scenario simulator (MMM-lite)—with Diagnostics and Data Health checks to make results easier to trust and explain.

## 🔗 Live Demo

Try the app here: [https://ofosuosei.shinyapps.io/marketing-attribution/](https://ofosuosei.shinyapps.io/marketing-attribution/)

## 🧭 What this app does

### Upload & Validate

- Upload a CSV (or use included demo datasets)
- Auto-detects dataset type:
- Journey (multi-touch events) → Markov attribution + journey diagnostics
- MMM (media spend / activity + sales) → MMM-lite scenario simulation + MMM diagnostics
- Preview the data and confirm required columns

### Attribution (Markov)

- Computes removal-effect attribution (normalized)
**Optional:**
- Lookback window (days)
- Remove consecutive duplicate channels
**Shows:**
- Attribution chart + table
- Sample “top paths”

### ROI Simulator (MMM-lite)

- Fit a simple MMM-lite model (per division if applicable)
- Adjust channel multipliers (sliders) to run “what-if” scenarios
**Outputs:**
- Baseline vs scenario predicted sales plot
- Scenario summary + per-period table (baseline, scenario, lift)

### 📈 Diagnostics

- MMM diagnostics
- Holdout metrics (train/test)
- Coefficients table
- Actual vs predicted (holdout)
- Residuals vs fitted
- Markov diagnostics
- Journey stats (users, touches, conversion rate, etc.)
- Top transitions
- Optional “health” warnings (e.g., suspiciously high conversion rate)

### 📊 Data Health

**A credibility-focused panel to quickly spot common issues:**

- Missingness by column
- Date/time coverage (min/max timestamps)
- Touches-per-user distribution (journey datasets)
- A configurable conversion-rate threshold (numeric input) to flag “suspiciously high” conversion rates

## ⚙️ Setup

### Install packages

```r
    install.packages(c("shiny","dplyr","ggplot2","DT"))
```

## ▶️ Run

**From project root:**

```r
    shiny::runApp()
    # or
    shiny::runApp(".")
```

## ⚠️ Notes & disclaimers

> MMM-lite is intentionally simple (useful for demos and directional “what-if” scenarios). It is not a full Bayesian MMM, does not automatically handle adstock/saturation unless explicitly implemented in your MMM script, and should not be treated as production-grade causal inference.
> Attribution results depend heavily on data definitions (especially what counts as a “conversion” and how journeys are filtered).