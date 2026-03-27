# Quantitative Commodity-Equity Trading Strategies

This guide explains the foundational mathematical logic behind the trading strategies implemented in the Discovery Engine and Backtester. Use these tools to identify statistical edges between resource-heavy equities (like Canadian Oil and Gas companies) and their underlying macro commodities (like Crude Oil).

---

## 1. Divergence Scanning (Pairs Trading / Mean Reversion)

### **How it Works**
Commodity-producing equities naturally track the price of the commodity they extract. For example, Suncor (SU.TO) generally moves in correlation with WTI Crude Oil (CL=F). However, markets are inefficient. In the short term, the equity price might temporarily decouple from the commodity price due to panic selling, temporary news, or market flow. Divergence scanning mathematically identifies these temporary decouplings so you can bet on the relationship "snapping back" to normal.

### **The Math (Rolling Z-Score)**
1. **Calculate the Ratio:** `Ratio = Target_Equity_Price / Base_Commodity_Price`
2. **Calculate the Rolling Baseline:** Determine the Moving Average (`Roll_Mean`) and Standard Deviation (`Roll_SD`) of that Ratio over a set lookback window (e.g., 60 days).
3. **Calculate the Z-Score:** 
   `Z-Score = (Current_Ratio - Roll_Mean) / Roll_SD`

### **Trading Logic**
* **Overbought (Z-Score > +2.0):** The equity is too expensive relative to the commodity. **Action:** Sell/Short the equity.
* **Oversold (Z-Score < -2.0):** The equity is too cheap relative to the commodity. **Action:** Buy/Long the equity.

---

## 2. Lead-Lag Prediction (Cross-Correlation / Delayed Momentum)

### **How it Works**
Information doesn't always hit all markets simultaneously. Sometimes, a massive spike in Crude Oil today isn't fully priced into secondary Canadian Oil companies until tomorrow or the day after. The Lead-Lag Predictor uses the Cross-Correlation Function (CCF) to scan historical arrays to prove if today's commodity movement consistently *predicts* tomorrow's equity movement.

### **The Math (CCF)**
1. **Determine Returns:** Calculate daily fractional returns for both assets: `Return_t = (Close_t / Close_t-1) - 1`.
2. **Align by Lags:** Shift the base commodity returns backward and forward by $k$ days (e.g., Lag +1, Lag +2).
3. **Correlate:** Calculate the Pearson correlation coefficient ($R$) between the standard Target returns and the Lagged Base returns.
4. **Significance Threshold:** Calculate the boundary line formatting `2 / sqrt(N)` (where N is the number of days). Any correlation spiking past this line is statistically significant, not just random noise.

### **Trading Logic**
* If the CCF scanner shows a massive positive correlation at **Lag +1**, you have a mathematically proven edge.
* **Action:** If Crude Oil closes up > 2% *today*, you Buy the Equity right at *tomorrow's* open, anticipating the delayed price reaction.

---

## 3. Future Implementations (Trend & Regression)

### **Moving Average Crossovers (Trend Following)**
* **How it works:** Buying an asset when its short-term momentum accelerates past its long-term average.
* **The Math:** Buy when the 50-day Simple Moving Average (SMA) crosses above the 200-day SMA. Sell when it crosses below. 

### **OLS Multiple Regression (Alpha Isolation)**
* **How it works:** Finding out precisely how much of an equity's return is dictated by Oil versus dictated by the broader S&P 500.
* **The Math:** $Y_{Equity} = \alpha + \beta_1(X_{Oil}) + \beta_2(X_{SPY}) + \epsilon$.
* **Trading Logic:** Allows you to build "market-neutral" portfolios where you hedge out the S&P 500 risk, leaving you trading solely on the pure individual alpha of the company.
