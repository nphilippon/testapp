#include <Rcpp.h>
using namespace Rcpp;

//' Fast Backtest Engine in C++
//'
//' Iterates through a vector of returns and trading signals to compute the cumulative equity line.
//'
//' @param asset_returns A numeric vector of the daily percentage returns of the asset.
//' @param signals A numeric vector of signals (-1, 0, 1). Signals should theoretically be lagged by 1 day to prevent look-ahead bias.
//'
//' @return A numeric vector of the cumulative portfolio equity curve.
//' @export
// [[Rcpp::export]]
NumericVector cpp_backtest(NumericVector asset_returns, NumericVector signals) {
  int n = asset_returns.size();
  NumericVector equity_curve(n);
  
  double current_equity = 1.0; // Start the backtest with a baseline of 1.0 (100% equity)
  
  // Calculate equity curve loops very quickly in C++
  for (int i = 0; i < n; i++) {
    // If signal is 1 (long), we earn the return.
    // If signal is -1 (short), we earn the inverse of the return.
    // If signal is 0, we earn 0 return.
    double daily_return = asset_returns[i] * signals[i];
    
    // Compounding growth equation
    current_equity = current_equity * (1.0 + daily_return);
    equity_curve[i] = current_equity;
  }
  
  return equity_curve;
}
