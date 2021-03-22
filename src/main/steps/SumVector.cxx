#include <steps/SumVector.hpp>
#include <iostream>

template <typename F>
void runSumVector(SumVector<F> &s) {
  s.out.sum = F(65.5);
}

template void runSumVector<int>(SumVector<int>&);
template void runSumVector<float>(SumVector<float>&);
template void runSumVector<double>(SumVector<double>&);
