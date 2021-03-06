#include <steps/SumVector.hpp>
#include <iostream>
#include <string>

template <typename F>
void runSumVector(SumVector<F> *s) {
  std::cout << "Running inside the motherfucking runSumvector" << std::endl;
  s->out.sum = F(65.5);
}

template void runSumVector<int>(SumVector<int>*);
template void runSumVector<float>(SumVector<float>*);
template void runSumVector<double>(SumVector<double>*);
template<> void runSumVector<std::string>(SumVector<std::string>* d) {}
