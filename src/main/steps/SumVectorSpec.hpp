#pragma once

template < typename _GF >
struct SumVector {
  const struct  {
    std::vector< _GF > vector;
  } in;
  struct  {
    _GF sum;
  } out;
};