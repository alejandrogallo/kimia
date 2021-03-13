#pragma once
struct TensorReaderSpec {
  struct  {
    std::string file;
    enum  {
      BINARY,
      TEXT,
    } mode;
  } in;
  struct  {
    int data;
  } out;
};
