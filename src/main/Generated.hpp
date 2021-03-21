#include<string>
#include<map>
#include<vector>
#include<array>
#include<cstdlib>
#include<ecl/ecl.h>
#include "/home/gallo/software/kimia/src/main/steps/SumVectorSpec.hpp"
std::map<std::string, size_t> POINTER_DATABASE;

size_t clstr (const cl_object o){
  const size_t dimension(o->base_string.dim)
             , charSize = ECL_EXTENDED_STRING_P(o) ? 4 : 1
             ;
  std::string result;
  ecl_base_char* c = o->base_string.self;
  // TODO: handle the unicode well.
  // right now I know it is 32bit characters,
  // that is why the i * 4 is there
  for (size_t i = 0; i < dimension; i++)
    result += *(c + i * charSize);
  return (size_t)new std::string(result);
}


size_t clstr (const cl_object o);
size_t v_of_clstr (const cl_object o){
  std::vector< std::string > result(ecl_to_int(cl_length(o)));
  for (size_t i=0; i < result.size(); i++) {
    cl_object index(c_string_to_object(std::to_string(i).c_str()));
    std::string *element = (std::string*)clstr(cl_aref(2, o, index));
    result[i] = *element;
  }
  return (size_t)new std::vector< std::string >(result);
}


template
struct SumVector< std::string >;
size_t cs_nil (const cl_object o);
size_t s_sum_vector_with_clstr (const cl_object o){
  return (size_t)new SumVector< std::string >{
    {
      *(std::vector< std::string >*)v_of_clstr(cl_getf(2, cl_getf(2, o, c_string_to_object(":IN")), c_string_to_object(":VECTOR")))
    } /* unnamed */
  ,
    {
      *(std::string*)clstr(cl_getf(2, cl_getf(2, o, c_string_to_object(":OUT")), c_string_to_object(":SUM")))
    } /* unnamed */
  };
}


size_t clint (const cl_object o){
  return (size_t)new int(ecl_to_int(o));
}


size_t clint (const cl_object o);
size_t v_of_clint (const cl_object o){
  std::vector< int > result(ecl_to_int(cl_length(o)));
  for (size_t i=0; i < result.size(); i++) {
    cl_object index(c_string_to_object(std::to_string(i).c_str()));
    int *element = (int*)clint(cl_aref(2, o, index));
    result[i] = *element;
  }
  return (size_t)new std::vector< int >(result);
}


template
struct SumVector< int >;
size_t cs_nil (const cl_object o);
size_t s_sum_vector_with_clint (const cl_object o){
  return (size_t)new SumVector< int >{
    {
      *(std::vector< int >*)v_of_clint(cl_getf(2, cl_getf(2, o, c_string_to_object(":IN")), c_string_to_object(":VECTOR")))
    } /* unnamed */
  ,
    {
      *(int*)clint(cl_getf(2, cl_getf(2, o, c_string_to_object(":OUT")), c_string_to_object(":SUM")))
    } /* unnamed */
  };
}


size_t clfloat (const cl_object o){
  return (size_t)new float(ecl_to_float(o));
}


size_t clfloat (const cl_object o);
size_t v_of_clfloat (const cl_object o){
  std::vector< float > result(ecl_to_int(cl_length(o)));
  for (size_t i=0; i < result.size(); i++) {
    cl_object index(c_string_to_object(std::to_string(i).c_str()));
    float *element = (float*)clfloat(cl_aref(2, o, index));
    result[i] = *element;
  }
  return (size_t)new std::vector< float >(result);
}


template
struct SumVector< float >;
size_t cs_nil (const cl_object o);
size_t s_sum_vector_with_clfloat (const cl_object o){
  return (size_t)new SumVector< float >{
    {
      *(std::vector< float >*)v_of_clfloat(cl_getf(2, cl_getf(2, o, c_string_to_object(":IN")), c_string_to_object(":VECTOR")))
    } /* unnamed */
  ,
    {
      *(float*)clfloat(cl_getf(2, cl_getf(2, o, c_string_to_object(":OUT")), c_string_to_object(":SUM")))
    } /* unnamed */
  };
}


size_t cldouble (const cl_object o){
  return (size_t)new double(ecl_to_double(o));
}


size_t cldouble (const cl_object o);
size_t v_of_cldouble (const cl_object o){
  std::vector< double > result(ecl_to_int(cl_length(o)));
  for (size_t i=0; i < result.size(); i++) {
    cl_object index(c_string_to_object(std::to_string(i).c_str()));
    double *element = (double*)cldouble(cl_aref(2, o, index));
    result[i] = *element;
  }
  return (size_t)new std::vector< double >(result);
}


template
struct SumVector< double >;
size_t cs_nil (const cl_object o);
size_t s_sum_vector_with_cldouble (const cl_object o){
  return (size_t)new SumVector< double >{
    {
      *(std::vector< double >*)v_of_cldouble(cl_getf(2, cl_getf(2, o, c_string_to_object(":IN")), c_string_to_object(":VECTOR")))
    } /* unnamed */
  ,
    {
      *(double*)cldouble(cl_getf(2, cl_getf(2, o, c_string_to_object(":OUT")), c_string_to_object(":SUM")))
    } /* unnamed */
  };
}

