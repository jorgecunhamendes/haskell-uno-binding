#ifndef HSUNO_UNO_BINARY_H
#define HSUNO_UNO_BINARY_H

#include "com/sun/star/uno/XComponentContext.hpp"
#include "uno/any2.h"
#include "uno/mapping.hxx"

#include "com/sun/star/uno/Exception.hpp"
#include <string>

using ::com::sun::star::uno::Exception;

extern css::uno::Reference<css::uno::XComponentContext> g_context;
extern css::uno::Mapping g_cpp2uno;
extern css::uno::Mapping g_uno2cpp;

extern "C"
void makeBinaryUnoCall(
    uno_Interface * interface, char const * methodType, void * result,
    void ** arguments, uno_Any ** exception);

extern "C"
bool anyToBool (uno_Any * any);

extern "C"
sal_Int32 anyToInt32 (uno_Any * any);

extern "C"
sal_Int64 anyToInt64 (uno_Any * any);

extern "C"
css::uno::Reference< css::uno::XInterface > * anyToInterface (uno_Any * any);

#endif // HSUNO_UNO_BINARY_H
