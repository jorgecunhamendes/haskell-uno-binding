#ifndef HSUNO_UNO_BINARY_H
#define HSUNO_UNO_BINARY_H

#include "com/sun/star/uno/XComponentContext.hpp"
#include "com/sun/star/util/XMacroExpander.hpp"
#include "uno/any2.h"
#include "uno/mapping.hxx"

#include "com/sun/star/uno/Exception.hpp"
#include <string>

using ::com::sun::star::uno::Exception;

extern css::uno::Reference<css::uno::XComponentContext> g_context;
extern css::uno::Mapping g_cpp2uno;

extern "C"
void makeBinaryUnoCall(
    uno_Interface * interface, char const * methodType, void * result,
    void ** arguments, uno_Any ** exception);


#endif // HSUNO_UNO_BINARY_H
