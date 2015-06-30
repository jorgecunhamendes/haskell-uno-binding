#include "Binary.hxx"

#include "cppuhelper/bootstrap.hxx"
#include "uno/dispatcher.h"
#include "com/sun/star/uno/Any.hxx"

using ::com::sun::star::uno::Any;

css::uno::Reference<css::uno::XComponentContext> g_context;
css::uno::Mapping g_cpp2uno;

extern "C"
void bootstrap()
{
    g_context = cppu::defaultBootstrap_InitialComponentContext();
    g_cpp2uno = css::uno::Mapping(
                css::uno::Environment::getCurrent(),
                css::uno::Environment(UNO_LB_UNO));
}

extern "C"
void makeBinaryUnoCall(
    uno_Interface * interface, char const * methodType, void * result,
    void ** arguments, uno_Any ** exception)
{
    typelib_TypeDescription * td = 0;
    css::uno::Type(css::uno::TypeClass_INTERFACE_METHOD, methodType)
        .getDescription(&td);
    assert(td != 0); // for now, just assert
    (*interface->pDispatcher)(interface, td, result, arguments, exception);
    typelib_typedescription_release(td);
}
