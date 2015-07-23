#include "Binary.hxx"

#include "cppuhelper/bootstrap.hxx"
#include "uno/dispatcher.h"
#include "com/sun/star/uno/Any.hxx"

using ::com::sun::star::uno::Any;

css::uno::Reference<css::uno::XComponentContext> g_context;
css::uno::Mapping g_cpp2uno;
css::uno::Mapping g_uno2cpp;

extern "C"
void * bootstrap()
{
    g_context = cppu::defaultBootstrap_InitialComponentContext();
    g_cpp2uno = css::uno::Mapping(
                css::uno::Environment::getCurrent(),
                css::uno::Environment(UNO_LB_UNO));
    g_uno2cpp = css::uno::Mapping(
                css::uno::Environment(UNO_LB_UNO),
                css::uno::Environment::getCurrent());
    css::uno::Reference<css::uno::XComponentContext> * result =
        new css::uno::Reference<css::uno::XComponentContext>(g_context);
    return static_cast<void *>(result);
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

extern "C"
bool anyToBool (uno_Any * any) {
    bool a;
    *static_cast< Any * >(any) >>= a;
    return a;
}

extern "C"
sal_Int32 anyToInt32 (uno_Any * any) {
    sal_Int32 a;
    *static_cast< Any * >(any) >>= a;
    return a;
}

extern "C"
sal_Int64 anyToInt64 (uno_Any * any) {
    sal_Int64 a;
    *static_cast< Any * >(any) >>= a;
    return a;
}

extern "C"
css::uno::Reference< css::uno::XInterface > * anyToInterface ( uno_Any * any) {
    void * cppIface = g_uno2cpp.mapInterface(any->pReserved, any->pType);
    css::uno::Reference< com::sun::star::uno::XInterface > result
        (static_cast< css::uno::XInterface * >(cppIface), css::uno::UNO_QUERY);
    return new css::uno::Reference< com::sun::star::uno::XInterface >(result);
}
