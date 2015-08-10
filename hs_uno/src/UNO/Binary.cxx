#include "Binary.hxx"

#include "cppuhelper/bootstrap.hxx"
#include "uno/dispatcher.h"
#include "com/sun/star/uno/Any.hxx"
#include "sal/main.h"
#include "com/sun/star/uno/Sequence.hxx"

using ::com::sun::star::uno::Any;

css::uno::Reference<css::uno::XComponentContext> g_context;
css::uno::Mapping g_cpp2uno;
css::uno::Mapping g_uno2cpp;

extern "C"
void * bootstrap(int argc, char ** argv)
{
    sal_detail_initialize(argc, argv);
    g_context = cppu::defaultBootstrap_InitialComponentContext();
    g_cpp2uno = css::uno::Mapping(
                css::uno::Environment::getCurrent(),
                css::uno::Environment(UNO_LB_UNO));
    g_uno2cpp = css::uno::Mapping(
                css::uno::Environment(UNO_LB_UNO),
                css::uno::Environment::getCurrent());
    return static_cast<void *>(g_context.get());
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


// Any

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
void * anyToInterface ( uno_Any * any, bool unoMap) {
    void * cppIface;
    if (unoMap)
      cppIface = g_uno2cpp.mapInterface(any->pReserved, any->pType);
    else
      cppIface = any->pReserved;
    return cppIface;
}

extern "C"
void destructAny (uno_Any * pAny, uno_ReleaseFunc release) {
  uno_any_destruct(pAny, release);
}

// Sequence

extern "C"
sal_Int32 unoSequenceGetLength (uno_Sequence const * pSequence) {
  return pSequence->nElements;
}

extern "C"
const void * unoSequenceGetArray (uno_Sequence const * pSequence) {
  return static_cast< const void * >(pSequence->elements);
}

extern "C"
void unoSequenceRelease (typelib_TypeDescription * td, uno_Sequence * pSequence)
{
    if (osl_atomic_decrement( &pSequence->nRefCount ) == 0) {
        uno_type_sequence_destroy( pSequence,
            reinterpret_cast< typelib_TypeDescriptionReference * >(td),
            (uno_ReleaseFunc)com::sun::star::uno::cpp_release );
    }
}

// Interface

extern "C"
void cpp_acquire (void * pCppI) {
  com::sun::star::uno::cpp_acquire(pCppI);
}

extern "C"
void cpp_release (void * pCppI) {
  com::sun::star::uno::cpp_release(pCppI);
}
