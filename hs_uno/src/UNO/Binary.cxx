#include "Binary.hxx"

#include "cppuhelper/bootstrap.hxx"
#include "uno/dispatcher.h"
#include "com/sun/star/uno/Any.hxx"
#include "sal/main.h"
#include "com/sun/star/uno/Sequence.hxx"

using ::com::sun::star::uno::Any;

extern "C"
void * bootstrap(int argc, char ** argv)
{
    sal_detail_initialize(argc, argv);
    css::uno::Reference< css::uno::XComponentContext > context;
    context = cppu::defaultBootstrap_InitialComponentContext();
    css::uno::Mapping cpp2uno ( css::uno::Environment::getCurrent(),
        css::uno::Environment(UNO_LB_UNO) );
    void * unoContext = cpp2uno.mapInterface( context.get(),
        cppu::UnoType< com::sun::star::uno::XComponentContext >::get() );
    assert(unoContext != 0);
    return unoContext;
}

extern "C"
uno_Interface * hsunoQueryInterfaceByName (uno_Interface * iface,
    rtl_uString * psName)
{
  typelib_TypeDescription * td = 0;
  typelib_typedescription_getByName(((typelib_TypeDescription **)&td), psName);
  uno_Interface * ret = hsunoQueryInterface(iface,
      (typelib_TypeDescriptionReference *)td);
  typelib_typedescription_release(td);
  return ret;
}

extern "C"
uno_Interface * hsunoQueryInterface (uno_Interface * iface,
    typelib_TypeDescriptionReference * pType)
{
  uno_Any result, exception;
  uno_Any * pException = &exception;
  void * arguments [1];
  arguments[0] = &pType;
  makeBinaryUnoCall(iface, "com.sun.star.uno.XInterface::queryInterface",
      &result, arguments, &pException);
  assert(pException == 0); // TODO handle exception
  // FIXME the following code may leak due to the use of an Any
  if (result.pType->eTypeClass == typelib_TypeClass_INTERFACE)
    return (uno_Interface *)result.pReserved;
  return 0;
}


extern "C"
uno_Interface * hsunoCreateInstanceWithContext (rtl_uString * sServiceSpecifier,
    uno_Interface * pContext)
{
  rtl_uString_acquire(sServiceSpecifier);

  uno_Any exception;
  uno_Any * pException = &exception;

  uno_Interface * xComponentContext = 0;
  uno_Interface * pServiceManager = 0;
  {
    // FIXME the queryInterface should not be needed
    rtl_uString * sXComponentContext = 0;
    rtl_uString_newFromAscii(&sXComponentContext,
        "com.sun.star.uno.XComponentContext");
    xComponentContext = hsunoQueryInterfaceByName(pContext, sXComponentContext);
    rtl_uString_release(sXComponentContext);
    makeBinaryUnoCall(xComponentContext,
    //makeBinaryUnoCall(pContext,
        "com.sun.star.uno.XComponentContext::getServiceManager",
        &pServiceManager, NULL, &pException);
    assert(pException == 0); // TODO handle exception
  }

  uno_Interface * ret = 0;
  {
    pException = &exception;
    void * arguments [2];
    arguments[0] = &sServiceSpecifier;
    arguments[1] = &xComponentContext;
    makeBinaryUnoCall(pServiceManager,
        "com.sun.star.lang.XMultiComponentFactory::createInstanceWithContext",
        &ret, arguments, &pException);
    assert(pException == 0); // TODO handle exception
  }

  rtl_uString_release(sServiceSpecifier);
  return ret;
}

extern "C"
uno_Interface * hsunoCreateInstanceWithContextFromAscii (
    const char * sServiceSpecifier, uno_Interface * pContext)
{
  rtl_uString * str = 0;
  rtl_uString_newFromAscii(&str, sServiceSpecifier);
  uno_Interface * ret = hsunoCreateInstanceWithContext(str, pContext);
  rtl_uString_release(str);
  return ret;
}

extern "C"
void hsunoGetSingletonFromContext (
    rtl_uString * sSingletonSpecifier, uno_Interface * pContext, uno_Any * result)
{
    void * arguments [1];
    arguments[0] = &sSingletonSpecifier;
    uno_Any exception;
    uno_Any * pException = &exception;
    makeBinaryUnoCall(pContext,
        "com.sun.star.uno.XComponentContext::getValueByName", result,
        arguments, &pException);
    assert(pException == 0); // TODO handle exception
    return;
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
    // TODO check that it is indeed an interface
    return any->pReserved;
}

extern "C" {

int hsuno_any_structSize () {
  return sizeof(uno_Any);
}

int hsuno_any_getTypeClass (uno_Any * pAny) {
  return pAny->pType->eTypeClass;
}

rtl_uString * hsuno_any_getTypeName (uno_Any * pAny) {
  return pAny->pType->pTypeName;
}

void * hsuno_any_getValue (uno_Any * pAny) {
  return pAny->pData;
}

void hsuno_any_destruct (uno_Any * pAny, uno_ReleaseFunc release) {
  uno_any_destruct(pAny, release);
}

} // extern "C"

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
