#ifndef HSUNO_UNO_BINARY_H
#define HSUNO_UNO_BINARY_H

#include "com/sun/star/uno/XComponentContext.hpp"
#include "uno/any2.h"
#include "uno/mapping.hxx"

#include "com/sun/star/uno/Exception.hpp"
#include <string>

using ::com::sun::star::uno::Exception;

extern "C"
uno_Interface * hsunoQueryInterface (uno_Interface * iface,
    typelib_TypeDescriptionReference * pType);

extern "C"
uno_Interface * hsunoQueryInterfaceByName (uno_Interface * iface,
    rtl_uString * psName);

extern "C"
uno_Interface * hsunoCreateInstanceWithContext (rtl_uString * sServiceSpecifier,
    uno_Interface * pContext);

extern "C"
uno_Interface * hsunoCreateInstanceWithContextFromAscii (
    const char * sServiceSpecifier, uno_Interface * pContext);

extern "C"
void makeBinaryUnoCall(
    uno_Interface * interface, char const * methodType, void * result,
    void ** arguments, uno_Any ** exception);

/** UNO Any Functions */

#ifdef __cplusplus
extern "C" {
#endif

/** The size of an uno_Any C structure.
 */
int hsuno_any_structSize ();

/** Retrieve the type class the value contained within the Any.
 */
int hsuno_any_getTypeClass (uno_Any * pAny);

/** Retrieve the type name the value contained within the Any.
 */
rtl_uString * hsuno_any_getTypeName (uno_Any * pAny);

/** Retrieve the value contained within the Any.
 *
 * Note: when the value is an interface, it returns a pointer to it.
 */
void * hsuno_any_getValue (uno_Any * pAny);

/** Destroys an Any, releasing the interface it contains if any.
 */
void hsuno_any_destruct (uno_Any * pAny, uno_ReleaseFunc release);

#ifdef __cplusplus
}
#endif

// TODO clean the funtions below.

extern "C"
bool anyToBool (uno_Any * any);

extern "C"
sal_Int32 anyToInt32 (uno_Any * any);

extern "C"
sal_Int64 anyToInt64 (uno_Any * any);

extern "C"
void * anyToInterface (uno_Any * any, bool unoMap = true);

#endif // HSUNO_UNO_BINARY_H
