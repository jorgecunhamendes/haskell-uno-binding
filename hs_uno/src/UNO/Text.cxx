#include "Text.h"

extern "C"
OUString * create_oustring (sal_Unicode * buf, sal_Int32 len) {
	return new OUString(buf, len);
}

extern "C"
const sal_Unicode * oustring_buffer (OUString * str) {
	return str->getStr();
}

extern "C"
sal_Int32 oustring_length (OUString * str) {
	return str->getLength();
}

extern "C"
void delete_oustring (OUString * str) {
  delete str;
}

extern "C"
rtl_uString * oustringGetUString (OUString * str) {
  return (str->pData);
}

extern "C"
OUString * oustringFromUString (rtl_uString * ustr) {
  OUString * str = new OUString (ustr, SAL_NO_ACQUIRE);
  return str;
}

extern "C" {

// FIXME This can be written directly in Haskell.
rtl_uString * hsuno_uString_new (sal_Unicode * buf, sal_Int32 len) {
    rtl_uString * str = 0;
    rtl_uString_newFromStr_WithLength(&str, buf, len);
    return str;
}

}
