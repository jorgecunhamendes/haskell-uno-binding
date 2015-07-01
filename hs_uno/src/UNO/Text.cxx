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
