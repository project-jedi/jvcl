//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

USERES("JvCryptC5R.res");
USEUNIT("..\..\run\JvCabFile.pas");
USEUNIT("..\..\run\JvCaesarCipher.pas");
USEUNIT("..\..\run\JvGenetic.pas");
USEUNIT("..\..\run\JvSerialMaker.pas");
USEUNIT("..\..\run\JvVigenereCipher.pas");
USEUNIT("..\..\run\JvXorCipher.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");

#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------

