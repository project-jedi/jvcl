//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvUIBC5R.res");
USEUNIT("..\..\run\JvUIB.pas");
USEUNIT("..\..\run\JvUIBase.pas");
USEUNIT("..\..\run\JvUIBConst.pas");
USEUNIT("..\..\run\JvUIBDataSet.pas");
USEUNIT("..\..\run\JvUIBError.pas");
USEUNIT("..\..\run\JvUIBLib.pas");
USEUNIT("..\..\run\JvUIBMetaData.pas");
USEUNIT("..\..\run\JvUIBSQLParser.pas");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vcldb50.bpi");

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

