//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("JvSIMDCpuInfo.pas", Jvsimdcpuinfo, FormCpuInfo);
USEFORMNS("JvSIMDModifyForm.pas", Jvsimdmodifyform, JvSIMDModifyFrm);
USEFORMNS("JvSIMDViewForm.pas", Jvsimdviewform, JvSIMDViewFrm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//  Source du paquet.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
