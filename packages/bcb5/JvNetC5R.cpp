//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvNetC5R.res");
USEUNIT("..\..\run\JvStrToHtml.pas");
USEUNIT("..\..\run\JvFormToHtml.pas");
USEUNIT("..\..\run\JvFtpGrabber.pas");
USEUNIT("..\..\run\JvHtmlParser.pas");
USEUNIT("..\..\run\JvHttpGrabber.pas");
USEUNIT("..\..\run\JvMail.pas");
USEUNIT("..\..\run\JvMultiHttpGrabber.pas");
USEUNIT("..\..\run\JvRgbToHtml.pas");
USEUNIT("..\..\run\JvRichEditToHtml.pas");
USEUNIT("..\..\run\JvStringListToHtml.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
//---------------------------------------------------------------------------
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

