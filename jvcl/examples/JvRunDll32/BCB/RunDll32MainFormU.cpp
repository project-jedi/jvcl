//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RunDll32MainFormU.h"
#include "InfoFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TRunDll32MainForm *RunDll32MainForm;

/*
{
RunDLL32.EXE shell32.dll,Control_FillCache_RunDLL
RunDLL32.EXE shell32.dll,Control_RunDLL

RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,1
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,2
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,3
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,4
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,5

RunDLL32.EXE shell32.dll,Control_RunDLL ports.cpl

RunDLL32.EXE shell32.dll,Control_RunDLL appwiz.cpl,,1
RunDLL32.EXE shell32.dll,Control_RunDLL appwiz.cpl,,2
RunDLL32.EXE shell32.dll,Control_RunDLL appwiz.cpl,,3

RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,-1
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,0
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,1
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,2
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,3

RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL Image
RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL Infrared

RunDLL32.EXE shell32.dll,Control_RunDLL odbccp32.cpl

RunDLL32.EXE shell32.dll,Control_RunDLL joy.cpl
RunDLL32.EXE shell32.dll,Control_RunDLL sysdm.cpl @1
RunDLL32.EXE shell32.dll,Control_RunDLL findfast.cpl

RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL <DeviceID>
RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL,,0
RunDLL32.EXE devmgr.dll DeviceManager_Execute
RunDLL32.EXE SHELL32.DLL,OpenAs_RunDLL <FileName>
RunDLL32.EXE SHELL32.DLL,SHHelpShortcuts_RunDLL <FontsFolder>
RunDLL32.EXE shell32.dll,Control_RunDLL main.cpl @3

RunDLL32.EXE SYNCUI.DLL,Briefcase_Create
RunDLL32.EXE syncui.dll,Briefcase_Intro


RunDLL32.EXE USER.DLL,cascadechildwindows
RunDLL32.EXE USER.DLL,tilechildwindows

RunDLL32.EXE RNAUI.DLL,RnaWizard
RunDLL32.EXE RNAUI.DLL,RnaWizard @ 1
RunDLL32.EXE Rnaui.dll,RnaDial <ConnectionName>

RunDLL32.EXE DISKCOPY.DLL,DiskCopyRunDll
RunDLL32.EXE SHELL32.DLL,SHFormatDrive
RunDLL32.EXE USER.DLL,repaintscreen
}

*/
//---------------------------------------------------------------------------
__fastcall TRunDll32MainForm::TRunDll32MainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
AnsiString TRunDll32MainForm::GetWinSysDir(void)
{
 AnsiString stResult;
 char buf[MAX_PATH+1];

  GetSystemDirectory((LPTSTR  )buf, MAX_PATH);
  stResult.sprintf("%s",buf);

  return stResult;
}
//---------------------------------------------------------------------------

AnsiString TRunDll32MainForm::GetExpandedSysFilePath(const AnsiString S)
{
 AnsiString stResult;
  if( ExtractFilePath(S) == "" )
  {
    stResult = IncludeTrailingPathDelimiter(GetWinSysDir()) + S;
  }
  else
  {
    stResult = S;
  }

  return stResult;
}
//---------------------------------------------------------------------------

bool TRunDll32MainForm::PipeToFile(AnsiString FileName, AnsiString Cmd, bool Append)
{
  TStartupInfo SI;
  TProcessInformation PI;
  char buf[MAX_PATH+1];
  bool flRes;
  String Appnd;
  String ComSpec;

//  procedure GetComSpec;
    memset(&buf,0,MAX_PATH+1);
    if( GetEnvironmentVariable("COMSPEC",(LPTSTR)&buf, MAX_PATH) == 0 )
    {
      ComSpec = "C:\command.com"; // best guess ?
    }
    else
    {
      ComSpec.sprintf("%s",buf);
    }
// end of GetComSpec;
  SI.cb = sizeof(TStartupInfo);
  GetStartupInfo(&SI);
  SI.wShowWindow = SW_HIDE;
  if( Append )
  {
    Appnd = " >>";
  }
  else
  {
    Appnd = " >";
  }
  Cmd = ComSpec + " /C " + Cmd + Appnd + FileName;
  flRes = CreateProcess(NULL, Cmd.c_str(), NULL, NULL, false, 0, NULL, NULL, &SI, &PI);
  try
  {
    if( flRes )
    {
      WaitForInputIdle(PI.hProcess, INFINITE);
      flRes = (WaitForSingleObject(PI.hProcess, INFINITE) != WAIT_FAILED);
    }
  }
  __finally
  {
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  }
  if(!flRes)
  {
    RaiseLastOSError();
  }

  return flRes;
}
//---------------------------------------------------------------------------

void __fastcall TRunDll32MainForm::Label1Click(TObject *Sender)
{
  OpenObject("http://www.dx21.com/scripting/rundll32/");
}
//---------------------------------------------------------------------------
void __fastcall TRunDll32MainForm::btnRunClick(TObject *Sender)
{
  if(!RunDLL32(edModule->Text, edFunc->Text, edCmdLine->Text, chkWait->Checked))
  {
    RaiseLastOSError();
  }
}
//---------------------------------------------------------------------------


void __fastcall TRunDll32MainForm::edModuleChange(TObject *Sender)
{
  btnInfo->Enabled = FileExists(GetExpandedSysFilePath(edModule->Text));
}
//---------------------------------------------------------------------------


void __fastcall TRunDll32MainForm::btnInternalClick(TObject *Sender)
{
 HWND H;

  if( chkWait->Checked )
  {
    H = Handle;
  }
  else
  {
    H = GetDesktopWindow();
  }

  RunDll32Internal(reinterpret_cast<unsigned int >(H),edModule->Text,edFunc->Text,edCmdLine->Text,SW_SHOWDEFAULT);

}
//---------------------------------------------------------------------------

void __fastcall TRunDll32MainForm::btnBrowseClick(TObject *Sender)
{
  TOpenDialog *pOD;

  pOD = new TOpenDialog(NULL);
  try
  {
    pOD->Filter = "Modules|*.exe;*.dll;*.ocx;|All files|*.*";
    pOD->FileName = edModule->Text;
    if( ExtractFilePath(pOD->FileName) == "" )
    {
      pOD->InitialDir = GetWinSysDir();
    }
    else
    {
      pOD->InitialDir = ExtractFilePath(pOD->FileName);
    }
    if( pOD->Execute() )
    {
      edModule->Text = pOD->FileName;
    }
  }
  __finally
  {
    delete pOD;
  }
}
//---------------------------------------------------------------------------

void __fastcall TRunDll32MainForm::btnInfoClick(TObject *Sender)
{
 String tmp;

  tmp = ChangeFileExt(Application->ExeName, ".dmp");
  // NOTE: this requires the $(DELPHI)\bin folder and the tdump.exe utility in the PATH to work!!!
  PipeToFile(tmp, Format("tdump.exe -ee -q %s",  OPENARRAY(TVarRec, (GetExpandedSysFilePath(edModule->Text) ) ) ), false);
  TfrmInfo_View(tmp, ExtractFileName(edModule->Text));

}
//---------------------------------------------------------------------------

