{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppUtils.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAppUtils;

interface

uses
  Windows, Registry, Classes, Controls, Forms, Grids, IniFiles,
  {$IFDEF COMPILER6_UP}
  RTLConsts, { Variants, }
  {$ENDIF}
  JvVCLUtils; {, JvComponent}

function GetDefaultSection(Component: TComponent): string;
{$IFDEF WIN32}
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean);
{$ELSE}
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string);
{$ENDIF}
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;

{$IFDEF WIN32}
var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;

function GetDefaultIniRegKey: string;
{$ENDIF}

function FindForm(FormClass: TFormClass): TForm;
function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
function ShowDialog(FormClass: TFormClass): Boolean;
function InstantiateForm(FormClass: TFormClass; var Reference): TForm;

{$IFDEF WIN32}
procedure SaveFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
procedure RestoreFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
procedure WriteFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string);
procedure ReadFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
procedure SaveMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
procedure RestoreMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
procedure RestoreGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
procedure SaveGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
{$ELSE}
procedure SaveFormPlacement(Form: TForm; const IniFileName: string);
procedure RestoreFormPlacement(Form: TForm; const IniFileName: string);
{$ENDIF WIN32}

procedure WriteFormPlacement(Form: TForm; IniFile: TCustomIniFile;
  const Section: string);
procedure ReadFormPlacement(Form: TForm; IniFile: TCustomIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
procedure SaveMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
procedure RestoreMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile);
procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile);

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

function StrToIniStr(const Str: string): string;
function IniStrToStr(const Str: string): string;

function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  Default: Longint): Longint;
procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
procedure IniReadSections(IniFile: TObject; Strings: TStrings);
procedure IniEraseSection(IniFile: TObject; const Section: string);
procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);

{$IFDEF WIN32}
procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);
{$ELSE}
procedure AppBroadcast(Msg, wParam: Word; lParam: Longint);
{$ENDIF WIN32}

procedure AppTaskbarIcons(AppOnly: Boolean);

{ Internal using utilities }

procedure InternalSaveGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
procedure InternalRestoreGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
procedure InternalSaveMDIChildren(MainForm: TForm; IniFile: TObject);
procedure InternalRestoreMDIChildren(MainForm: TForm; IniFile: TObject);

implementation

uses
  SysUtils, Messages, Consts,
  JvTypes, JvStrUtils, JvFileUtil, JvPlacemnt;

const
  CrLf = #13#10;

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then
  begin
    if Component is TCustomForm then
      Result := Component.ClassName
    else
    begin
      Result := Component.Name;
      if Component is TControl then
      begin
        F := GetParentForm(TControl(Component));
        if F <> nil then
          Result := F.ClassName + Result
        else
        begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else
      begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else
    Result := '';
end;

function GetDefaultIniName: string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result:= OnGetDefaultIniName
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.INI'));
end;

{$IFDEF WIN32}
function GetDefaultIniRegKey: string;
begin
  if RegUseAppTitle and (Application.Title <> '') then
    Result := Application.Title
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  if DefCompanyName <> '' then
    Result := DefCompanyName + '\' + Result;
  Result := 'Software\' + Result;
end;
{$ENDIF}

{$IFDEF WIN32}
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean);
{$ELSE}
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string);
{$ENDIF}
var
  I: Integer;
begin
  IniFileName := '';
  with Control do
    if Owner is TCustomForm then
      for I := 0 to Owner.ComponentCount - 1 do
        if Owner.Components[I] is TJvFormPlacement then
        begin
          IniFileName := TJvFormPlacement(Owner.Components[I]).IniFileName;
          Break;
        end;
  Section := GetDefaultSection(Control);
  if IniFileName = '' then
    {$IFDEF WIN32}
    if UseRegistry then
      IniFileName := GetDefaultIniRegKey
    else
      IniFileName := GetDefaultIniName;
    {$ELSE}
    IniFileName := GetDefaultIniName;
    {$ENDIF}
end;

function FindForm(FormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is FormClass then
    begin
      Result := Screen.Forms[I];
      Break;
    end;
  end;
end;

function InternalFindShowForm(FormClass: TFormClass;
  const Caption: string; Restore: Boolean): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is FormClass then
      if (Caption = '') or (Caption = Screen.Forms[I].Caption) then
      begin
        Result := Screen.Forms[I];
        Break;
      end;
  end;
  if Result = nil then
  begin
    Application.CreateForm(FormClass, Result);
    if Caption <> '' then
      Result.Caption := Caption;
  end;
  with Result do
  begin
    if Restore and (WindowState = wsMinimized) then
      WindowState := wsNormal;
    Show;
  end;
end;

function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
begin
  Result := InternalFindShowForm(FormClass, Caption, True);
end;

function ShowDialog(FormClass: TFormClass): Boolean;
var
  Dlg: TForm;
begin
  Application.CreateForm(FormClass, Dlg);
  try
    Result := Dlg.ShowModal in [mrOk, mrYes];
  finally
    Dlg.Free;
  end;
end;

function InstantiateForm(FormClass: TFormClass; var Reference): TForm;
begin
  if TForm(Reference) = nil then
    Application.CreateForm(FormClass, Reference);
  Result := TForm(Reference);
end;

// (rom) use StrStringToEscaped, StrEscapedToString from JclStrings.pas

function StrToIniStr(const Str: string): string;
var
  N: Integer;
begin
  Result := Str;
  repeat
    N := Pos(CrLf, Result);
    if N > 0 then
      Result := Copy(Result, 1, N-1) + '\n' + Copy(Result, N+2, Length(Result));
  until N = 0;
  repeat
    N := Pos(#10#13, Result);
    if N > 0 then
      Result := Copy(Result, 1, N-1) + '\n' + Copy(Result, N+2, Length(Result));
  until N = 0;
end;

function IniStrToStr(const Str: string): string;
var
  N: Integer;
begin
  Result := Str;
  repeat
    N := Pos('\n', Result);
    if N > 0 then
      Result := Copy(Result, 1, N-1) + CrLf + Copy(Result, N+2, Length(Result));
  until N = 0;
end;

{ The following strings should not be localized }
const
  siFlags     = 'Flags';
  siShowCmd   = 'ShowCmd';
  siMinMaxPos = 'MinMaxPos';
  siNormPos   = 'NormPos';
  siPixels    = 'PixelsPerInch';
  siMDIChild  = 'MDI Children';
  siListCount = 'Count';
  siItem      = 'Item%d';

function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadString(Section, Ident, Default)
  else
  {$ENDIF}
  if IniFile is TCustomIniFile then
    Result := TCustomIniFile(IniFile).ReadString(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
var
  S: string;
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
  {$ENDIF}
  begin
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TCustomIniFile then
      TCustomIniFile(IniFile).WriteString(Section, Ident, S);
  end;
end;

function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  Default: Longint): Longint;
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
  {$ENDIF}
  if IniFile is TCustomIniFile then
    Result := TCustomIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else
  {$ENDIF}
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).WriteInteger(Section, Ident, Value);
end;

function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
  {$ENDIF}
  if IniFile is TCustomIniFile then
    Result := TCustomIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteBool(Section, Ident, Value)
  else
  {$ENDIF}
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).WriteBool(Section, Ident, Value);
end;

procedure IniEraseSection(IniFile: TObject; const Section: string);
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).EraseSection(Section)
  else
  {$ENDIF}
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).EraseSection(Section);
end;

procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
{$IFNDEF WIN32}
var
  CSection: array [0..127] of Char;
  CIdent: array [0..127] of Char;
  CFileName: array [0..127] of Char;
{$ENDIF}
begin
  {$IFDEF WIN32}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).DeleteKey(Section, Ident)
  else
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).DeleteKey(Section, Ident);
  {$ELSE}
  if IniFile is TCustomIniFile then begin
    WritePrivateProfileString(StrPLCopy(CSection, Section, SizeOf(CSection) - 1),
      StrPLCopy(CIdent, Ident, SizeOf(CIdent) - 1), nil,
      StrPLCopy(CFileName, TCustomIniFile(IniFile).FileName, SizeOf(CFileName) - 1));
  end;
  {$ENDIF}
end;

{$IFNDEF WIN32}
procedure IniFileReadSections(IniFile: TCustomIniFile; Strings: TStrings);
const
  BufSize = 8192;
var
  CFileName: array [0..127] of Char;
  Buffer, P: PChar;
begin
  GetMem(Buffer, BufSize);
  try
    Strings.BeginUpdate;
    try
      Strings.Clear;
      if GetPrivateProfileString(nil, nil, nil, Buffer, BufSize,
        StrPLCopy(CFileName, IniFile.FileName, SizeOf(CFileName) - 1)) <> 0 then
      begin
        P := Buffer;
        while P^ <> #0 do
        begin
          Strings.Add(StrPas(P));
          Inc(P, StrLen(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;
{$ENDIF}

procedure IniReadSections(IniFile: TObject; Strings: TStrings);
begin
  {$IFDEF WIN32}
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).ReadSections(Strings)
  else
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).ReadSections(Strings);
  {$ELSE}
  if IniFile is TCustomIniFile then
    IniFileReadSections(TCustomIniFile(IniFile), Strings);
  {$ENDIF}
end;

procedure InternalSaveMDIChildren(MainForm: TForm; IniFile: TObject);
var
  I: Integer;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(ResStr(SNoMDIForm));
  IniEraseSection(IniFile, siMDIChild);
  if MainForm.MDIChildCount > 0 then
  begin
    IniWriteInteger(IniFile, siMDIChild, siListCount,
      MainForm.MDIChildCount);
    for I := 0 to MainForm.MDIChildCount - 1 do
      IniWriteString(IniFile, siMDIChild, Format(siItem, [I]),
        MainForm.MDIChildren[I].ClassName);
  end;
end;

procedure InternalRestoreMDIChildren(MainForm: TForm; IniFile: TObject);
var
  I: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(ResStr(SNoMDIForm));
  StartWait;
  try
    Count := IniReadInteger(IniFile, siMDIChild, siListCount, 0);
    if Count > 0 then
    begin
      for I := 0 to Count - 1 do
      begin
        FormClass := TFormClass(GetClass(IniReadString(IniFile, siMDIChild,
          Format(siItem, [Count - I - 1]), '')));
        if FormClass <> nil then
          InternalFindShowForm(FormClass, '', False);
      end;
    end;
  finally
    StopWait;
  end;
end;

{$IFDEF WIN32}

procedure SaveMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
begin
  InternalSaveMDIChildren(MainForm, IniFile);
end;

procedure RestoreMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
begin
  InternalRestoreMDIChildren(MainForm, IniFile);
end;

{$ENDIF WIN32}

procedure SaveMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
begin
  InternalSaveMDIChildren(MainForm, IniFile);
end;

procedure RestoreMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
begin
  InternalRestoreMDIChildren(MainForm, IniFile);
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    IniWriteInteger(IniFile, Section, Format(siItem, [I]),
      TDrawGrid(Grid).ColWidths[I]);
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid; IniFile: TObject;
  const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[I] := IniReadInteger(IniFile, Section,
      Format(siItem, [I]), TDrawGrid(Grid).ColWidths[I]);
end;

{$IFDEF WIN32}

procedure RestoreGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
begin
  InternalRestoreGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure SaveGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
begin
  InternalSaveGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

{$ENDIF WIN32}

procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile);
begin
  InternalRestoreGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile);
begin
  InternalSaveGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

function CrtResString: string;
begin
  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN),
    GetSystemMetrics(SM_CYSCREEN)]);
end;

function ReadPosStr(IniFile: TObject; const Section, Ident: string): string;
begin
  Result := IniReadString(IniFile, Section, Ident + CrtResString, '');
  if Result = '' then
    Result := IniReadString(IniFile, Section, Ident, '');
end;

procedure WritePosStr(IniFile: TObject; const Section, Ident, Value: string);
begin
  IniWriteString(IniFile, Section, Ident + CrtResString, Value);
  IniWriteString(IniFile, Section, Ident, Value);
end;

procedure InternalWriteFormPlacement(Form: TForm; IniFile: TObject;
  const Section: string);
var
  Placement: TWindowPlacement;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if (Form = Application.MainForm) and IsIconic(Application.Handle) then
      ShowCmd := SW_SHOWMINIMIZED;
    if (FormStyle = fsMDIChild) and (WindowState = wsMinimized) then
      Flags := Flags or WPF_SETMINPOSITION;
    IniWriteInteger(IniFile, Section, siFlags, Flags);
    IniWriteInteger(IniFile, Section, siShowCmd, ShowCmd);
    IniWriteInteger(IniFile, Section, siPixels, Screen.PixelsPerInch);
    WritePosStr(IniFile, Section, siMinMaxPos, Format('%d,%d,%d,%d',
      [ptMinPosition.X, ptMinPosition.Y, ptMaxPosition.X, ptMaxPosition.Y]));
    WritePosStr(IniFile, Section, siNormPos, Format('%d,%d,%d,%d',
      [rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Right,
      rcNormalPosition.Bottom]));
  end;
end;

{$IFDEF WIN32}
procedure WriteFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string);
begin
  InternalWriteFormPlacement(Form, IniFile, Section);
end;
{$ENDIF WIN32}

procedure WriteFormPlacement(Form: TForm; IniFile: TCustomIniFile;
  const Section: string);
begin
  InternalWriteFormPlacement(Form, IniFile, Section);
end;

{$IFDEF WIN32}
procedure SaveFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
{$ELSE}
procedure SaveFormPlacement(Form: TForm; const IniFileName: string);
{$ENDIF WIN32}
var
  IniFile: TObject;
begin
  {$IFDEF WIN32}
  if UseRegistry then
    IniFile := TRegIniFile.Create(IniFileName)
  else
    IniFile := TIniFile.Create(IniFileName);
  {$ELSE}
  IniFile := TIniFile.Create(IniFileName);
  {$ENDIF WIN32}
  try
    InternalWriteFormPlacement(Form, IniFile, Form.ClassName);
  finally
    IniFile.Free;
  end;
end;

{$IFDEF WIN32}
  {$HINTS OFF}
{$ENDIF}

type

{*******************************************************}
{ !! ATTENTION Nasty implementation                     }
{*******************************************************}
{                                                       }
{ This class definition was copied from FORMS.PAS.      }
{ It is needed to access some private fields of TForm.  }
{                                                       }
{ Any changes in the underlying classes may cause       }
{ errors in this implementation!                        }
{                                                       }
{*******************************************************}

  TJvNastyForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    {$IFDEF COMPILER4_UP}
    FSizeChanging: Boolean;
    {$ENDIF}
    FWindowState: TWindowState; { !! }
  end;

  TJvHackComponent = class(TComponent);
{$IFDEF WIN32}
  {$HINTS ON}
{$ENDIF}

procedure InternalReadFormPlacement(Form: TForm; IniFile: TObject;
  const Section: string; LoadState, LoadPosition: Boolean);
const
  Delims = [',', ' '];
var
  PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound: Boolean;
begin
  if not (LoadState or LoadPosition) then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if not IsWindowVisible(Form.Handle) then
      ShowCmd := SW_HIDE;
    if LoadPosition then
    begin
      DataFound := False;
      Flags := IniReadInteger(IniFile, Section, siFlags, Flags);
      PosStr := ReadPosStr(IniFile, Section, siMinMaxPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
        ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
        ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
      end;
      PosStr := ReadPosStr(IniFile, Section, siNormPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Left);
        rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Top);
        rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Left + Width);
        rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Top + Height);
      end;
      if Screen.PixelsPerInch <> IniReadInteger(IniFile, Section, siPixels,
        Screen.PixelsPerInch) then
        DataFound := False;
      if DataFound then
      begin
        if not (BorderStyle in [bsSizeable {$IFDEF WIN32}, bsSizeToolWin {$ENDIF}]) then
          rcNormalPosition := Rect(rcNormalPosition.Left, rcNormalPosition.Top,
            rcNormalPosition.Left + Width, rcNormalPosition.Top + Height);
        if rcNormalPosition.Right > rcNormalPosition.Left then
        begin
          if (Position in [poScreenCenter {$IFDEF COMPILER4_UP}, poDesktopCenter {$ENDIF}]) and
            not (csDesigning in ComponentState) then
          begin
            TJvHackComponent(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              TJvHackComponent(Form).SetDesigning(False);
            end;
          end;
          SetWindowPlacement(Handle, @Placement);
        end;
      end;
    end;
    if LoadState then
    begin
      WinState := wsNormal;
      { default maximize MDI main form }
      if ((Application.MainForm = Form) {$IFDEF COMPILER4_UP} or
        (Application.MainForm = nil) {$ENDIF}) and ((FormStyle = fsMDIForm) or
        ((FormStyle = fsNormal) and (Position = poDefault))) then
        WinState := wsMaximized;
      ShowCmd := IniReadInteger(IniFile, Section, siShowCmd, SW_HIDE);
      case ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE:
          WinState := wsMaximized;
      end;
      {$IFDEF WIN32}
      if (WinState = wsMinimized) and ((Form = Application.MainForm)
        {$IFDEF COMPILER4_UP} or (Application.MainForm = nil) {$ENDIF}) then
      begin
        TJvNastyForm(Form).FWindowState := wsNormal;
        PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
      {$ENDIF}
      if FormStyle in [fsMDIChild, fsMDIForm] then
        TJvNastyForm(Form).FWindowState := WinState
      else
        WindowState := WinState;
    end;
    Update;
  end;
end;

{$IFDEF WIN32}
procedure ReadFormPlacementReg(Form: TForm; IniFile: TRegIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
begin
  InternalReadFormPlacement(Form, IniFile, Section, LoadState, LoadPosition);
end;
{$ENDIF WIN32}

procedure ReadFormPlacement(Form: TForm; IniFile: TCustomIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);
begin
  InternalReadFormPlacement(Form, IniFile, Section, LoadState, LoadPosition);
end;

{$IFDEF WIN32}
procedure RestoreFormPlacement(Form: TForm; const IniFileName: string;
  UseRegistry: Boolean);
{$ELSE}
procedure RestoreFormPlacement(Form: TForm; const IniFileName: string);
{$ENDIF}
var
  IniFile: TObject;
begin
  {$IFDEF WIN32}
  if UseRegistry then
  begin
    IniFile := TRegIniFile.Create(IniFileName);
    {$IFDEF COMPILER5_UP}
    TRegIniFile(IniFile).Access := KEY_READ;
    {$ENDIF}
  end
  else
    IniFile := TIniFile.Create(IniFileName);
  {$ELSE}
  IniFile := TIniFile.Create(IniFileName);
  {$ENDIF WIN32}
  try
    InternalReadFormPlacement(Form, IniFile, Form.ClassName, True, True);
  finally
    IniFile.Free;
  end;
end;

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;
var
  CurrentName: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to MaxInt do
  begin
    CurrentName := Format(FileNameMask, [I]);
    if not FileExists(NormalDir(Path) + CurrentName) then
    begin
      Result := CurrentName;
      Exit;
    end;
  end;
end;

{$IFDEF WIN32}
procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);
{$ELSE}
procedure AppBroadcast(Msg, wParam: Word; lParam: Longint);
{$ENDIF WIN32}
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    SendMessage(Screen.Forms[I].Handle, Msg, wParam, lParam);
end;

procedure AppTaskbarIcons(AppOnly: Boolean);
var
  Style: Longint;
begin
  Style := GetWindowLong(Application.Handle, GWL_STYLE);
  if AppOnly then
    Style := Style or WS_CAPTION
  else
    Style := Style and not WS_CAPTION;
  SetWindowLong(Application.Handle, GWL_STYLE, Style);
  if AppOnly then
    SwitchToWindow(Application.Handle, False);
end;

end.
