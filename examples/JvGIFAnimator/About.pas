{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit About;

{$I jvcl.inc}

interface

uses Classes, Graphics, Controls;

procedure ShowAbout(const Developer, Company: string;
  HiVer, LoVer, Year: Integer);
procedure ShowAboutDialog(const AppTitle, Developer, Company: string;
  AppIcon: TIcon; HiVer, LoVer, Year: Integer);
procedure VerAboutBox;

implementation

uses
  Windows, SysUtils, Messages, Consts, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,
  JvJVCLUtils, JvVersionInfo, JvConsts, JvJCLUtils;

{$IFDEF COMPILER3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  sYear = '%s © %d';
  sFreeMemory = '%s KB';
  sAbout = 'About';
{$IFDEF WIN32}
  sFreeMemoryLabel = 'Physical memory:';
  sFreeResourcesLabel = 'Memory in use:';
{$ELSE}
  sFreeMemoryLabel = 'Free memory:';
  sFreeResourcesLabel = 'Free system resources:';
{$ENDIF WIN32}
  sVersion = 'Version %d.%.2d';
  sFileVer = 'Version %s';
  sExeFile = 'Executable file %s';

{ TAboutDialog }

type
  TAboutDialog = class(TForm)
    FProgramIcon: TImage;
    FProductName: TLabel;
    FProductVersion: TLabel;
    FCopyright: TLabel;
    FYearLabel: TLabel;
    FMemSize: TLabel;
    FPercent: TLabel;
  private
    procedure UpdateMemoryInfo;
    function GetWindowsVersion: string;
    procedure SetVersion(LoVer, HiVer: Integer);
    procedure SetCopyright(const Developer, Company: string; Year: Integer);
    procedure SetAppData(const AppTitle: string; AIcon: TIcon);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ Utility routines }

function MakeAboutDialog: TForm;
begin
  Result := TAboutDialog.Create(Application);
  with Result do
    { scale to screen res }
    if Screen.PixelsPerInch <> 96 then begin
      ScaleBy(Screen.PixelsPerInch, 96);
      { The ScaleBy method does not scale the font well, so set the
        font back to the original info. }
      Font.Name := 'MS Sans Serif';
      Font.Size := 8;
      Font.Style := [];
      Font.Color := clWindowText;
      Left := (Screen.Width div 2) - (Width div 2);
      Top := (Screen.Height div 2) - (Height div 2);
    end;
end;

procedure VerAboutBox;
const
  DefVersion = '0.09';
  SCopyright = 'Copyright © %d';
var
  JvVerInf: TJvVersionInfo ;
begin
  JvVerInf := TJvVersionInfo .Create(Application.ExeName);
  try
    with TAboutDialog(MakeAboutDialog) do
    try
      if JvVerInf.Valid then begin
        FYearLabel.Caption := JvVerInf.LegalCopyright;
        if FYearLabel.Caption = '' then
          FYearLabel.Caption := Format(SCopyright,
            [ExtractYear(JvVerInf.VerFileDate)]);
        FProductVersion.Caption := Format(sFileVer, [JvVerInf.FileVersion]);
      end
      else begin
        FYearLabel.Caption := Format(SCopyright,
          [ExtractYear(JvVerInf.VerFileDate)]);
        FProductVersion.Caption := Format(sFileVer, [DefVersion]);
      end;
      FCopyright.Caption := Format(sExeFile,
        [FormatDateTime('c', JvVerInf.VerFileDate)]);
      SetAppData(Application.Title, Application.Icon);
      ShowModal;
    finally
      Free;
    end;
  finally
    JvVerInf.Free;
  end
end;

procedure ShowAboutDialog(const AppTitle, Developer, Company: string;
  AppIcon: TIcon; HiVer, LoVer, Year: Integer);
begin
  with TAboutDialog(MakeAboutDialog) do
  try
    SetVersion(LoVer, HiVer);
    SetCopyright(Developer, Company, Year);
    SetAppData(AppTitle, AppIcon);
    ShowModal;
  finally
    Free;
  end;
end;

procedure ShowAbout(const Developer, Company: string;
  HiVer, LoVer, Year: Integer);
begin
  ShowAboutDialog(Application.Title, Developer, Company, Application.Icon,
    HiVer, LoVer, Year);
end;

{ TAboutDialog }

constructor TAboutDialog.Create(AOwner: TComponent);
begin
{$IFDEF BCB}
  inherited CreateNew(AOwner, 0);
{$ELSE}
  inherited CreateNew(AOwner);
{$ENDIF BCB}
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientHeight := 143;
  ClientWidth := 339;
  Caption := sAbout;

  with Font do begin
{$IFNDEF WIN32}
    Color := clWindowText;
    Size := 8;
    Name := 'MS Sans Serif';
{$ENDIF}
    Style := [];
  end;

  FProgramIcon := TImage.Create(Self);
  with FProgramIcon do begin
    Parent := Self;
    Left := 12;
    Top := 8;
    Width := 32;
    Height := 32;
    AutoSize := True;
  end;

  FProductName := TLabel.Create(Self);
  with FProductName do begin
    Parent := Self;
    Left := 60;
    Top := 6;
    Width := 205;
    Height := 13;
    ParentFont := True;
    ShowAccelChar := False;
  end;

  FProductVersion := TLabel.Create(Self);
  with FProductVersion do begin
    Parent := Self;
    Left := 60;
    Top := 23;
    Width := 205;
    Height := 13;
    ParentFont := True;
    Caption := sVersion;
  end;

  FCopyright := TLabel.Create(Self);
  with FCopyright do begin
    Parent := Self;
    Left := 60;
    Top := 57;
    Width := 273;
    Height := 13;
    ParentFont := True;
  end;

  FYearLabel := TLabel.Create(Self);
  with FYearLabel do begin
    Parent := Self;
    Left := 60;
    Top := 40;
    Width := 273;
    Height := 13;
    ParentFont := True;
  end;

  with TBevel.Create(Self) do begin
    Parent := Self;
    Shape := bsTopLine;
    Style := bsLowered;
    Left := 60;
    Top := 81;
    Width := 273;
    Height := 2;
  end;

  with TLabel.Create(Self) do begin
    Parent := Self;
    Left := 60;
    Top := 91;
    Width := 269;
    Height := 13;
    ParentFont := True;
    Caption := GetWindowsVersion;
  end;

  with TLabel.Create(Self) do begin
    Parent := Self;
    Left := 60;
    Top := 107;
    Width := 130;
    Height := 13;
    ParentFont := True;
    Caption := sFreeMemoryLabel;
  end;

  FMemSize := TLabel.Create(Self);
  with FMemSize do begin { free memory }
    Parent := Self;
    Left := 210;
    Top := 107;
    Width := 125;
    Height := 13;
    ParentFont := True;
  end;

  with TLabel.Create(Self) do begin
    Parent := Self;
    Left := 60;
    Top := 123;
    Width := 130;
    Height := 13;
    ParentFont := True;
    Caption := sFreeResourcesLabel;
  end;

  FPercent := TLabel.Create(Self);
  with FPercent do begin { free resources or memory in use }
    Parent := Self;
    Left := 210;
    Top := 123;
    Width := 125;
    Height := 13;
    ParentFont := True;
  end;

  with TButton.Create(Self) do begin
    Parent := Self;
    Left := 272;
    Top := 6;
    Width := 61;
    Height := 25;
    Caption := SOkButton;
    Cursor := crHandPoint;
    Default := False;
    Cancel := True;
    ModalResult := mrOk;
  end;
  UpdateMemoryInfo;
end;

procedure TAboutDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

procedure TAboutDialog.SetCopyright(const Developer, Company: string;
  Year: Integer);
begin
  FYearLabel.Caption := Format(sYear, [Company, Year]);
  FCopyright.Caption := Developer;
end;

procedure TAboutDialog.SetVersion(LoVer, HiVer: Integer);
begin
  FProductVersion.Caption := Format(sVersion, [HiVer, LoVer]);
end;

procedure TAboutDialog.UpdateMemoryInfo;
{$IFDEF WIN32}
var
  MemStatus: TMemoryStatus;
{$ENDIF}
begin
{$IFDEF WIN32}
  MemStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MemStatus);
  FMemSize.Caption := Format(sFreeMemory, [FormatFloat(',0.##',
    MemStatus.dwTotalPhys / 1024.0)]);
  FPercent.Caption := Format('%d %%', [MemStatus.dwMemoryLoad]);
{$ELSE}
  FMemSize.Caption := Format(sFreeMemory, [FormatFloat(',0.##',
    GetFreeSpace(0) / 1024.0)]);
  FPercent.Caption := Format('%d %%',
    [GetFreeSystemResources(GFSR_SYSTEMRESOURCES)]);
{$ENDIF}
end;

function TAboutDialog.GetWindowsVersion: string;
begin
  Result := JvJCLUtils.GetWindowsVersion;
end;

procedure TAboutDialog.SetAppData(const AppTitle: string; AIcon: TIcon);
begin
  if (AIcon <> nil) and not AIcon.Empty then Icon := AIcon
  else Icon.Handle := LoadIcon(0, IDI_APPLICATION);
  FProductName.Caption := AppTitle;
  FProgramIcon.Picture.Icon := Icon;
end;

end.
