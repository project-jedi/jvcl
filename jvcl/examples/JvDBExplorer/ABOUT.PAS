{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit About;
{$J+}
interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, JvComponent,
  JvSecretPanel, JvExExtCtrls;

type
  TAboutDlg = class(TForm)
    Image1: TImage;
    Shape1: TShape;
    WinVer: TLabel;
    FreeMem: TLabel;
    SecretPanel: TJvSecretPanel ;
    Label1: TLabel;
    VersionLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAbout;

const
  SDbxVersion: string = '';

implementation

uses
  JvVersionInfo, JvConsts, JvJCLUtils, JclWin32;

{$R *.DFM}

procedure ShowAbout;
begin
  with TAboutDlg.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TAboutDlg.FormCreate(Sender: TObject);
const
  sFreeMem = 'Physical memory:        %s M';
var
  Mem: Int64;
  MemStatus: TMemoryStatusEx;
begin
  Image1.Cursor := crHandPoint;
  VersionLabel.Caption := Format(VersionLabel.Caption, [SDbxVersion]);
  WinVer.Caption := GetWindowsVersionString;
  MemStatus.dwLength := SizeOf(TMemoryStatusEx);
  GlobalMemoryStatusEx(MemStatus);
  Mem := MemStatus.ullTotalPhys;
  FreeMem.Caption := Format(sFreeMem, [FormatFloat(',0.##', Mem / 1024.0 / 1024.0)]);
end;

procedure TAboutDlg.Image1DblClick(Sender: TObject);
begin
  SecretPanel.Active := not SecretPanel.Active;
end;

initialization
  with TJvVersionInfo.Create(Application.ExeName) do
  try
    if Valid then
      SDbxVersion := Format('%d.%d', [FileLongVersion.All[2],
        FileLongVersion.All[1]])
    else SDbxVersion := '1.0';
  finally
    Free;
  end;
end.
