{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit InstallLabelMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QImgList, JvQInstallLabel, JvQComponent, JvQExControls;

type
  TInstallLabelMainForm = class(TForm)
    ImageList1: TImageList;
    Next: TButton;
    Button1: TButton;
    Image1: TImage;
    Panel1: TPanel;
    InstallLabel1: TJvInstallLabel;
    procedure NextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FIndex: Integer;
  end;

var
  InstallLabelMainForm: TInstallLabelMainForm;

implementation

{$R *.xfm}

procedure TInstallLabelMainForm.NextClick(Sender: TObject);
begin
  Next.Caption := '&Next >>';
  if FIndex = 0 then { first line, so clear all others }
    InstallLabel1.SetExclusive(FIndex, 0, [fsBold])
  else
  begin
    InstallLabel1.SetStyle(FIndex, 0, [fsBold]); { current line is an arrow }
    InstallLabel1.SetStyle(FIndex - 1, 1, []); { prev line is a check mark }

    if FIndex = InstallLabel1.Lines.Count - 1 then { this is the last line }
    begin
      FIndex := -1; { incremented below...}
      Next.Caption := '&Again...';
    end;
  end;

  Inc(FIndex);
end;

procedure TInstallLabelMainForm.FormCreate(Sender: TObject);
begin
  FIndex := 0;
end;

procedure TInstallLabelMainForm.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

