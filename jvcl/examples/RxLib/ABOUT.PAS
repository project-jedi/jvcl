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

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, JvComponent, JvLabel,
  JvSecretPanel, JvExControls, JvExExtCtrls;

type
  TAboutForm = class(TForm)
    SecretPanel1: TJvSecretPanel ;
    AppIcon: TImage;
    WebLabel: TJvLabel ;
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TBitBtn;
    Label3: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AppIconDblClick(Sender: TObject);
    procedure SecretPanel1DblClick(Sender: TObject);
    procedure LabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure RxWebSite;

implementation

uses JvConsts, ShellAPI;

{$R *.DFM}

procedure RxWebSite; 
begin
  ShellExecute(Application.Handle, nil, 'http://jvcl.sf.net', nil,
    nil, SW_SHOWNOACTIVATE);
end;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  AppIcon.Picture.Icon := Application.Icon;
  AppIcon.Cursor := crHandPoint;
  WebLabel.Cursor := crHandPoint;
end;

procedure TAboutForm.AppIconDblClick(Sender: TObject);
begin
  SecretPanel1.Active := True;
end;

procedure TAboutForm.SecretPanel1DblClick(Sender: TObject);
begin
  SecretPanel1.Active := False;
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
  RxWebSite;
end;

end.
