{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [ralfgspam@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au

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

unit JvDialogsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvBaseDlg, JvPasswordForm, StdCtrls, JvExchListboxes,
  JvLoginDlg, JvNagScreen, JvSerialDlg, JvTipsOfDay, JvImageDlg,
  JvCalculator, JvProgressDlg, JvCommonDialogD, JvDiskPrompt, JvCopyError,
  JvDeleteError, JvPageSetup, JvPageSetupTitled, JvSHFmt, JvObjPickerComp,
  JvCalc;

type
  TJvDialogs = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    JvPasswordForm1: TJvPasswordForm;
    JvExchListboxes1: TJvExchListboxes;
    JvLoginDlg1: TJvLoginDlg;
    JvSerialDlg1: TJvSerialDlg;
    JvImageDlg1: TJvImageDlg;
    JvCalculator1: TJvCalculator;
    JvProgressDlg1: TJvProgressDlg;
    JvDiskPrompt1: TJvDiskPrompt;
    JvCopyError1: TJvCopyError;
    JvDeleteError1: TJvDeleteError;
    JvPageSetupDialog1: TJvPageSetupDialog;
    JvPageSetupTitledDialog1: TJvPageSetupTitledDialog;
    JvFormatDrive1: TJvFormatDrive;
    JvObjectPickerDialog1: TJvObjectPickerDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
  private
    { Private-Deklarationen }

  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

procedure TJvDialogs.Button1Click(Sender: TObject);
begin
  JvPasswordForm1.Execute;
end;

procedure TJvDialogs.Button2Click(Sender: TObject);
begin
  JvExchListboxes1.Execute;
end;

procedure TJvDialogs.Button3Click(Sender: TObject);
begin
  JvLoginDlg1.execute;
end;

procedure TJvDialogs.Button5Click(Sender: TObject);
var
  nag : TJvNagScreen;
begin
 Nag := TJvNagScreen.Create(NIL);
 nag.Picture.LoadFromResourceName(HINSTANCE,'NAGPIC');
 nag.Execute;
 nag.free;
end;

procedure TJvDialogs.Button4Click(Sender: TObject);
begin
  JvSerialDlg1.Execute;
end;

procedure TJvDialogs.Button6Click(Sender: TObject);
var
  TOD : TJvTipsofday;

begin
  TOD := TJvTipsOfDay.Create(NIL);
  TOD.Hints.Append('Hintline 1');
  TOD.Hints.Append('Hintline 2');
  TOD.Hints.Append('Hintline 3');
  TOD.Hints.Append('Hintline 4');
  TOD.Execute;
  tod.free;
end;

procedure TJvDialogs.Button7Click(Sender: TObject);
begin
  JvImageDlg1.Execute;
end;

procedure TJvDialogs.Button8Click(Sender: TObject);
begin
  JvCalculator1.Execute;
end;

procedure TJvDialogs.Button9Click(Sender: TObject);
var
  i : integer;
begin
  jvProgressdlg1.Text := 'Progressing....';
  JvProgressDlg1.Show;
  JvProgressDlg1.StartProgression;
  for I := 0 to 100 do
  begin
    jvProgressDlg1.Value := I;
    sleep(100);
  end;
  jvProgressdlg1.Close;
end;

procedure TJvDialogs.Button10Click(Sender: TObject);
begin
  JvDiskPrompt1.Execute;
end;

procedure TJvDialogs.Button11Click(Sender: TObject);
begin
  JvCopyError1.Execute;
end;

procedure TJvDialogs.Button12Click(Sender: TObject);
begin
  JvDeleteError1.Execute;
end;

procedure TJvDialogs.Button13Click(Sender: TObject);
begin
  JvPageSetupDialog1.Execute;
end;

procedure TJvDialogs.Button14Click(Sender: TObject);
begin
  JvPageSetupTitledDialog1.execute;
end;

procedure TJvDialogs.Button16Click(Sender: TObject);
begin
  JvFormatDrive1.Execute;
end;

procedure TJvDialogs.Button17Click(Sender: TObject);
begin
 JvObjectPickerDialog1.Execute;
end;

end.

