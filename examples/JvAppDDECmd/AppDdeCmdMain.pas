{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

unit AppDdeCmdMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, JvDdeCmd, JvComponent;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    JvAppDdeCmd1: TJvAppDdeCmd;
    procedure PvAppDdeCmd1ExecParsedCmd(Sender: TObject;
      const Command: string; Parameters: TStrings);
    procedure Button1Click(Sender: TObject);
    procedure PvAppDdeCmd1BusyChanged(Sender: TObject; IsBusy: Boolean);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

uses AppDdeCmdModal;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PvAppDdeCmd1BusyChanged(nil, False);
end;

procedure TMainForm.PvAppDdeCmd1ExecParsedCmd(Sender: TObject;
  const Command: string; Parameters: TStrings);
var
  I: Integer;
  S: String;
begin
  with Memo1.Lines do
  begin
    Add(Format('Command: %s', [Command]));
    for I := 0 to Parameters.Count - 1 do
    begin
      S := #9 + Parameters.Strings[I];
      if Assigned(Parameters.Objects[I]) then S := S + ' <- Integer Value';
      Add(S);
    end;
    Add('');
  end;
end;

procedure TMainForm.PvAppDdeCmd1BusyChanged(Sender: TObject; IsBusy: Boolean);
const
  EnabledStr: array[Boolean] of String = ('Ready', 'Busy');
begin
  Label1.Caption := EnabledStr[IsBusy];
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

end.
