{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHTHintEditor.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Design-time Hint Editor

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvHTHintEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  {$IFDEF COMPILER6_UP}
   DesignIntf, DesignEditors,
  {$ELSE}
   DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvHint;

type
  TJvHintEditor = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    bOk: TButton;
    bCancel: TButton;
    Label2: TLabel;
    procedure Memo1Change(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TJvHintProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$R *.DFM}

procedure TJvHintProperty.Edit;
var
  OldHintWindowClass: THintWindowClass;
begin
  OldHintWindowClass := HintWindowClass;
  with TJvHintEditor.Create(Application) do
    try
      Memo1.Text := GetValue;
      HintWindowClass := TJvHTHintWindow;
      if ShowModal = mrOk then
        SetValue(Memo1.Text);
    finally 
      Free;
      HintWindowClass := OldHintWindowClass;
     { recreate hint window }
      Application.ShowHint := not Application.ShowHint;
      Application.ShowHint := not Application.ShowHint;
    end;    { try/finally }
end;

function TJvHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;


procedure TJvHintEditor.Memo1Change(Sender: TObject);
begin
  Label2.Hint := Memo1.Text;
end;

procedure TJvHintEditor.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then bCancel.Click;
end;

end.
