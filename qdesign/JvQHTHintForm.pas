{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHTHintEditor.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Design-time Hint Editor

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQHTHintForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QControls, QForms, QStdCtrls, Types, QWindows,  
  DesignIntf, DesignEditors, 
  JvQHint, JvQComponent;

type
  TJvHintEditor = class(TJvForm)
    HintMemo: TMemo;
    Label1: TLabel;
    BtnOk: TButton;
    BtnCancel: TButton;
    HintLabel: TLabel;
    procedure HintMemoChange(Sender: TObject);
    procedure HintMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  public
  end;

  TJvHintProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation



{$R *.xfm}


//=== { TJvHintProperty } ====================================================

procedure TJvHintProperty.Edit;
var
  OldHintWindowClass: THintWindowClass;
begin
  OldHintWindowClass := HintWindowClass;
  with TJvHintEditor.Create(Application) do
    try
      HintMemo.Text := GetValue;
      HintWindowClass := TJvHTHintWindow;
      if ShowModal = mrOk then
        SetValue(HintMemo.Text);
    finally 
      Free;
      HintWindowClass := OldHintWindowClass;
      { recreate hint window }
      Application.ShowHint := not Application.ShowHint;
      Application.ShowHint := not Application.ShowHint;
    end;
end;

function TJvHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

//=== { TJvHintEditor } ======================================================

procedure TJvHintEditor.HintMemoChange(Sender: TObject);
begin
  HintLabel.Hint := HintMemo.Text;
end;

procedure TJvHintEditor.HintMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //if Key = VK_ESCAPE then
  if Ord(Key) = 27 then   //asn: With VisualCLX VK_ESCAPE <> 27
    BtnCancel.Click;
end;

end.
