{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License 
Version 1.1 (the "License"); you may not use this file except in compliance 
with the License. You may obtain a copy of the License at 
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, 
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for 
the specific language governing rights and limitations under the License. 

The Original Code is: JvVirtualKeySelection.PAS, released 2003-07-05.

The Initial Developer of the Original Code is Olivier Sannier <obones@meloo.com>
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): none to date


Last Modified: 2003-08-17;

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description: This unit defines a frame that you can use to select a key code.
             The primary use for that frame is un conjunction with a TJvAVICapture
             component   

Known Issues: none known
-----------------------------------------------------------------------------}

unit JvVirtualKeySelectionFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF USE_DXGETTEXT}
  gnugettext,
{$ENDIF}
  StdCtrls;

type
  TJvVirtualKeySelectionFrame = class(TFrame)
    cmbVirtualKey: TComboBox;
    lblVirtualKey: TLabel;
    chkShift: TCheckBox;
    chkCtrl: TCheckBox;
    lblModifiers: TLabel;
  protected
    FShiftState : TShiftState;

    function GetKeyCode: Word;
    function GetShiftState: TShiftState;
    function GetCombinedKeyCode: Word;
    procedure SetCombinedKeyCode(const Value: Word);
    procedure EnumKeys;
  public
    constructor Create(AOwner: TComponent); override;
    property CombinedKeyCode : Word read GetCombinedKeyCode write SetCombinedKeyCode;
    property KeyCode : Word read GetKeyCode;
    property ShiftState : TShiftState read GetShiftState;
  end;

implementation

uses
  JvResources;

{$R *.dfm}

constructor TJvVirtualKeySelectionFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF USE_DXGETTEXT}
  TranslateComponent(Self);
  {$ENDIF}

  EnumKeys;
end;

procedure TJvVirtualKeySelectionFrame.EnumKeys;
var i : integer;
begin
  with cmbVirtualKey.Items do
  begin
    // add the easy ones
    for i := ord('0') to ord('9') do
      AddObject('VK_'+chr(i),  TObject(i));
    for i := ord('A') to ord('Z') do
      AddObject('VK_'+chr(i),  TObject(i));

    // then add the others...
    AddObject('VK_LBUTTON',    TObject(VK_LBUTTON));
    AddObject('VK_RBUTTON',    TObject(VK_RBUTTON));
    AddObject('VK_CANCEL',     TObject(VK_CANCEL));
    AddObject('VK_MBUTTON',    TObject(VK_MBUTTON));
    AddObject('VK_BACK',       TObject(VK_BACK));
    AddObject('VK_TAB',        TObject(VK_TAB));
    AddObject('VK_CLEAR',      TObject(VK_CLEAR));
    AddObject('VK_RETURN',     TObject(VK_RETURN));
    AddObject('VK_SHIFT',      TObject(VK_SHIFT));
    AddObject('VK_CONTROL',    TObject(VK_CONTROL));
    AddObject('VK_MENU',       TObject(VK_MENU));
    AddObject('VK_PAUSE',      TObject(VK_PAUSE));
    AddObject('VK_CAPITAL',    TObject(VK_CAPITAL));
    AddObject('VK_KANA',       TObject(VK_KANA));
    AddObject('VK_HANGUL',     TObject(VK_HANGUL));
    AddObject('VK_JUNJA',      TObject(VK_JUNJA));
    AddObject('VK_FINAL',      TObject(VK_FINAL));
    AddObject('VK_HANJA',      TObject(VK_HANJA));
    AddObject('VK_KANJI',      TObject(VK_KANJI));
    AddObject('VK_CONVERT',    TObject(VK_CONVERT));
    AddObject('VK_NONCONVERT', TObject(VK_NONCONVERT));
    AddObject('VK_ACCEPT',     TObject(VK_ACCEPT));
    AddObject('VK_MODECHANGE', TObject(VK_MODECHANGE));
    AddObject('VK_ESCAPE',     TObject(VK_ESCAPE));
    AddObject('VK_SPACE',      TObject(VK_SPACE));
    AddObject('VK_PRIOR',      TObject(VK_PRIOR));
    AddObject('VK_NEXT',       TObject(VK_NEXT));
    AddObject('VK_END',        TObject(VK_END));
    AddObject('VK_HOME',       TObject(VK_HOME));
    AddObject('VK_LEFT',       TObject(VK_LEFT));
    AddObject('VK_UP',         TObject(VK_UP));
    AddObject('VK_RIGHT',      TObject(VK_RIGHT));
    AddObject('VK_DOWN',       TObject(VK_DOWN));
    AddObject('VK_SELECT',     TObject(VK_SELECT));
    AddObject('VK_PRINT',      TObject(VK_PRINT));
    AddObject('VK_EXECUTE',    TObject(VK_EXECUTE));
    AddObject('VK_SNAPSHOT',   TObject(VK_SNAPSHOT));
    AddObject('VK_INSERT',     TObject(VK_INSERT));
    AddObject('VK_DELETE',     TObject(VK_DELETE));
    AddObject('VK_HELP',       TObject(VK_HELP));
    AddObject('VK_LWIN',       TObject(VK_LWIN));
    AddObject('VK_RWIN',       TObject(VK_RWIN));
    AddObject('VK_APPS',       TObject(VK_APPS));
    AddObject('VK_NUMPAD0',    TObject(VK_NUMPAD0));
    AddObject('VK_NUMPAD1',    TObject(VK_NUMPAD1));
    AddObject('VK_NUMPAD2',    TObject(VK_NUMPAD2));
    AddObject('VK_NUMPAD3',    TObject(VK_NUMPAD3));
    AddObject('VK_NUMPAD4',    TObject(VK_NUMPAD4));
    AddObject('VK_NUMPAD5',    TObject(VK_NUMPAD5));
    AddObject('VK_NUMPAD6',    TObject(VK_NUMPAD6));
    AddObject('VK_NUMPAD7',    TObject(VK_NUMPAD7));
    AddObject('VK_NUMPAD8',    TObject(VK_NUMPAD8));
    AddObject('VK_NUMPAD9',    TObject(VK_NUMPAD9));
    AddObject('VK_MULTIPLY',   TObject(VK_MULTIPLY));
    AddObject('VK_ADD',        TObject(VK_ADD));
    AddObject('VK_SEPARATOR',  TObject(VK_SEPARATOR));
    AddObject('VK_SUBTRACT',   TObject(VK_SUBTRACT));
    AddObject('VK_DECIMAL',    TObject(VK_DECIMAL));
    AddObject('VK_DIVIDE',     TObject(VK_DIVIDE));
    AddObject('VK_F1',         TObject(VK_F1));
    AddObject('VK_F2',         TObject(VK_F2));
    AddObject('VK_F3',         TObject(VK_F3));
    AddObject('VK_F4',         TObject(VK_F4));
    AddObject('VK_F5',         TObject(VK_F5));
    AddObject('VK_F6',         TObject(VK_F6));
    AddObject('VK_F7',         TObject(VK_F7));
    AddObject('VK_F8',         TObject(VK_F8));
    AddObject('VK_F9',         TObject(VK_F9));
    AddObject('VK_F10',        TObject(VK_F10));
    AddObject('VK_F11',        TObject(VK_F11));
    AddObject('VK_F12',        TObject(VK_F12));
    AddObject('VK_F13',        TObject(VK_F13));
    AddObject('VK_F14',        TObject(VK_F14));
    AddObject('VK_F15',        TObject(VK_F15));
    AddObject('VK_F16',        TObject(VK_F16));
    AddObject('VK_F17',        TObject(VK_F17));
    AddObject('VK_F18',        TObject(VK_F18));
    AddObject('VK_F19',        TObject(VK_F19));
    AddObject('VK_F20',        TObject(VK_F20));
    AddObject('VK_F21',        TObject(VK_F21));
    AddObject('VK_F22',        TObject(VK_F22));
    AddObject('VK_F23',        TObject(VK_F23));
    AddObject('VK_F24',        TObject(VK_F24));
    AddObject('VK_NUMLOCK',    TObject(VK_NUMLOCK));
    AddObject('VK_SCROLL',     TObject(VK_SCROLL));
    AddObject('VK_LSHIFT',     TObject(VK_LSHIFT));
    AddObject('VK_RSHIFT',     TObject(VK_RSHIFT));
    AddObject('VK_LCONTROL',   TObject(VK_LCONTROL));
    AddObject('VK_RCONTROL',   TObject(VK_RCONTROL));
    AddObject('VK_LMENU',      TObject(VK_LMENU));
    AddObject('VK_RMENU',      TObject(VK_RMENU));
    AddObject('VK_PROCESSKEY', TObject(VK_PROCESSKEY));
    AddObject('VK_ATTN',       TObject(VK_ATTN));
    AddObject('VK_CRSEL',      TObject(VK_CRSEL));
    AddObject('VK_EXSEL',      TObject(VK_EXSEL));
    AddObject('VK_EREOF',      TObject(VK_EREOF));
    AddObject('VK_PLAY',       TObject(VK_PLAY));
    AddObject('VK_ZOOM',       TObject(VK_ZOOM));
    AddObject('VK_NONAME',     TObject(VK_NONAME));
    AddObject('VK_PA1',        TObject(VK_PA1));
    AddObject('VK_OEM_CLEAR',  TObject(VK_OEM_CLEAR));
  end;
end;

function TJvVirtualKeySelectionFrame.GetCombinedKeyCode: Word;
begin
  Result := GetKeyCode;
  if chkShift.Checked then
    Result := Result or $4000;
  if chkCtrl.Checked then
    Result := Result or $8000;
end;

function TJvVirtualKeySelectionFrame.GetKeyCode: Word;
begin
  if cmbVirtualKey.ItemIndex = -1 then
  begin
    // Signal an error, but not sure how...
    Application.MessageBox(PChar(sNoValidKeyCode), PChar(sInvalidKeyCode), MB_ICONERROR);

    // in any case, return 0
    Result := 0;
  end
  else
  begin
    Result := Word(cmbVirtualKey.Items.Objects[cmbVirtualKey.ItemIndex]);
  end;
end;

function TJvVirtualKeySelectionFrame.GetShiftState: TShiftState;
begin
  Result := [];
    if chkShift.Checked then
      Result := Result + [ssShift];
    if chkCtrl.Checked then
      Result := Result + [ssCtrl];
end;

procedure TJvVirtualKeySelectionFrame.SetCombinedKeyCode(
  const Value: Word);
begin
  chkShift.Checked := (Value and $4000) <> 0;
  chkCtrl.Checked := (Value and $8000) <> 0;
  cmbVirtualKey.ItemIndex := cmbVirtualKey.Items.IndexOfObject(TObject(Value and $FF));
end;

end.
