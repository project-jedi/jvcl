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

Contributor(s):


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description: This unit defines a frame that you can use to select a key code.
             The primary use for that frame is un conjunction with a TJvAVICapture
             component

Known Issues: none known
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvVirtualKeySelectionFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF USE_DXGETTEXT}
  gnugettext,
  {$ENDIF USE_DXGETTEXT}
  StdCtrls;

type
  TJvVirtualKeySelectionFrame = class(TFrame)
    cmbVirtualKey: TComboBox;
    lblVirtualKey: TLabel;
    chkShift: TCheckBox;
    chkCtrl: TCheckBox;
    lblModifiers: TLabel;
  protected
    function GetKeyCode: Word;
    function GetShiftState: TShiftState;
    function GetCombinedKeyCode: Word;
    procedure SetCombinedKeyCode(const Value: Word);
    procedure EnumKeys;
  public
    constructor Create(AOwner: TComponent); override;
    property CombinedKeyCode: Word read GetCombinedKeyCode write SetCombinedKeyCode;
    property KeyCode: Word read GetKeyCode;
    property ShiftState: TShiftState read GetShiftState;
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
  {$ENDIF USE_DXGETTEXT}
  EnumKeys;
end;

procedure TJvVirtualKeySelectionFrame.EnumKeys;
type
  TKeyElem = record
    Name: PChar;
    Value: Word;
  end;
const
  VirtualKeys: array [0..100] of TKeyElem =
   (
    (Name: 'VK_LBUTTON';    Value: VK_LBUTTON),
    (Name: 'VK_RBUTTON';    Value: VK_RBUTTON),
    (Name: 'VK_CANCEL';     Value: VK_CANCEL),
    (Name: 'VK_MBUTTON';    Value: VK_MBUTTON),
    (Name: 'VK_BACK';       Value: VK_BACK),
    (Name: 'VK_TAB';        Value: VK_TAB),
    (Name: 'VK_CLEAR';      Value: VK_CLEAR),
    (Name: 'VK_RETURN';     Value: VK_RETURN),
    (Name: 'VK_SHIFT';      Value: VK_SHIFT),
    (Name: 'VK_CONTROL';    Value: VK_CONTROL),
    (Name: 'VK_MENU';       Value: VK_MENU),
    (Name: 'VK_PAUSE';      Value: VK_PAUSE),
    (Name: 'VK_CAPITAL';    Value: VK_CAPITAL),
    (Name: 'VK_KANA';       Value: VK_KANA),
    (Name: 'VK_HANGUL';     Value: VK_HANGUL),
    (Name: 'VK_JUNJA';      Value: VK_JUNJA),
    (Name: 'VK_FINAL';      Value: VK_FINAL),
    (Name: 'VK_HANJA';      Value: VK_HANJA),
    (Name: 'VK_KANJI';      Value: VK_KANJI),
    (Name: 'VK_CONVERT';    Value: VK_CONVERT),
    (Name: 'VK_NONCONVERT'; Value: VK_NONCONVERT),
    (Name: 'VK_ACCEPT';     Value: VK_ACCEPT),
    (Name: 'VK_MODECHANGE'; Value: VK_MODECHANGE),
    (Name: 'VK_ESCAPE';     Value: VK_ESCAPE),
    (Name: 'VK_SPACE';      Value: VK_SPACE),
    (Name: 'VK_PRIOR';      Value: VK_PRIOR),
    (Name: 'VK_NEXT';       Value: VK_NEXT),
    (Name: 'VK_END';        Value: VK_END),
    (Name: 'VK_HOME';       Value: VK_HOME),
    (Name: 'VK_LEFT';       Value: VK_LEFT),
    (Name: 'VK_UP';         Value: VK_UP),
    (Name: 'VK_RIGHT';      Value: VK_RIGHT),
    (Name: 'VK_DOWN';       Value: VK_DOWN),
    (Name: 'VK_SELECT';     Value: VK_SELECT),
    (Name: 'VK_PRINT';      Value: VK_PRINT),
    (Name: 'VK_EXECUTE';    Value: VK_EXECUTE),
    (Name: 'VK_SNAPSHOT';   Value: VK_SNAPSHOT),
    (Name: 'VK_INSERT';     Value: VK_INSERT),
    (Name: 'VK_DELETE';     Value: VK_DELETE),
    (Name: 'VK_HELP';       Value: VK_HELP),
    (Name: 'VK_LWIN';       Value: VK_LWIN),
    (Name: 'VK_RWIN';       Value: VK_RWIN),
    (Name: 'VK_APPS';       Value: VK_APPS),
    (Name: 'VK_NUMPAD0';    Value: VK_NUMPAD0),
    (Name: 'VK_NUMPAD1';    Value: VK_NUMPAD1),
    (Name: 'VK_NUMPAD2';    Value: VK_NUMPAD2),
    (Name: 'VK_NUMPAD3';    Value: VK_NUMPAD3),
    (Name: 'VK_NUMPAD4';    Value: VK_NUMPAD4),
    (Name: 'VK_NUMPAD5';    Value: VK_NUMPAD5),
    (Name: 'VK_NUMPAD6';    Value: VK_NUMPAD6),
    (Name: 'VK_NUMPAD7';    Value: VK_NUMPAD7),
    (Name: 'VK_NUMPAD8';    Value: VK_NUMPAD8),
    (Name: 'VK_NUMPAD9';    Value: VK_NUMPAD9),
    (Name: 'VK_MULTIPLY';   Value: VK_MULTIPLY),
    (Name: 'VK_ADD';        Value: VK_ADD),
    (Name: 'VK_SEPARATOR';  Value: VK_SEPARATOR),
    (Name: 'VK_SUBTRACT';   Value: VK_SUBTRACT),
    (Name: 'VK_DECIMAL';    Value: VK_DECIMAL),
    (Name: 'VK_DIVIDE';     Value: VK_DIVIDE),
    (Name: 'VK_F1';         Value: VK_F1),
    (Name: 'VK_F2';         Value: VK_F2),
    (Name: 'VK_F3';         Value: VK_F3),
    (Name: 'VK_F4';         Value: VK_F4),
    (Name: 'VK_F5';         Value: VK_F5),
    (Name: 'VK_F6';         Value: VK_F6),
    (Name: 'VK_F7';         Value: VK_F7),
    (Name: 'VK_F8';         Value: VK_F8),
    (Name: 'VK_F9';         Value: VK_F9),
    (Name: 'VK_F10';        Value: VK_F10),
    (Name: 'VK_F11';        Value: VK_F11),
    (Name: 'VK_F12';        Value: VK_F12),
    (Name: 'VK_F13';        Value: VK_F13),
    (Name: 'VK_F14';        Value: VK_F14),
    (Name: 'VK_F15';        Value: VK_F15),
    (Name: 'VK_F16';        Value: VK_F16),
    (Name: 'VK_F17';        Value: VK_F17),
    (Name: 'VK_F18';        Value: VK_F18),
    (Name: 'VK_F19';        Value: VK_F19),
    (Name: 'VK_F20';        Value: VK_F20),
    (Name: 'VK_F21';        Value: VK_F21),
    (Name: 'VK_F22';        Value: VK_F22),
    (Name: 'VK_F23';        Value: VK_F23),
    (Name: 'VK_F24';        Value: VK_F24),
    (Name: 'VK_NUMLOCK';    Value: VK_NUMLOCK),
    (Name: 'VK_SCROLL';     Value: VK_SCROLL),
    (Name: 'VK_LSHIFT';     Value: VK_LSHIFT),
    (Name: 'VK_RSHIFT';     Value: VK_RSHIFT),
    (Name: 'VK_LCONTROL';   Value: VK_LCONTROL),
    (Name: 'VK_RCONTROL';   Value: VK_RCONTROL),
    (Name: 'VK_LMENU';      Value: VK_LMENU),
    (Name: 'VK_RMENU';      Value: VK_RMENU),
    (Name: 'VK_PROCESSKEY'; Value: VK_PROCESSKEY),
    (Name: 'VK_ATTN';       Value: VK_ATTN),
    (Name: 'VK_CRSEL';      Value: VK_CRSEL),
    (Name: 'VK_EXSEL';      Value: VK_EXSEL),
    (Name: 'VK_EREOF';      Value: VK_EREOF),
    (Name: 'VK_PLAY';       Value: VK_PLAY),
    (Name: 'VK_ZOOM';       Value: VK_ZOOM),
    (Name: 'VK_NONAME';     Value: VK_NONAME),
    (Name: 'VK_PA1';        Value: VK_PA1),
    (Name: 'VK_OEM_CLEAR';  Value: VK_OEM_CLEAR)
   );
var
  I: Integer;
begin
  with cmbVirtualKey.Items do
  begin
    // add the easy ones
    for I := Ord('0') to Ord('9') do
      AddObject('VK_' + Chr(I), TObject(I));
    for I := Ord('A') to Ord('Z') do
      AddObject('VK_' + Chr(I), TObject(I));

    // then add the others...
    for I := Low(VirtualKeys) to High(VirtualKeys) do
      AddObject(VirtualKeys[I].Name, TObject(VirtualKeys[I].Value));
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
    Application.MessageBox(PChar(RsNoValidKeyCode), PChar(RsInvalidKeyCode), MB_ICONERROR);
    // in any case, return 0
    Result := 0;
  end
  else
    Result := Word(cmbVirtualKey.Items.Objects[cmbVirtualKey.ItemIndex]);
end;

function TJvVirtualKeySelectionFrame.GetShiftState: TShiftState;
begin
  Result := [];
  if chkShift.Checked then
    Result := Result + [ssShift];
  if chkCtrl.Checked then
    Result := Result + [ssCtrl];
end;

procedure TJvVirtualKeySelectionFrame.SetCombinedKeyCode(const Value: Word);
begin
  chkShift.Checked := (Value and $4000) <> 0;
  chkCtrl.Checked := (Value and $8000) <> 0;
  cmbVirtualKey.ItemIndex := cmbVirtualKey.Items.IndexOfObject(TObject(Value and $FF));
end;

end.
