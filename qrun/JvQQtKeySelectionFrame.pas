{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVirtualKeySelection.PAS, released 2003-09-29.

The Initial Developer of the Original Code is Olivier Sannier <obones att altern dott org>
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): André Snepvangers [asn att xs4all dott nl]: JvQtKeySelectionFrame

Last Modified: 2004-02-05

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description: This unit defines a frame that you can use to select a key code.

Known Issues: none known
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQQtKeySelectionFrame;

interface

uses
  Qt, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  {$IFDEF USE_DXGETTEXT}
  JvQGnugettext,
  {$ENDIF USE_DXGETTEXT}
  SysUtils, Classes;

type
  TJvQtKeySelectionFrame = class(TFrame)
    cmbVirtualKey: TComboBox;
    lblVirtualKey: TLabel;
    chkShift: TCheckBox;
    chkCtrl: TCheckBox;
    lblModifiers: TLabel;
  protected
    FShiftState: TShiftState;
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

{$R *.xfm}

resourcestring
  sNoValidKeyCode = 'This is not a valid key code';
  sInvalidKeyCode = 'Invalid key code';

constructor TJvQtKeySelectionFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF USE_DXGETTEXT}
  TranslateComponent(Self);
  {$ENDIF USE_DXGETTEXT}
  EnumKeys;
end;

procedure TJvQtKeySelectionFrame.EnumKeys;
var
  I: Integer;
begin
  with cmbVirtualKey.Items do
  begin
    // add the easy ones
    for I := Ord('0') to Ord('9') do
      AddObject('Key_' + Chr(I), TObject(I));
    for I := Ord('A') to Ord('Z') do
      AddObject('Key_' + Chr(I), TObject(I));

    // then add the others...
//    AddObject('VK_LBUTTON',    TObject(VK_LBUTTON));
//    AddObject('VK_RBUTTON',    TObject(VK_RBUTTON));
//    AddObject('VK_CANCEL',     TObject(VK_CANCEL));
//    AddObject('VK_MBUTTON',    TObject(VK_MBUTTON));
    AddObject('Key_Backspace', TObject(Key_Backspace));
    AddObject('Key_Tab', TObject(Key_Tab));
//  AddObject('VK_CLEAR',      TObject(VK_CLEAR));
    AddObject('Key_Return', TObject(Key_Return));
    AddObject('Key_Shift', TObject(Key_Shift));
    AddObject('Key_Control', TObject(Key_Control));
    AddObject('Key_Alt', TObject(Key_Alt));
    AddObject('Key_Pause', TObject(Key_Pause));
    AddObject('Key_CapsLock', TObject(Key_CapsLock));
//    AddObject('VK_KANA',       TObject(VK_KANA));
//    AddObject('VK_HANGUL',     TObject(VK_HANGUL));
//    AddObject('VK_JUNJA',      TObject(VK_JUNJA));
//    AddObject('VK_FINAL',      TObject(VK_FINAL));
//    AddObject('VK_HANJA',      TObject(VK_HANJA));
//    AddObject('VK_KANJI',      TObject(VK_KANJI));
//    AddObject('VK_CONVERT',    TObject(VK_CONVERT));
//    AddObject('VK_NONCONVERT', TObject(VK_NONCONVERT));
//    AddObject('VK_ACCEPT',     TObject(VK_ACCEPT));
//    AddObject('VK_MODECHANGE', TObject(VK_MODECHANGE));
    AddObject('Key_Escape', TObject(Key_Escape));
    AddObject('Key_Space', TObject(Key_Space));
    AddObject('Key_Prior', TObject(Key_Prior));
    AddObject('Key_Next', TObject(Key_Next));
    AddObject('Key_End', TObject(Key_End));
    AddObject('Key_Home', TObject(Key_Home));
    AddObject('Key_Left', TObject(Key_Left));
    AddObject('Key_Up', TObject(Key_Up));
    AddObject('Key_Right', TObject(Key_Right));
    AddObject('Key_Down', TObject(Key_Down));
//    AddObject('VK_SELECT',     TObject(VK_SELECT));
    AddObject('Key_Print', TObject(Key_Print));
//    AddObject('VK_EXECUTE',    TObject(VK_EXECUTE));
    AddObject('Key_SysReq', TObject(Key_SysReq));
    AddObject('Key_Insert', TObject(Key_Insert));
    AddObject('Key_Delete', TObject(Key_Delete));
    AddObject('Key_Help', TObject(Key_Help));
//    AddObject('VK_LWIN',       TObject(VK_LWIN));
//    AddObject('VK_RWIN',       TObject(VK_RWIN));
    AddObject('Key_Menu', TObject(Key_Menu));
    AddObject('Key_Asterisk', TObject(Key_Asterisk));
    AddObject('Key_Plus', TObject(Key_Plus));
//    AddObject('VK_SEPARATOR',  TObject(VK_SEPARATOR));
    AddObject('Key_Minus', TObject(Key_Minus));
    AddObject('Key_Period', TObject(Key_Period));
    AddObject('Key_Slash', TObject(Key_Slash));
    for I := 1 to 35 do
      AddObject('Qt_F' + IntToStr(I), TObject(4143 + I));
    AddObject('Key_Super_L', TObject(Key_Super_L));
    AddObject('Key_Super_R', TObject(Key_Super_R));
    AddObject('Key_Hyper_L', TObject(Key_Hyper_L));
    AddObject('Key_Hyper_R', TObject(Key_Hyper_R));

    AddObject('Key_NumLock', TObject(Key_NumLock));
    AddObject('Key_ScrollLock', TObject(Key_ScrollLock));
    AddObject('Key_Shift', TObject(Key_Shift));
//   AddObject('VK_RSHIFT',     TObject(VK_RSHIFT));
    AddObject('Key_Control', TObject(Key_Control));
//   AddObject('VK_RCONTROL',   TObject(VK_RCONTROL));
//    AddObject('VK_LMENU',      TObject(VK_LMENU));
//    AddObject('VK_RMENU',      TObject(VK_RMENU));
//    AddObject('VK_PROCESSKEY', TObject(VK_PROCESSKEY));
//    AddObject('VK_ATTN',       TObject(VK_ATTN));
//    AddObject('VK_CRSEL',      TObject(VK_CRSEL));
//    AddObject('VK_EXSEL',      TObject(VK_EXSEL));
//    AddObject('VK_EREOF',      TObject(VK_EREOF));
//    AddObject('VK_PLAY',       TObject(VK_PLAY));
//    AddObject('VK_ZOOM',       TObject(VK_ZOOM));
//    AddObject('VK_NONAME',     TObject(VK_NONAME));
//    AddObject('VK_PA1',        TObject(VK_PA1));
//    AddObject('VK_OEM_CLEAR',  TObject(VK_OEM_CLEAR));
  end;
end;

function TJvQtKeySelectionFrame.GetCombinedKeyCode: Word;
begin
  Result := GetKeyCode;
  if chkShift.Checked then
    Result := Result or $4000;
  if chkCtrl.Checked then
    Result := Result or $8000;
end;

function TJvQtKeySelectionFrame.GetKeyCode: Word;
begin
  if cmbVirtualKey.ItemIndex = -1 then
  begin
    // Signal an error, but not sure how...
    Application.MessageBox(sNoValidKeyCode, sInvalidKeyCode, [smbOK], smsCritical);
    // in any case, return 0
    Result := 0;
  end
  else
    Result := Word(cmbVirtualKey.Items.Objects[cmbVirtualKey.ItemIndex]);
end;

function TJvQtKeySelectionFrame.GetShiftState: TShiftState;
begin
  Result := [];
  if chkShift.Checked then
    Result := Result + [ssShift];
  if chkCtrl.Checked then
    Result := Result + [ssCtrl];
end;

procedure TJvQtKeySelectionFrame.SetCombinedKeyCode(const Value: Word);
begin
  chkShift.Checked := (Value and $4000) <> 0;
  chkCtrl.Checked := (Value and $8000) <> 0;
  cmbVirtualKey.ItemIndex := cmbVirtualKey.Items.IndexOfObject(TObject(Value and $FF));
end;

end.

