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

The Original Code is: JvSLDMappingEditorDialog.pas, released on 2003-07-18.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQSLDMappingEditorDialog;

interface

uses
  Types, QWindows, QMessages, SysUtils, Classes, QGraphics, QControls,
  QForms, QDialogs, QStdCtrls,
  JvQBaseDlg, JvQSegmentedLEDDisplay, JvQSegmentedLEDDisplayMapperFrame,
  QActnList, QMenus, JvQExControls, JvQComponent;

type
  TfrmSLDMappingEditorDialog = class(TForm)
    EditorFrame: TfmeJvSegmentedLEDDisplayMapper;
    lblDigitClassCaption: TLabel;
    lblSegmentCountCaption: TLabel;
    lblCharCaption: TLabel;
    lblMapperValueCaption: TLabel;
    lblSegmentsCaption: TLabel;
    lblDigitClass: TLabel;
    lblSegmentCount: TLabel;
    lblChar: TLabel;
    lblMapperValue: TLabel;
    lblSegments: TLabel;
    btnOK: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    procedure Loaded; override;
    procedure UpdateDigitClass(Sender: TObject);
    procedure UpdateInfo(Sender: TObject);
  end;

procedure SegmentedLEDDisplayMappingEditor(ADisplay: TJvCustomSegmentedLEDDisplay;
  var OpenFolder, SaveFolder: string);

implementation

{$R *.xfm}

procedure SegmentedLEDDisplayMappingEditor(ADisplay: TJvCustomSegmentedLEDDisplay;
  var OpenFolder, SaveFolder: string);
begin
  with TfrmSLDMappingEditorDialog.Create(Application) do
  try
    EditorFrame.Display := ADisplay;
    EditorFrame.LastOpenFolder := OpenFolder;
    EditorFrame.LastSaveFolder := SaveFolder;
    ShowModal;
    OpenFolder := EditorFrame.LastOpenFolder;
    SaveFolder := EditorFrame.LastSaveFolder;
  finally
    Free;
  end;
end;

//=== { TfrmSLDMappingEditorDialog } =========================================

procedure TfrmSLDMappingEditorDialog.Loaded;
begin
  inherited Loaded;
  EditorFrame.OnDisplayChanged := UpdateDigitClass;
  EditorFrame.OnInfoUpdate := UpdateInfo;
end;

procedure TfrmSLDMappingEditorDialog.UpdateDigitClass(Sender: TObject);
begin
  if EditorFrame.Display <> nil then
  begin
    lblDigitClass.Caption := EditorFrame.DigitClass.ClassName;
    lblSegmentCount.Caption := IntToStr(EditorFrame.DigitClass.SegmentCount);
  end
  else
  begin
    lblDigitClass.Caption := '';
    lblSegmentCount.Caption := '';
  end;
end;

procedure TfrmSLDMappingEditorDialog.UpdateInfo(Sender: TObject);
begin
  with EditorFrame do
  begin
    if CharSelected then
    begin
      if CurChar in ['!' .. 'z'] then
        lblChar.Caption := CurChar + ' (#' + IntToStr(Ord(CurChar)) + ')'
      else
        lblChar.Caption := '#' + IntToStr(Ord(CurChar));
    end
    else
      lblChar.Caption := '';
    if Display <> nil then
    begin
      lblMapperValue.Caption := IntToStr(sldEdit.Digits[0].GetSegmentStates);
      lblSegments.Caption := sldEdit.Digits[0].GetSegmentString;
    end
    else
    begin
      lblMapperValue.Caption := '';
      lblSegments.Caption := '';
    end;
  end;
end;

procedure TfrmSLDMappingEditorDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := EditorFrame.CanClose;
end;

end.
