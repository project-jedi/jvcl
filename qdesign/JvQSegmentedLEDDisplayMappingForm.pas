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

The Original Code is: JvSegmentedLEDDisplayMappingForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQSegmentedLEDDisplayMappingForm;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs, QStdCtrls, 
  DesignIntf, DesignEditors, 
  JvQBaseDsgnForm, JvQSegmentedLEDDisplayMapperFrame, JvQSegmentedLEDDisplay,
  JvQBaseDsgnFrame, JvQDsgnTypes;

type
  TfrmJvSLDMappingEditor = class(TJvBaseDesign)
    fmeMapper: TfmeJvSegmentedLEDDisplayMapper;
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
  private
    FDesigner: IJvFormDesigner;
    function GetDisplay: TJvCustomSegmentedLEDDisplay;
    procedure SetDisplay(Value: TJvCustomSegmentedLEDDisplay);
    procedure SetDesigner(Value: IJvFormDesigner);
  protected
    function DesignerFormName: string; override;
    function AutoStoreSettings: Boolean; override;
    procedure StoreSettings; override;
    procedure RestoreSettings; override;
    procedure UpdateDigitClass(Sender: TObject);
    procedure UpdateInfo(Sender: TObject);
    procedure MappingChanged(Sender: TObject);
  public
    procedure Loaded; override;
    property Designer: IJvFormDesigner read FDesigner write SetDesigner;
    property Display: TJvCustomSegmentedLEDDisplay read GetDisplay write SetDisplay;
  end;

procedure EditSLDMapping(ADisplay: TJvCustomSegmentedLEDDisplay; ADesigner: IJvFormDesigner);

implementation

uses
  Registry,
  JvQDsgnConsts;



{$R *.xfm}


const
  cLastOpenFolder = 'LastOpenFolder';
  cLastSaveFolder = 'LastSaveFolder';

function IsSLDMappingEditForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvSLDMappingEditor;
  if Result then
    with (Form as TfrmJvSLDMappingEditor) do
      Result := (Pointer(Display) = Args[0].VObject) and
        (Pointer(Designer) = Args[1].VInterface);
end;

procedure EditSLDMapping(ADisplay: TJvCustomSegmentedLEDDisplay; ADesigner: IJvFormDesigner);
var
  Form: TfrmJvSLDMappingEditor;
begin
  Form := TfrmJvSLDMappingEditor(GetDesignerForm(IsSLDMappingEditForm, [ADisplay, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmJvSLDMappingEditor.Create(nil);
    try
      Form.Display := ADisplay;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise;
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

//=== { TfrmJvSLDMappingEditor } =============================================

function TfrmJvSLDMappingEditor.GetDisplay: TJvCustomSegmentedLEDDisplay;
begin
  Result := fmeMapper.Display;
end;

procedure TfrmJvSLDMappingEditor.SetDisplay(Value: TJvCustomSegmentedLEDDisplay);
begin
  if Value <> Display then
    fmeMapper.Display := Value;
end;

procedure TfrmJvSLDMappingEditor.SetDesigner(Value: IJvFormDesigner);
begin
  if Value <> FDesigner then
    FDesigner := Value;
end;

function TfrmJvSLDMappingEditor.DesignerFormName: string;
begin
  Result := RsSegmentedLEDDisplayMappingEditor;
end;

function TfrmJvSLDMappingEditor.AutoStoreSettings: Boolean;
begin
  Result := True;
end;

procedure TfrmJvSLDMappingEditor.StoreSettings;
begin
  inherited StoreSettings;
    with TRegistry.Create do
    try
      LazyWrite := False;
      if OpenKey(GetRegKey, True) then
        try
          WriteString(cLastOpenFolder, fmeMapper.LastOpenFolder);
          WriteString(cLastSaveFolder, fmeMapper.LastSaveFolder);
        finally
          CloseKey;
        end;
    finally
      Free;
  end;
end;

procedure TfrmJvSLDMappingEditor.RestoreSettings;
begin
  inherited RestoreSettings;
  with TRegistry.Create do
    try
      if OpenKey(GetRegKey, False) then
        try
          if ValueExists(cLastOpenFolder) then
            fmeMapper.LastOpenFolder := ReadString(cLastOpenFolder);
          if ValueExists(cLastSaveFolder) then
            fmeMapper.LastSaveFolder := ReadString(cLastSaveFolder);
        finally
          CloseKey;
        end;
    finally
      Free;
    end;
end;

procedure TfrmJvSLDMappingEditor.UpdateDigitClass(Sender: TObject);
begin
  if fmeMapper.Display <> nil then
  begin
    lblDigitClass.Caption := fmeMapper.DigitClass.ClassName;
    lblSegmentCount.Caption := IntToStr(fmeMapper.DigitClass.SegmentCount);
  end
  else
  begin
    lblDigitClass.Caption := '';
    lblSegmentCount.Caption := '';
  end;
end;

procedure TfrmJvSLDMappingEditor.UpdateInfo(Sender: TObject);
begin
  with fmeMapper do
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

procedure TfrmJvSLDMappingEditor.MappingChanged(Sender: TObject);
begin
  if Designer <> nil then
    Designer.Modified;
end;

procedure TfrmJvSLDMappingEditor.Loaded;
begin
  inherited Loaded;
  
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  
  if fmeMapper <> nil then
  begin
    fmeMapper.OnMappingChanged := MappingChanged;
    fmeMapper.OnDisplayChanged := UpdateDigitClass;
    fmeMapper.OnInfoUpdate := UpdateInfo;
  end;
end;

procedure TfrmJvSLDMappingEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := fmeMapper.CanClose;
  // (rom) this seems silly
  if CanClose then
    inherited;
end;

end.
