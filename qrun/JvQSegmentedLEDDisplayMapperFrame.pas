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

The Original Code is: JvSegmentedLEDDisplayMapperFrame.pas, released on 2003-07-18.

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

unit JvQSegmentedLEDDisplayMapperFrame;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QActnList, QMenus,
  {$IFDEF USE_DXGETTEXT}
  JvQGnugettext,
  {$ENDIF USE_DXGETTEXT}
  JvQComponent, JvQSegmentedLEDDisplay, JvQExControls;

type
  TfmeJvSegmentedLEDDisplayMapper = class(TFrame)
    sldEdit: TJvSegmentedLEDDisplay;
    pmDigit: TPopupMenu;
    miSetStates: TMenuItem;
    miClearStates: TMenuItem;
    miInvertStates: TMenuItem;
    mnuCharMapEdit: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Default1: TMenuItem;
    N2: TMenuItem;
    Close1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N3: TMenuItem;
    Selectchar1: TMenuItem;
    Apply1: TMenuItem;
    Revert1: TMenuItem;
    N4: TMenuItem;
    Setallsegments1: TMenuItem;
    Emptysegments1: TMenuItem;
    Invertsegments1: TMenuItem;
    alCharMapEditor: TActionList;
    aiFileOpen: TAction;
    aiFileSave: TAction;
    aiFileLoadDefault: TAction;
    aiFileClose: TAction;
    aiEditCopy: TAction;
    aiEditPaste: TAction;
    aiEditClear: TAction;
    aiEditSetAll: TAction;
    aiEditInvert: TAction;
    aiEditSelectChar: TAction;
    aiEditRevert: TAction;
    aiEditApply: TAction;
    procedure sldEditClick(Sender: TObject);
    procedure sldEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure alCharMapEditorUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure aiFileOpenExecute(Sender: TObject);
    procedure aiFileSaveExecute(Sender: TObject);
    procedure aiFileLoadDefaultExecute(Sender: TObject);
    procedure aiFileCloseExecute(Sender: TObject);
    procedure aiEditCopyExecute(Sender: TObject);
    procedure aiEditPasteExecute(Sender: TObject);
    procedure aiEditClearExecute(Sender: TObject);
    procedure aiEditSetAllExecute(Sender: TObject);
    procedure aiEditInvertExecute(Sender: TObject);
    procedure aiEditSelectCharExecute(Sender: TObject);
    procedure aiEditRevertExecute(Sender: TObject);
    procedure aiEditApplyExecute(Sender: TObject);
  private
    FDisplay: TJvCustomSegmentedLEDDisplay;
    FMouseDownX: Integer;
    FMouseDownY: Integer;
    FCurChar: Char;
    FCopiedValue: Int64;
    FCharSelected: Boolean;
    FCharModified: Boolean;
    FMapperModified: Boolean;
    FLastOpenFolder: string;
    FLastSaveFolder: string;
    FLastSaveFileName: string;
    FOnDisplayChanged: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnInfoUpdate: TNotifyEvent;
    FOnMappingChanged: TNotifyEvent;
    function CheckCharModified: Boolean;
    function CheckMapperModified: Boolean;
    function DoSaveMapping: Boolean;
    function GetDigitClass: TJvSegmentedLEDDigitClass;
    function GetDisplay: TJvCustomSegmentedLEDDisplay;
    function GetMapper: TJvSegmentedLEDCharacterMapper;
    procedure SetDisplay(Value: TJvCustomSegmentedLEDDisplay);
  protected
    procedure DisplayChanged;
    procedure CloseEditor;
    procedure InfoUpdate;
    procedure MappingChanged;
  public
    constructor Create(AOwner: TComponent); override;
    function CanClose: Boolean;
    property DigitClass: TJvSegmentedLEDDigitClass read GetDigitClass;
    property CurChar: Char read FCurChar;
    property CopiedValue: Int64 read FCopiedValue;
    property CharSelected: Boolean read FCharSelected;
    property CharModified: Boolean read FCharModified;
    property MapperModified: Boolean read FMapperModified;
    property LastOpenFolder: string read FLastOpenFolder write FLastOpenFolder;
    property LastSaveFolder: string read FLastSaveFolder write FLastSaveFolder;
    property Mapper: TJvSegmentedLEDCharacterMapper read GetMapper;
  published
    property Display: TJvCustomSegmentedLEDDisplay read GetDisplay write SetDisplay;
    property OnDisplayChanged: TNotifyEvent read FOnDisplayChanged write FOnDisplayChanged;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnInfoUpdate: TNotifyEvent read FOnInfoUpdate write FOnInfoUpdate;
    property OnMappingChanged: TNotifyEvent read FOnMappingChanged write FOnMappingChanged;
  end;

implementation

uses
  JvQConsts, JvQResources;

{$R *.xfm}

type
  TJvCustomSegmentedLEDDisplayAccessProtected = class(TJvCustomSegmentedLEDDisplay);
  TJvCustomSegmentedLEDDigitAccessProtected = class(TJvCustomSegmentedLEDDigit);

function Mask(SegCount: Integer): Int64;
begin
  Result := (1 shl SegCount) - 1;
end;

function TfmeJvSegmentedLEDDisplayMapper.CheckCharModified: Boolean;
var
  mr: TModalResult;
begin
  if FCharModified then
  begin
    mr := MessageDlg(RsTheCurrentCharacterHasBeenModifiedA, mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    Result := mr <> mrCancel;
    if mr = mrYes then
      aiEditApply.Execute
    else
    if Result then
      FCharModified := False;
  end
  else
    Result := True;
end;

function TfmeJvSegmentedLEDDisplayMapper.CheckMapperModified: Boolean;
var
  mr: TModalResult;
begin
  if FMapperModified then
  begin
    mr := MessageDlg(RsTheCurrentMappingHasBeenModifiedSav, mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    Result := mr <> mrCancel;
    if mr = mrYes then
      Result := DoSaveMapping
    else
    if Result then
      FMapperModified := False;
  end
  else
    Result := True;
end;

function TfmeJvSegmentedLEDDisplayMapper.DoSaveMapping: Boolean;
begin
  with TSaveDialog.Create(Application) do
  try
    InitialDir := LastSaveFolder;  
    Options := [ofOverwritePrompt, ofNoChangeDir, ofPathMustExist,
      ofEnableSizing]; 
    Filter := RsSegmentedLEDDisplayMappingFilessdms;
    FilterIndex := 0;
    FileName := FLastSaveFileName;
    Result := Execute;
    if Result then
    try
      FLastSaveFolder := ExtractFilePath(FileName);
      FLastSaveFileName := FileName;
      Mapper.SaveToFile(FileName);
      FMapperModified := False;
    except
      Result := False;
      raise;
    end;
  finally
    Free;
  end;
end;

function TfmeJvSegmentedLEDDisplayMapper.GetMapper: TJvSegmentedLEDCharacterMapper;
begin
  Result := TJvCustomSegmentedLEDDisplayAccessProtected(Display).CharacterMapper;
end;

function TfmeJvSegmentedLEDDisplayMapper.GetDigitClass: TJvSegmentedLEDDigitClass;
begin
  Result := TJvCustomSegmentedLEDDisplayAccessProtected(Display).DigitClass;
end;

function TfmeJvSegmentedLEDDisplayMapper.GetDisplay: TJvCustomSegmentedLEDDisplay;
begin
  Result := FDisplay;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.SetDisplay(Value: TJvCustomSegmentedLEDDisplay);
begin
  if Value <> Display then
  begin
    FDisplay := Value;
    if Value <> nil then
    begin
      sldEdit.DigitClass := TJvCustomSegmentedLEDDisplayAccessProtected(Value).DigitClass;
      if sldEdit.Digits.Count = 0 then
        sldEdit.Digits.Add;
      TJvCustomSegmentedLEDDigitAccessProtected(sldEdit.Digits[0]).EnableAllSegs;
      DisplayChanged;
    end;
  end;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.DisplayChanged;
begin
  if Assigned(FOnDisplayChanged) then
    FOnDisplayChanged(Self);
end;

procedure TfmeJvSegmentedLEDDisplayMapper.CloseEditor;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TfmeJvSegmentedLEDDisplayMapper.InfoUpdate;
begin
  if Assigned(FOnInfoUpdate) then
    FOnInfoUpdate(Self);
end;

procedure TfmeJvSegmentedLEDDisplayMapper.MappingChanged;
begin
  if Assigned(FOnMappingChanged) then
    FOnMappingChanged(Self);
end;

constructor TfmeJvSegmentedLEDDisplayMapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF USE_DXGETTEXT}
  TranslateComponent(Self);
  {$ENDIF USE_DXGETTEXT}
  FCopiedValue := -1;
end;

function TfmeJvSegmentedLEDDisplayMapper.CanClose: Boolean;
begin
  Result := CheckCharModified and CheckMapperModified;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.sldEditClick(Sender: TObject);
var
  Digit: TJvCustomSegmentedLEDDigit;
  SegIdx: Integer;
begin
  if aiEditClear.Enabled and
    (sldEdit.GetHitInfo(FMouseDownX, FMouseDownY, Digit, SegIdx) = shiDigitSegment) then
  begin
    TJvCustomSegmentedLEDDigitAccessProtected(Digit).SetSegmentStates(Digit.GetSegmentStates xor 1 shl SegIdx);
    FCharModified := True;
  end;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.sldEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownX := X;
  FMouseDownY := Y;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.alCharMapEditorUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action = aiFileClose then
  begin
    aiFileOpen.Enabled := Display <> nil;
    aiFileSave.Enabled := FMapperModified;
    aiFileLoadDefault.Enabled := aiFileOpen.Enabled;
    aiEditApply.Enabled := FCharModified;
    aiEditPaste.Enabled := (FCopiedValue <> -1);
    aiEditRevert.Enabled := FCharModified;
    aiEditClear.Enabled := FCharSelected;
    aiEditInvert.Enabled := aiEditClear.Enabled;
    aiEditSetAll.Enabled := aiEditClear.Enabled;
    aiEditCopy.Enabled := aiEditClear.Enabled;
    aiEditSelectChar.Enabled := Display <> nil;
    InfoUpdate;
  end;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiFileOpenExecute(Sender: TObject);
begin
  if CheckCharModified and CheckMapperModified then
  begin
    with TOpenDialog.Create(Application) do
    try
      InitialDir := LastOpenFolder;  
      Options := [ofNoChangeDir, ofPathMustExist, ofFileMustExist,
         ofEnableSizing]; 
      Filter := RsSegmentedLEDDisplayMappingFilessdms;
      FilterIndex := 0;
      if Execute then
      begin
        Mapper.LoadFromFile(FileName);
        LastOpenFolder := ExtractFilePath(FileName);
        aiEditRevert.OnExecute(Sender);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiFileSaveExecute(Sender: TObject);
begin
  if CheckCharModified then
    DoSaveMapping;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiFileLoadDefaultExecute(Sender: TObject);
begin
  if CheckCharModified and CheckMapperModified then
  begin
    Mapper.LoadDefaultMapping;
    aiEditRevert.OnExecute(Sender);
  end;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiFileCloseExecute(Sender: TObject);
var
  ParentForm: TCustomForm;
begin
  CloseEditor;
  ParentForm := GetParentForm(Self);
  if ParentForm <> nil then
    ParentForm.Close;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditCopyExecute(Sender: TObject);
begin
  FCopiedValue := sldEdit.Digits[0].GetSegmentStates;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditPasteExecute(Sender: TObject);
begin
  TJvCustomSegmentedLEDDigitAccessProtected(sldEdit.Digits[0]).SetSegmentStates(FCopiedValue);
  FCharModified := True;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditClearExecute(Sender: TObject);
begin
  TJvCustomSegmentedLEDDigitAccessProtected(sldEdit.Digits[0]).SetSegmentStates(0);
  FCharModified := True;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditSetAllExecute(Sender: TObject);
var
  Digit: TJvCustomSegmentedLEDDigit;
begin
  Digit := sldEdit.Digits[0];
  TJvCustomSegmentedLEDDigitAccessProtected(Digit).SetSegmentStates(Digit.GetSegmentStates or Mask(Digit.SegmentCount));
  FCharModified := True;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditInvertExecute(Sender: TObject);
var
  Digit: TJvCustomSegmentedLEDDigit;
begin
  Digit := sldEdit.Digits[0];
  TJvCustomSegmentedLEDDigitAccessProtected(Digit).SetSegmentStates(Digit.GetSegmentStates xor Mask(Digit.SegmentCount));
  FCharModified := True;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditSelectCharExecute(Sender: TObject);
var
  S: string;
  Done: Boolean;
begin
  if FCharSelected then
    S := FCurChar
  else
    S := '';
  Done := False;
  repeat
    if InputQuery(RsSelectCharacter, RsSpecifyANewCharacter, S) then
    begin
      if Length(S) > 0 then
      begin
        if (S[1] = '#') and (Length(S) > 1) then
          S := Chr(StrToInt(Copy(S, 2, Length(S) - 1)));
        FCurChar := S[1];
        FCharSelected := True;
        Done := True;
        aiEditRevert.OnExecute(Sender);
      end;
    end
    else
      Done := True;
  until Done;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditRevertExecute(Sender: TObject);
begin
  TJvCustomSegmentedLEDDigitAccessProtected(sldEdit.Digits[0]).SetSegmentStates(
    Mapper.CharMapping[FCurChar]);
  FCharModified := False;
end;

procedure TfmeJvSegmentedLEDDisplayMapper.aiEditApplyExecute(Sender: TObject);
begin
  Mapper.CharMapping[FCurChar] := sldEdit.Digits[0].GetSegmentStates;
  FCharModified := False;
  FMapperModified := True;
  MappingChanged;
end;

end.

