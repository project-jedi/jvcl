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

The Original Code is: JvCSVBase.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQCSVBaseControls;

interface

uses  
  Types, 
  Classes, QControls, QStdCtrls, QButtons,
  JvQComponent;

type
  // (ahuer) changed NameValues: TStringList to TStrings
  TCursorChangedEvent = procedure(Sender: TObject; NameValues: TStrings;
    FieldCount: Integer) of object;

  TJvCSVBase = class(TJvComponent)
  private
    FDBOpen: Boolean;
    FDB: TStrings;
    FDBRecord: TStrings;
    FDBFields: TStrings;
    FDBCursor: Integer;
    FOnCursorChanged: TCursorChangedEvent;
    FCSVFileName: string;
    FCSVFieldNames: TStrings;
    procedure DoCursorChange;
    procedure SetCSVFileName(const Value: string);
    procedure SetCSVFieldNames(const Value: TStrings);
    procedure DisplayFields(NameValues: TStrings);
  protected
    procedure DoCursorChanged(NameValues: TStrings; FieldCount: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DataBaseCreate(const AFile: string; FieldNames: TStrings);
    procedure DataBaseOpen(const AFile: string);
    procedure DataBaseClose;
    procedure DataBaseRestructure(const AFile: string; FieldNames: TStrings);
    procedure RecordNew;
    procedure RecordGet(NameValues: TStrings);
    procedure RecordSet(NameValues: TStrings);
    procedure RecordDelete;
    function RecordNext: Boolean;
    function RecordPrevious: Boolean;
    function RecordFirst: Boolean;
    function RecordLast: Boolean;
    procedure RecordPost;
    function RecordFind(const AText: string): Boolean;
    procedure Display;
  published
    property CSVFileName: string read FCSVFileName write SetCSVFileName;
    property CSVFieldNames: TStrings read FCSVFieldNames write SetCSVFieldNames;
    property OnCursorChanged: TCursorChangedEvent read FOnCursorChanged write FOnCursorChanged;
  end;

  TJvCSVEdit = class(TEdit)
  private
    FCSVDataBase: TJvCSVBase;
    FCSVField: string;
    procedure SetCSVDataBase(const Value: TJvCSVBase);
    procedure SetCSVField(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
    property CSVField: string read FCSVField write SetCSVField;
  end;

  TJvCSVComboBox = class(TComboBox)
  private
    FCSVField: string;
    FCSVDataBase: TJvCSVBase;
    procedure SetCSVDataBase(const Value: TJvCSVBase);
    procedure SetCSVField(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
    property CSVField: string read FCSVField write SetCSVField;
  end;

  TJvCSVCheckBox = class(TCheckBox)
  private
    FCSVField: string;
    FCSVDataBase: TJvCSVBase;
    procedure SetCSVDataBase(const Value: TJvCSVBase);
    procedure SetCSVField(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
    property CSVField: string read FCSVField write SetCSVField;
  end;

  TJvCSVNavigator = class(TJvCustomControl)
  private
    FBtnFirst: TSpeedButton;
    FBtnPrevious: TSpeedButton;
    FBtnFind: TSpeedButton;
    FBtnNext: TSpeedButton;
    FBtnLast: TSpeedButton;
    FBtnAdd: TSpeedButton;
    FBtnDelete: TSpeedButton;
    FBtnPost: TSpeedButton;
    FBtnRefresh: TSpeedButton;
    FCSVDataBase: TJvCSVBase;
    procedure CreateButtons;
    procedure BtnFirstClick(Sender: TObject);
    procedure BtnPreviousClick(Sender: TObject);
    procedure BtnFindClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnLastClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnPostClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure SetCSVDataBase(const Value: TJvCSVBase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoBoundsChanged; override;
  public
    constructor Create(AOwner: TComponent); override; 
  published
    property CSVDataBase: TJvCSVBase read FCSVDataBase write SetCSVDataBase;
  end;

implementation

uses
  SysUtils, QForms, QDialogs,
  JvQThemes, JvQResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCSVBase.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCSVBase.res}
{$ENDIF LINUX}

//=== { TJvCSVBase } =========================================================

constructor TJvCSVBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDB := TStringList.Create;
  FDBRecord := TStringList.Create;
  FDBFields := TStringList.Create;
  FCSVFieldNames := TStringList.Create;
  FDBCursor := -1;
  FDBOpen := False;
end;

destructor TJvCSVBase.Destroy;
begin
  FDB.Free;
  FDBRecord.Free;
  FDBFields.Free;
  FCSVFieldNames.Free;
  inherited Destroy;
end;

procedure TJvCSVBase.DataBaseClose;
begin
  FCSVFileName := '';
  FDBCursor := -1;
  DoCursorChange;
end;

procedure TJvCSVBase.DataBaseCreate(const AFile: string; FieldNames: TStrings);
var
  newfile: string;
  AList: TStrings;
begin
  newfile := ChangeFileExt(AFile, '.csv');
  if FileExists(newfile) then
    if MessageDlg(RsReplaceExistingDatabase, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  AList := TStringList.Create;
  try
    if (FieldNames <> nil) then
      if FieldNames.Count > 0 then
        AList.Text := FieldNames.CommaText;
    AList.SaveToFile(newfile);
  finally
    AList.Free;
  end;
end;

procedure TJvCSVBase.DataBaseOpen(const AFile: string);
begin
  if not FileExists(AFile) then
    DataBaseCreate(AFile, nil);
  FCSVFileName := AFile;
  FDB.LoadFromFile(CSVFileName);
  FDBCursor := -1;
  FDBFields.Clear;
  FDBRecord.Clear;
  if FDB.Count > 0 then
  begin
    FDBCursor := 0;
    FDBFields.CommaText := FDB[0];
    FCSVFieldNames.CommaText := FDB[0];
    if FDB.Count > 1 then
    begin
      FDBCursor := 1;
      FDBRecord.CommaText := FDB[FDBCursor];
      DoCursorChange;
    end;
  end;
end;

procedure TJvCSVBase.DataBaseRestructure(const AFile: string; FieldNames: TStrings);
var
  OldBase: TStrings;
  OldRec: TStrings;
  OldFields: TStrings;
  NewBase: TStrings;
  NewRec: TStrings;
  NewFields: TStrings;
  Index, Rec, Fld: Integer;
begin
  DataBaseClose;
  if FieldNames.Count = 0 then
    raise Exception.CreateRes(@RsENoFieldsDefined);

  OldBase := TStringList.Create;
  OldRec := TStringList.Create;
  OldFields := TStringList.Create;
  NewBase := TStringList.Create;
  NewRec := TStringList.Create;
  NewFields := TStringList.Create;
  try
    OldBase.LoadFromFile(AFile);
    if OldBase.Count = 0 then
    begin
      NewFields.Assign(FieldNames);
      NewBase.Append(NewFields.CommaText);
    end
    else
    begin
      //restructure
      OldFields.CommaText := OldBase[0];
      NewFields.Assign(FieldNames);
      NewBase.Append(NewFields.CommaText);
      if OldBase.Count > 1 then
        for Rec := 1 to OldBase.Count - 1 do
        begin
          OldRec.CommaText := OldBase[Rec];
          NewRec.Clear;
          for Fld := 0 to NewFields.Count - 1 do
          begin
            Index := OldFields.IndexOf(NewFields[Fld]);
            if Index = -1 then
              NewRec.Append('-')
            else
              NewRec.Append(OldRec[Index]);
          end;
          NewBase.Append(NewRec.CommaText);
        end;
    end;
    NewBase.SaveToFile(AFile);
  finally
    OldBase.Free;
    OldRec.Free;
    OldFields.Free;
    NewBase.Free;
    NewRec.Free;
    NewFields.Free;
  end;
end;

procedure TJvCSVBase.RecordNew;
var
  I: Integer;
begin
  if FDBCursor <> -1 then
  begin
    FDBRecord.Clear;
    for I := 0 to FDBFields.Count - 1 do
      FDBRecord.Append('-');
    FDB.Append(FDBRecord.CommaText);
    FDBCursor := FDB.Count - 1;
    FDB.SaveToFile(CSVFileName);
    DoCursorChange;
  end;
end;

procedure TJvCSVBase.RecordDelete;
begin
  if FDBCursor > 0 then
  begin
    FDB.Delete(FDBCursor);
    if FDBCursor > (FDB.Count - 1) then
      Dec(FDBCursor);
    if FDBCursor > 0 then
    begin
      FDBRecord.CommaText := FDB[FDBCursor];
      FDB.SaveToFile(CSVFileName);
    end;
    DoCursorChange;
  end;
end;

function TJvCSVBase.RecordFind(const AText: string): Boolean;
var
  I, From: Integer;
  S: string;
begin
  Result := False;
  if FDBCursor < 1 then
    Exit;
  if FDBCursor < (FDB.Count - 1) then
  begin
    From := FDBCursor + 1;
    S := LowerCase(AText);
    for I := From to FDB.Count - 1 do
      if Pos(S, LowerCase(FDB[I])) > 0 then
      begin
        FDBCursor := I;
        FDBRecord.CommaText := FDB[FDBCursor];
        Result := True;
        DoCursorChange;
        Break;
      end;
  end;
end;

function TJvCSVBase.RecordFirst: Boolean;
begin
  Result := False;
  if FDBCursor <> -1 then
    if FDB.Count > 1 then
    begin
      FDBCursor := 1;
      FDBRecord.CommaText := FDB[FDBCursor];
      Result := True;
      DoCursorChange;
    end;
end;

procedure TJvCSVBase.RecordGet(NameValues: TStrings);
var
  I: Integer;
begin
  NameValues.Clear;
  if FDBCursor < 1 then
    Exit;
  for I := 0 to FDBFields.Count - 1 do
    NameValues.Append(FDBFields[I] + '=' + FDBRecord[I]);
end;

function TJvCSVBase.RecordLast: Boolean;
begin
  Result := False;
  if FDBCursor <> -1 then
    if FDB.Count > 1 then
    begin
      FDBCursor := FDB.Count - 1;
      FDBRecord.CommaText := FDB[FDBCursor];
      Result := True;
      DoCursorChange;
    end;
end;

function TJvCSVBase.RecordNext: Boolean;
begin
  Result := False;
  if FDBCursor <> -1 then
  begin
    if FDBCursor < (FDB.Count - 1) then
    begin
      Inc(FDBCursor);
      FDBRecord.CommaText := FDB[FDBCursor];
      Result := True;
      DoCursorChange;
    end;
  end;
end;

function TJvCSVBase.RecordPrevious: Boolean;
begin
  Result := False;
  if FDBCursor <> -1 then
  begin
    if FDBCursor > 1 then
    begin
      Dec(FDBCursor);
      FDBRecord.CommaText := FDB[FDBCursor];
      Result := True;
      DoCursorChange;
    end;
  end;
end;

procedure TJvCSVBase.RecordSet(NameValues: TStrings);
var
  I, Index: Integer;
  FieldName: string;
begin
  if NameValues.Count > 0 then
  begin
    for I := 0 to NameValues.Count - 1 do
    begin
      FieldName := NameValues.names[I];
      Index := FDBFields.IndexOf(FieldName);
      if Index <> -1 then
        FDBRecord[Index] := NameValues.Values[FieldName];
    end;
    FDB[FDBCursor] := FDBRecord.CommaText;
    FDB.SaveToFile(CSVFileName);
  end;
end;

procedure TJvCSVBase.DoCursorChanged(NameValues: TStrings; FieldCount: Integer);
begin
  if Assigned(OnCursorChanged) then
    OnCursorChanged(Self, NameValues, FieldCount);
end;

procedure TJvCSVBase.DoCursorChange;
var
  NameValues: TStrings;
begin
  NameValues := TStringList.Create;
  try
    RecordGet(NameValues);
    DisplayFields(NameValues);
    DoCursorChanged(NameValues, NameValues.Count);
  finally
    NameValues.Free;
  end;
end;

procedure TJvCSVBase.DisplayFields(NameValues: TStrings);
var
  AForm: TForm;
  I, Index: Integer;
  ed: TJvCSVEdit;
  cbo: TJvCSVComboBox;
  ck: TJvCSVCheckBox;
  AField: string;
begin
  AForm := TForm(Self.Owner);
  for I := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[I].ClassName = 'TJvCSVEdit' then
    begin
      ed := TJvCSVEdit(AForm.Components[I]);
      if ed.CSVDataBase = Self then
      begin
        AField := ed.CSVField;
        Index := CSVFieldNames.IndexOf(AField);
        if Index <> -1 then
          if FDBCursor > 0 then
            ed.Text := FDBRecord[Index]
          else
            ed.Text := '[' + AField + ']';
      end;
    end
    else
    if AForm.Components[I].ClassName = 'TJvCSVComboBox' then
    begin
      cbo := TJvCSVComboBox(AForm.Components[I]);
      if cbo.CSVDataBase = Self then
      begin
        AField := cbo.CSVField;
        Index := CSVFieldNames.IndexOf(AField);
        if Index <> -1 then
          if FDBCursor > 0 then
            cbo.Text := FDBRecord[Index]
          else
            cbo.Text := '[' + AField + ']';
      end;
    end
    else
    if AForm.Components[I].ClassName = 'TJvCSVCheckBox' then
    begin
      ck := TJvCSVCheckBox(AForm.Components[I]);
      if ck.CSVDataBase = Self then
      begin
        AField := ck.CSVField;
        Index := CSVFieldNames.IndexOf(AField);
        if Index <> -1 then
          if FDBCursor > 0 then
            ck.Checked := FDBRecord[Index] = 'True'
          else
            ck.Checked := False;
      end;
    end;
end;

procedure TJvCSVBase.SetCSVFileName(const Value: string);
begin
  if Value <> FCSVFileName then
  begin
    DataBaseClose;
    FCSVFileName := Value;
    if FileExists(CSVFileName) then
      DataBaseOpen(CSVFileName)
    else
      DataBaseCreate(CSVFileName, nil);
  end;
end;

procedure TJvCSVBase.Display;
begin
  DoCursorChange;
end;

procedure TJvCSVBase.RecordPost;
var
  AForm: TForm;
  I, Index: Integer;
  ed: TJvCSVEdit;
  cbo: TJvCSVComboBox;
  ck: TJvCSVCheckBox;
  AField: string;
begin
  if FDBCursor < 1 then
    Exit;
  AForm := TForm(Self.Owner);
  for I := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[I].ClassName = 'TJvCSVEdit' then
    begin
      ed := TJvCSVEdit(AForm.Components[I]);
      if ed.CSVDataBase = Self then
      begin
        AField := ed.CSVField;
        Index := CSVFieldNames.IndexOf(AField);
        if Index <> -1 then
          FDBRecord[Index] := ed.Text;
      end;
    end
    else
    if AForm.Components[I].ClassName = 'TJvCSVComboBox' then
    begin
      cbo := TJvCSVComboBox(AForm.Components[I]);
      if cbo.CSVDataBase = Self then
      begin
        AField := cbo.CSVField;
        Index := CSVFieldNames.IndexOf(AField);
        if Index <> -1 then
          FDBRecord[Index] := cbo.Text;
      end;
    end
    else
    if AForm.Components[I].ClassName = 'TJvCSVCheckBox' then
    begin
      ck := TJvCSVCheckBox(AForm.Components[I]);
      if ck.CSVDataBase = Self then
      begin
        AField := ck.CSVField;
        Index := CSVFieldNames.IndexOf(AField);
        if Index <> -1 then
          if ck.Checked then
            FDBRecord[Index] := 'True'
          else
            FDBRecord[Index] := 'False';
      end;
    end;

  FDB[FDBCursor] := FDBRecord.CommaText;
  FDB.SaveToFile(CSVFileName);
end;

procedure TJvCSVBase.SetCSVFieldNames(const Value: TStrings);
var
  OldFile: string;
begin
  if (CSVFileName <> '') and (Value.Count > 0) then
  begin
    OldFile := CSVFileName;
    DataBaseClose;
    FCSVFieldNames.Assign(Value);
    DataBaseRestructure(OldFile, Value);
    DataBaseOpen(OldFile);
  end;
end;

//=== { TJvCSVEdit } =========================================================

procedure TJvCSVEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
    FCSVField := '';
  end;
end;

procedure TJvCSVEdit.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

procedure TJvCSVEdit.SetCSVField(const Value: string);
begin
  if Value <> FCSVField then
  begin
    FCSVField := Value;
    if Assigned(FCSVDataBase) then
      CSVDataBase.Display;
  end;
end;

//=== { TJvCSVNavigator } ====================================================

constructor TJvCSVNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  Height := 24;
  Width := 217;
  CreateButtons; 
  ControlStyle := ControlStyle - [csSetCaption]; 
end;

procedure TJvCSVNavigator.BtnAddClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordNew;
end;

procedure TJvCSVNavigator.BtnDeleteClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordDelete;
end;

procedure TJvCSVNavigator.BtnFindClick(Sender: TObject);
var
  AText: string;
begin
  if Assigned(FCSVDataBase) then
  begin
    AText := inputbox(RsCVSDatabase, RsFindText, '');
    if AText <> '' then
      CSVDataBase.RecordFind(AText);
  end;
end;

procedure TJvCSVNavigator.BtnFirstClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordFirst;
end;

procedure TJvCSVNavigator.BtnLastClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordLast;
end;

procedure TJvCSVNavigator.BtnNextClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordNext;
end;

procedure TJvCSVNavigator.BtnPostClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordPost;
end;

procedure TJvCSVNavigator.BtnPreviousClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.RecordPrevious;
end;

procedure TJvCSVNavigator.BtnRefreshClick(Sender: TObject);
begin
  if Assigned(FCSVDataBase) then
    CSVDataBase.Display;
end;

procedure TJvCSVNavigator.CreateButtons;

  procedure InitButton(var Btn: TSpeedButton; ALeft: Integer; GlyphName: string;
    ClickEvent: TNotifyEvent; AHint: string);
  begin
    Btn := TSpeedButton.Create(Self);
    Btn.Width := 23;
    Btn.Height := 22;
    Btn.Flat := True;
    Btn.Top := 1;
    Btn.Left := ALeft;
    Btn.Glyph.LoadFromResourceName(HInstance, GlyphName);
    Btn.OnClick := ClickEvent;
    Btn.Hint := AHint;
    Btn.Parent := Self;
  end;

begin
  ShowHint := True;
  InitButton(FBtnFirst, 1, 'JVCSVFIRST', BtnFirstClick, RsFirstHint);
  InitButton(FBtnPrevious, 25, 'JVCSVPREVIOUS', BtnPreviousClick, RsPreviousHint);
  InitButton(FBtnFind, 49, 'JVCSVFIND', BtnFindClick, RsFindHint);
  InitButton(FBtnNext, 73, 'JVCSVNEXT', BtnNextClick, RsNextHint);
  InitButton(FBtnLast, 97, 'JVCSVLAST', BtnLastClick, RsLastHint);
  InitButton(FBtnAdd, 121, 'JVCSVADD', BtnAddClick, RsAddHint);
  InitButton(FBtnDelete, 145, 'JVCSVDELETE', BtnDeleteClick, RsDeleteHint);
  InitButton(FBtnPost, 169, 'JVCSVPOST', BtnPostClick, RsPostHint);
  InitButton(FBtnRefresh, 193, 'JVCSVREFRESH', BtnRefreshClick, RsRefreshHint);
end;



procedure TJvCSVNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
    FCSVDataBase := nil;
end;

procedure TJvCSVNavigator.DoBoundsChanged;
begin
  Height := 24;
  if Width < 221 then
    Width := 221;
  inherited DoBoundsChanged;
end;

procedure TJvCSVNavigator.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

//=== { TJvCSVComboBox } =====================================================

procedure TJvCSVComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
    FCSVField := '';
  end;
end;

procedure TJvCSVComboBox.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

procedure TJvCSVComboBox.SetCSVField(const Value: string);
begin
  if Value <> FCSVField then
  begin
    FCSVField := Value;
    if Assigned(FCSVDataBase) then
      CSVDataBase.Display;
  end;
end;

//=== { TJvCSVCheckBox } =====================================================

procedure TJvCSVCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCSVDataBase) then
  begin
    FCSVDataBase := nil;
    FCSVField := '';
  end;
end;

procedure TJvCSVCheckBox.SetCSVDataBase(const Value: TJvCSVBase);
begin
  FCSVDataBase := Value;
end;

procedure TJvCSVCheckBox.SetCSVField(const Value: string);
begin
  if Value <> FCSVField then
  begin
    FCSVField := Value;
    if Assigned(FCSVDataBase) then
      CSVDataBase.Display;
  end;
end;

end.
