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

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : TJvCsvDataSet data access component. Design time unit - editor form.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQCsvDataForm;

interface

uses
  SysUtils, Classes,
  QWindows, QMessages, Types, QGraphics, QControls, QForms,
  QDialogs, QExtCtrls, QButtons, QStdCtrls,
  JvQTypes, JvQComponent;

type
  TJvCsvDefStrDialog = class(TJvForm)
    EditCsvStr: TEdit;
    Label1: TLabel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ListBoxFieldTypes: TListBox;
    Label2: TLabel;
    EditFieldName: TEdit;
    Label3: TLabel;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonDel: TSpeedButton;
    SpeedButtonMod: TSpeedButton;
    LabelFieldLen: TLabel;
    EditFieldLength: TEdit;
    ListBoxFields: TListBox;
    Label5: TLabel;
    Bevel1: TBevel;
    SpeedButtonMoveFieldUp: TSpeedButton;
    SpeedButtonMoveFieldDown: TSpeedButton;
    LabelKey: TLabel;
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxFieldsClick(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonModClick(Sender: TObject);
    procedure ListBoxFieldTypesClick(Sender: TObject);
    procedure SpeedButtonMoveFieldUpClick(Sender: TObject);
    procedure SpeedButtonMoveFieldDownClick(Sender: TObject);
    procedure SpeedButtonDelClick(Sender: TObject);
    procedure ListBoxFieldsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    FUpdating: Boolean;
    FOriginalCsvStr: string;
    FTypeChars: array [0..6] of Char;
    FFieldTypeCh: Char;
    procedure ItemChange;
    function MakeString: string; // take changes, put back into string format
    procedure UpdateCsvStr;
    procedure LengthDisabled;
    procedure LengthEnabled(FieldLength: Integer);
  public
    procedure SetCsvStr(ACsvStr: string);
    function GetCsvStr: string;
  end;

var
  JvCsvDefStrDialog: TJvCsvDefStrDialog;

implementation

uses
  JvQCsvData, JvQCsvParse, JvQDsgnConsts;

{$R *.xfm}

procedure TJvCsvDefStrDialog.UpdateCsvStr;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to ListBoxFields.Items.Count - 1 do
    if S = '' then
      S := ListBoxFields.Items[I]
    else
      S := S + ',' + ListBoxFields.Items[I];
  EditCsvStr.Text := S;
end;

procedure TJvCsvDefStrDialog.LengthEnabled(FieldLength: Integer);
begin
  EditFieldLength.Text := IntToStr(FieldLength);
  EditFieldLength.Enabled := True;
  EditFieldLength.Color := clWindow;
  LabelFieldLen.Enabled := True;
end;

procedure TJvCsvDefStrDialog.LengthDisabled;
begin
  EditFieldLength.Text := '';
  EditFieldLength.Color := clBtnFace;
  EditFieldLength.Enabled := False;
  LabelFieldLen.Enabled := False;
end;

type
  TStringArray = array of string;

procedure TJvCsvDefStrDialog.ItemChange;
var
  SubFields: TStringArray;
  I, Count: Integer;
  SelectedText: string;
  FieldLength: Integer;
begin
  SetLength(SubFields, 3);

  if ListBoxFields.ItemIndex >= 0 then
  begin
    SelectedText := ListBoxFields.Items[ListboxFields.ItemIndex];
    Count := StrSplit(SelectedText, ':', Chr(0), SubFields, 2); // Look for Colon
  end
  else
  begin
    Count := 0;
    SelectedText := '';
  end;

  try
    if Count < 2 then
    begin { no colon! }
      { defaults to string, length DEFAULT_CSV_STR_FIELD}
      EditFieldName.Text := SelectedText;
      FFieldTypeCh := '$';
      FieldLength := DEFAULT_CSV_STR_FIELD;
    end
    else
    begin
      EditFieldName.Text := SubFields[0];
      FFieldTypeCh := SubFields[1][1];
      FieldLength := StrToIntDef(Copy(SubFields[1], 2, Length(SubFields[1])), DEFAULT_CSV_STR_FIELD);
    end;
  except
    { clear it if we have a problem }
    EditFieldName.Text := '';
    FFieldTypeCh := '$';
    FieldLength := DEFAULT_CSV_STR_FIELD;
  end;
  if FFieldTypeCh = '$' then
    LengthEnabled(FieldLength)
  else
    LengthDisabled;

 { given a field type character, match it and then selecting
    the correct index in the field types list }
  ListBoxFieldTypes.ItemIndex := 1;
  for I := Low(FTypeChars) to High(FTypeChars) do
    if FTypeChars[I] = FFieldTypeCh then
    begin
      ListBoxFieldTypes.ItemIndex := I;
      Break;
    end;
end;

procedure TJvCsvDefStrDialog.SetCsvStr(ACsvStr: string);
var
  Fields: array of string;
  I, Count: Integer;
  FieldDefStr: string;
begin
  FOriginalCsvStr := ACsvStr;
  FieldDefStr := UpperCase(StrStrip(ACsvStr));

  SetLength(Fields, MAXCOLUMNS); { MAXCOLUMNS is a constant from CsvDataSource.pas }
  EditCsvStr.Text := ACsvStr;
  if Length(FieldDefStr) > 0 then
    Count := StrSplit(FieldDefStr, ',', Chr(0), Fields, MAXCOLUMNS)
  else
    Count := 0;

  try
    FUpdating := True;
    ListBoxFields.Items.Clear;
    if Count > 0 then
      for I := 0 to Count - 1 do
        ListBoxFields.Items.Add(Fields[I]);
  finally
    ListBoxFields.ItemIndex := 0;
    ItemChange;
    FUpdating := False;
  end;
  Self.ActiveControl := EditFieldName;
end;

function TJvCsvDefStrDialog.GetCsvStr: string;
begin
  Result := EditCsvStr.Text;
end;

procedure TJvCsvDefStrDialog.ButtonOkClick(Sender: TObject);
begin
  UpdateCsvStr;
  if EditCsvStr.Text = FOriginalCsvStr then
    if MessageBox(Self.Handle, PChar(RsYouHaventActuallyChangedAnythingIfY),
      PChar(RsConfirm), MB_YESNO or MB_ICONWARNING) = IDNO then
      Exit; // quit before we set ModalResult, so we continue on editing.
  ModalResult := mrOk;
end;

procedure TJvCsvDefStrDialog.ButtonCancelClick(Sender: TObject);
begin
  EditCsvStr.Text := FOriginalCsvStr; // cancel all edits.
  ModalResult := mrCancel;
end;

procedure TJvCsvDefStrDialog.FormCreate(Sender: TObject);
begin
  FTypeChars[0] := '!'; // Boolean
  FTypeChars[1] := '$'; // String
  FTypeChars[2] := '%'; // Integer
  FTypeChars[3] := '&'; // Float
  FTypeChars[4] := '@'; // Ascii DateTime
  FTypeChars[5] := '#'; // Hex Timestamp
  FTypeChars[6] := '^'; // Hex LocalTime
  {
   $ = string (ftString) - also used if no character is given.
   % = whole Integer value (ftInteger)
   & = floating point value (ftFloat)
   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
   ! = Boolean Field (0 in csv file=False, not 0 = True, blank = NULL)
  }
end;

procedure TJvCsvDefStrDialog.ListBoxFieldsClick(Sender: TObject);
begin
  if not FUpdating then
    try
      FUpdating := True;
      ItemChange;
    finally
      FUpdating := False;
    end;
end;

function TJvCsvDefStrDialog.MakeString: string;
var
  S: string;
  FieldLength: Integer;
begin
  if ListBoxFieldTypes.ItemIndex < 0 then
    Exit;

  S := StrStrip(UpperCase(EditFieldName.Text));
  if S <> EditFieldName.Text then
    EditFieldName.Text := S;

  if not ValidIdentifier(PChar(S)) then
    Exit;

  FFieldTypeCh := FTypeChars[ListBoxFieldTypes.ItemIndex];
  if FFieldTypeCh = '$' then
  begin
    FieldLength := StrToIntDef(EditFieldLength.Text, DEFAULT_CSV_STR_FIELD);
    if FieldLength <> DEFAULT_CSV_STR_FIELD then
      S := S + ':$' + IntToStr(FieldLength);
  end
  else
    S := S + ':' + FFieldTypeCh;
  Result := S;
end;

function FieldNameOnly(CsvFieldDef: string): string;
var
  XPos: Integer;
begin
  XPos := Pos(':', CsvFieldDef);
  if XPos = 0 then
  begin
    Result := CsvFieldDef;
    Exit;
  end;
  Result := Copy(CsvFieldDef, 1, XPos - 1);
end;

procedure TJvCsvDefStrDialog.SpeedButtonAddClick(Sender: TObject);
var
  F, S, Unique: string;
  I: Integer;
begin
  if FUpdating then
    Exit;
  S := MakeString;
  if S = '' then
  begin
    MessageBox(Self.Handle, PChar(RsMustTypeAValidFieldNameAndSelectAFi),
      PChar(RsAddFailed), MB_OK or MB_ICONERROR);
    Exit; { not valid, can't add }
  end;
   // XXX Check Validity and Uniqueness before adding.
  F := FieldNameOnly(S);
  if not ValidIdentifier(PChar(F)) then
  begin
    MessageBox(Self.Handle, PChar(Format(RsFieldNameIsNotAValidIdentifier, [S])),
      PChar(RsAddFailed), MB_OK or MB_ICONERROR);
    Exit;
  end;
  for I := 0 to ListBoxFields.Items.Count - 1 do
  begin
    Unique := FieldNameOnly(ListBoxFields.Items[I]);
    if Unique = F then
    begin
      MessageBox(Self.Handle, PChar(RsCantAddTwoFieldsWithTheSameNameSele),
        PChar(RsAddFailed), MB_OK or MB_ICONERROR);
      Exit;
    end;
  end;

  try
    FUpdating := True;
    ListBoxFields.Items.Add(S);
    ListboxFields.ItemIndex := -1; // make sure no new item is selected, so we can add another.
    UpdateCsvStr;
    EditFieldName.Text := '';
    ActiveControl := EditFieldName;
  finally
    FUpdating := False;
  end;
end;

procedure TJvCsvDefStrDialog.SpeedButtonModClick(Sender: TObject);
var
  F, S, Unique: string;
  Selected, I: Integer;
begin
  if FUpdating then
    Exit;
  S := MakeString;
  if S = '' then
  begin
    MessageBox(Self.Handle, PChar(RsMustTypeAValidFieldNameAndSelectAFi),
      PChar(RsUpdateFailed), MB_OK or MB_ICONERROR);
    Exit; { not valid, can't add }
  end;
   // XXX Check Validity and Uniqueness before adding.
  F := FieldNameOnly(S);
  if not ValidIdentifier(PChar(F)) then
  begin
    MessageBox(Self.Handle, PChar(Format(RsFieldNameIsNotAValidIdentifier, [S])),
      PChar(RsUpdateFailed), MB_OK or MB_ICONERROR);
    Exit;
  end;
  Selected := ListBoxFields.ItemIndex;
  if Selected < 0 then
  begin
    MessageBox(Self.Handle, PChar(RsNoItemIsSelectedInTheFieldsListYouC),
      PChar(RsUpdateFailed), MB_OK or MB_ICONERROR);
    Exit; // can't do that!
  end;
  for I := 0 to ListBoxFields.Items.Count - 1 do
  begin
    Unique := FieldNameOnly(ListBoxFields.Items[I]);
    if (I <> Selected) and (Unique = F) then
    begin
      MessageBox(Self.Handle, PChar(RsModifyingTheCurrentlySelectedItemWo),
        PChar(RsUpdateFailed), MB_OK or MB_ICONERROR);
      Exit;
    end;
  end;
  try
    FUpdating := True;
    ListBoxFields.Items[Selected] := S; // changes current item.
    UpdateCsvStr;
  finally
    FUpdating := False;
  end;
end;

procedure TJvCsvDefStrDialog.ListBoxFieldTypesClick(Sender: TObject);
begin
  if not FUpdating then
    try
      FUpdating := True;
      if ListBoxFieldTypes.ItemIndex = 1 then
        LengthEnabled(DEFAULT_CSV_STR_FIELD)
      else
        LengthDisabled;
      ActiveControl := EditFieldName;
    finally
      FUpdating := False;
    end;
end;

procedure TJvCsvDefStrDialog.SpeedButtonMoveFieldUpClick(Sender: TObject);
var
  Selected: Integer;
  TempStr: string;
begin
  Selected := ListBoxFields.ItemIndex;
  if Selected <= 0 then
    Exit; // can't move an invalid item up, or item 0
  // swap selected, with selected-1 (moves selected item up)
  FUpdating := True;
  TempStr := ListboxFields.Items[Selected - 1];
  ListboxFields.Items[Selected - 1] := ListboxFields.Items[Selected];
  ListboxFields.Items[Selected] := TempStr;
  ListboxFields.ItemIndex := ListboxFields.ItemIndex - 1;
  UpdateCsvStr;
  FUpdating := False;
end;

procedure TJvCsvDefStrDialog.SpeedButtonMoveFieldDownClick(Sender: TObject);
var
  Selected: Integer;
  TempStr: string;
begin
  Selected := ListBoxFields.ItemIndex;
  if Selected < 0 then
    Exit; // can't move an invalid item down
  if Selected >= ListBoxFields.Items.Count - 1 then
    Exit; // can't move last item down

  // swap selected, with selected+1 (moves selected item down)
  FUpdating := True;
  TempStr := ListboxFields.Items[Selected + 1];
  ListboxFields.Items[Selected + 1] := ListboxFields.Items[Selected];
  ListboxFields.Items[Selected] := TempStr;
  ListboxFields.ItemIndex := ListboxFields.ItemIndex + 1;
  UpdateCsvStr;
  FUpdating := False;
end;

procedure TJvCsvDefStrDialog.SpeedButtonDelClick(Sender: TObject);
var
  Item: Integer;
begin
  Item := ListBoxFields.ItemIndex;
  if Item < 0 then
    Exit; // can't delete, nothing selected.
  FUpdating := True;
  ListboxFields.Items.Delete(Item);
  ListboxFields.ItemIndex := -1;
  UpdateCsvStr;
  FUpdating := False;
end;

procedure TJvCsvDefStrDialog.ListBoxFieldsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
// LabelKey.Caption := IntToStr(Key);
  if Key = VK_DELETE then
    SpeedButtonDelClick(Sender);
end;

end.

