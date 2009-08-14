{-----------------------------------------------------------------------------
  JVCSVDATASET - TIBURON VERSION 3.5

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

Contributor(s):  Warren Postma (warrenpstma att hotmail dott com)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

description : TJvCsvDataSet data access component. Design time unit - editor form.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCsvDataForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls,
  JvTypes, JvComponent;

const
  JvCsvAsciiTypeArrayLimit=9; // Array Size constant for TJvCsvColumnFlag to Ascii Character mapping.

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
  private
    FUpdating: Boolean;
    FOriginalCsvStr: String;
    FSeparator: Char;
    FTypeChars: array [0..JvCsvAsciiTypeArrayLimit] of AnsiChar;
    FFieldTypeCh: AnsiChar;
    procedure ItemChange;
    function MakeString: string; // take changes, put back into string format
    procedure UpdateCsvStr;
    procedure LengthDisabled;
    procedure LengthEnabled(FieldLength: Integer);
    procedure SetCsvStr(ACsvStr: string);
    function GetCsvStr: string;
  public
    property Separator: Char read FSeparator write FSeparator;
    property CsvStr: string read GetCsvStr write SetCsvStr;
  end;

var
  JvCsvDefStrDialog: TJvCsvDefStrDialog;

implementation

uses
  JvCsvData, JvCsvParse, JvDsgnConsts;

{$R *.dfm}

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
      S := S + FSeparator + ListBoxFields.Items[I];
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

//type
//  TStringArray = array of string;

procedure TJvCsvDefStrDialog.ItemChange;
var
  SubFields: Array of string;
  I, Count: Integer;
  SelectedText: string;
  FieldLength: Integer;
begin
  SetLength(SubFields, 3);

  if ListBoxFields.ItemIndex >= 0 then
  begin
    SelectedText := ListBoxFields.Items[ListBoxFields.ItemIndex];
    Count := JvStrSplit(SelectedText, ':', Chr(0), SubFields, 2); // Look for Colon
  end
  else
  begin
    Count := 0;
    SelectedText := '';
  end;

  try
    if Count < 2 then
    begin { no colon! }
      { defaults to string, length JvCsv_DEFAULT_CSV_STR_FIELD}
      EditFieldName.Text := SelectedText;
      FFieldTypeCh := '$';
      FieldLength := JvCsv_DEFAULT_CSV_STR_FIELD;
    end
    else
    begin
      EditFieldName.Text := string(SubFields[0]);
      FFieldTypeCh := AnsiChar(SubFields[1][1]);
      FieldLength := StrToIntDef(Copy(string(SubFields[1]), 2, Length(SubFields[1])), JvCsv_DEFAULT_CSV_STR_FIELD);
    end;
  except
    { clear it if we have a problem }
    EditFieldName.Text := '';
    FFieldTypeCh := '$';
    FieldLength := JvCsv_DEFAULT_CSV_STR_FIELD;
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

function TJvCsvDefStrDialog.GetCsvStr: string;
begin
  Result := EditCsvStr.Text;
end;

procedure TJvCsvDefStrDialog.SetCsvStr(ACsvStr: string);
var
  Fields: array of string;
  I, Count: Integer;
  FieldDefStr: string;

  function StripQuotes(S: string): string;
  begin
    if (Length(S) >= 2) and (S[1] = '"') and (S[Length(S)] = '"') then
      Result := Copy(S, 2, Length(S) - 2)
    else
      Result := S;
  end;

begin
  FOriginalCsvStr := ACsvStr;
  FieldDefStr := UpperCase(JvStrStrip(ACsvStr));

  SetLength(Fields, JvCsv_MAXCOLUMNS); { MAXCOLUMNS is a constant from CsvDataSource.pas }
  EditCsvStr.Text := ACsvStr;
  if Length(FieldDefStr) > 0 then
    Count := JvStrSplit(FieldDefStr, FSeparator, #0, Fields, JvCsv_MAXCOLUMNS)
  else
    Count := 0;

  try
    FUpdating := True;
    ListBoxFields.Items.Clear;
    if Count > 0 then
      for I := 0 to Count - 1 do
        ListBoxFields.Items.Add(StripQuotes(string(Fields[I])));
  finally
    ListBoxFields.ItemIndex := 0;
    ItemChange;
    FUpdating := False;
  end;
  Self.ActiveControl := EditFieldName;
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
  // FTypeChars:  Map of Ascii character symbols to TJvCsvColumnFlag enumerated
  //              types.
  //
  // IF YOU ADD ANYTHING TO THIS LIST, CHANGE CONSTANT
  // JvCsvAsciiTypeArrayLimit at top of this unit!
  //
  // The order of this list must match the order of the
  // enumerated TJvCsvColumnFlag type in JvCsvData.pas!
  //
  //
  FTypeChars[0] := '!'; // Boolean
  FTypeChars[1] := '$'; // String
  FTypeChars[2] := '%'; // Integer
  FTypeChars[3] := '&'; // Float
  FTypeChars[4] := '@'; // Ascii DateTime
  FTypeChars[5] := '#'; // Hex Timestamp
  FTypeChars[6] := '^'; // Hex LocalTime
  FTypeChars[7] := '/'; // Ascii Date
  FTypeChars[8] := '*';
  FTypeChars[9] := '~'; // New September 2008 (WideString field to UTF8)
  {
   $ = string (ftString) - also used if no character is given.
   % = whole Integer value (ftInteger)
   & = floating point value (ftFloat)
   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
   ! = Boolean Field (0 in csv file=False, not 0 = True, blank = NULL)
   ~ = UTF8 encoded WideString field.
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

  S := JvStrStrip(UpperCase(EditFieldName.Text));
  if S <> EditFieldName.Text then
    EditFieldName.Text := S;

  if not JvValidIdentifier(S) then  // in JvCsvParse.pas
    Exit;

  FFieldTypeCh := FTypeChars[ListBoxFieldTypes.ItemIndex];
  if FFieldTypeCh = '$' then
  begin
    FieldLength := StrToIntDef(EditFieldLength.Text, JvCsv_DEFAULT_CSV_STR_FIELD);
    if FieldLength <> JvCsv_DEFAULT_CSV_STR_FIELD then
      S := S + ':$' + IntToStr(FieldLength);
  end
  else
    S := S + ':' + Char(FFieldTypeCh);
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
  if not JvValidIdentifier(F) then // in JvCsvParse.pas
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
    ListBoxFields.ItemIndex := -1; // make sure no new item is selected, so we can add another.
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
  if not JvValidIdentifier(F) then // in JvCsvParse.pas
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
        LengthEnabled(JvCsv_DEFAULT_CSV_STR_FIELD)
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
  TempStr := ListBoxFields.Items[Selected - 1];
  ListBoxFields.Items[Selected - 1] := ListBoxFields.Items[Selected];
  ListBoxFields.Items[Selected] := TempStr;
  ListBoxFields.ItemIndex := ListBoxFields.ItemIndex - 1;
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
  TempStr := ListBoxFields.Items[Selected + 1];
  ListBoxFields.Items[Selected + 1] := ListBoxFields.Items[Selected];
  ListBoxFields.Items[Selected] := TempStr;
  ListBoxFields.ItemIndex := ListBoxFields.ItemIndex + 1;
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
  ListBoxFields.Items.Delete(Item);
  ListBoxFields.ItemIndex := -1;
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
