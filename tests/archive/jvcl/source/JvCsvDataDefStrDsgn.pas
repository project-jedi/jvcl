unit JvCsvDataDefStrDsgn;
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

Last Modified: 2003-04-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : TJvCsvDataSet data access component. Design time unit - editor form.

Known Issues:
-----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls;

type
  TJvCsvDefStrDialog = class(TForm)
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
    Label6: TLabel;
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
    { Private declarations }
  protected
    FUpdating:Boolean;
    FOriginalCsvStr:String;
    FTypeChars:Array[0..6] of Char;
    FFieldTypeCh:Char;
    procedure ItemChange;
    function MakeString:String; // take changes, put back into string format
    procedure UpdateCsvStr;
    procedure  LengthDisabled;
    procedure  LengthEnabled(FieldLength:Integer);

  public
    { Public declarations }
    procedure SetCsvStr(aCsvStr:String);
    function GetCsvStr:String;

  end;

var
  JvCsvDefStrDialog: TJvCsvDefStrDialog;

implementation

uses JvCsvData, JvCsvParse;

{$R *.dfm}

procedure TJvCsvDefStrDialog.UpdateCsvStr;
var
  t:Integer;
  s:String;
begin
  s := '';
  for t:= 0 to ListBoxFields.Items.Count-1 do begin
      if s = '' then
        s := ListBoxFields.Items[t]
      else
        s := s+','+ListBoxFields.Items[t]
  end;
  EditCsvStr.Text := s;
end;


procedure TJvCsvDefStrDialog.LengthEnabled(FieldLength:Integer);
begin
     EditFieldLength.Text := IntToStr(FieldLength);
     EditFieldLength.Enabled := True;
     EditFieldLength.Color := clWindow;
     LabelFieldLen.Enabled := true;
end;

procedure TJvCsvDefStrDialog.LengthDisabled;
begin
        EditFieldLength.Text := '';
        EditFieldLength.Color := clBtnFace;
        EditFieldLength.Enabled := False;
        LabelFieldLen.Enabled := false;
end;


procedure TJvCsvDefStrDialog.ItemChange;
var
  SubFields:Array of String;
  t,Count:Integer;
  selectedText:String;
  FieldLength:Integer;
begin
  SetLength(SubFields,3);

  if ListBoxFields.ItemIndex>=0 then begin
        selectedText := ListBoxFields.Items[ListboxFields.ItemIndex];
        Count := StrSplit(selectedText,':',Chr(0),SubFields, 2); // Look for Colon
  end else begin
        Count := 0;
        selectedText := '';
  end;

 try
 if Count < 2 then begin { no colon! }
        { defaults to string, length DEFAULT_CSV_STR_FIELD}
        EditFieldName.Text := selectedText;
        FFieldTypeCh := '$';
        FieldLength := DEFAULT_CSV_STR_FIELD;
 end else begin
         EditFieldName.Text := SubFields[0];
         FFieldTypeCh := SubFields[1][1];
         FieldLength := StrToIntDef( Copy(SubFields[1],1,Length(SubFields[1])), DEFAULT_CSV_STR_FIELD);
 end;
 except
        { clear it if we have a problem }
        EditFieldName.Text := '';
        FFieldTypeCh := '$';
        FieldLength := DEFAULT_CSV_STR_FIELD;
 end;
 if FFieldTypeCh = '$' then begin
        LengthEnabled(FieldLength)
 end else begin
        LengthDisabled;
 end;

 { given a field type character, match it and then selecting
    the correct index in the field types list }
 ListBoxFieldTypes.ItemIndex := 1;
 for t := 0 to 6 do begin
        if FTypeChars[t] = FFieldTypeCh then begin
               ListBoxFieldTypes.ItemIndex := t;
               break;
        end;
 end;
end;

procedure TJvCsvDefStrDialog.SetCsvStr(aCsvStr:String);
var
  Fields:Array of String;
  t,Count:Integer;
  fielddefstr :String;
begin
 FOriginalCsvStr := aCsvStr;
 fielddefstr := UpperCase(StrStrip(aCsvStr));

 SetLength(Fields,MAXCOLUMNS); { MAXCOLUMNS is a constant from CsvDataSource.pas }
 EditCsvStr.Text := aCsvStr;
 if length(FielddefStr)>0 then
    Count := StrSplit(fielddefstr,',',Chr(0),Fields, MAXCOLUMNS)
 else
    Count := 0;
    
 try
  FUpdating:= True;
  ListBoxFields.Items.Clear;
  if (Count >0 ) then
      for t := 0 to Count-1 do
           ListBoxFields.Items.Add(Fields[t]);
 finally
   ListBoxFields.ItemIndex := 0;
   ItemChange;
  FUpdating := False
 end;
  Self.ActiveControl :=   EditFieldName;
end;

function TJvCsvDefStrDialog.GetCsvStr:String;
begin
  result := EditCsvStr.Text;
end;

procedure TJvCsvDefStrDialog.ButtonOkClick(Sender: TObject);
begin
  UpdateCsvStr;
  if (EditCsvStr.Text = FOriginalCsvStr) then begin
        if MessageBox(Self.Handle, PChar('You haven''t actually changed anything. If you '+
                'made changes and didn''t click Modify, the changes have '+
                'not been made yet. (Click no, to go back.) '+Chr(13)+
                'Are you sure you want to close the CSV Fields editor? '),
                'Confirm?',
                MB_YESNO or MB_ICONWARNING )= idNo
                        then
                          exit; // quit before we set modalResult, so we continue on editing.
  end;
  modalResult := idOk;
end;

procedure TJvCsvDefStrDialog.ButtonCancelClick(Sender: TObject);
begin
  EditCsvStr.Text := FOriginalCsvStr; // cancel all edits.
  modalResult := idCancel;
end;

procedure TJvCsvDefStrDialog.FormCreate(Sender: TObject);
begin
  FTypeChars[0] := '!'; // Boolean
  FTypeChars[1] := '$'; //  String
  FTypeChars[2] := '%'; // Integer
  FTypeChars[3] := '&'; // Float
  FTypeChars[4] := '@'; // Ascii DateTime
  FTypeChars[5] := '#'; // Hex Timestamp
  FTypeChars[6] := '^'; // Hex LocalTime
  {
   $ = string (ftString) - also used if no character is given.
   % = whole integer value (ftInteger)
   & = floating point value (ftFloat)
   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
   ! = Boolean Field (0 in csv file=false, not 0 = true, blank = NULL)
  }

end;

procedure TJvCsvDefStrDialog.ListBoxFieldsClick(Sender: TObject);
begin
 if not FUpdating then begin
 
   try
        FUpdating := true;
        ItemChange;
   finally
        FUpdating := false;
   end; {end try finally}

 end; {endif }

end;

function TJvCsvDefStrDialog.MakeString:String;
var
 s:string;
 FieldLength:Integer;
begin
  if ListBoxFieldTypes.ItemIndex <0 then exit;

  s := StrStrip(UpperCase(EditFieldName.Text));
  if s <> EditFieldName.Text then
        EditFieldName.Text := s;
        
  if not ValidIdentifier(pchar(s)) then exit;
  
  FFieldTypeCh := FTypeChars[ListBoxFieldTypes.ItemIndex];
  if FFieldTypeCh = '$' then begin
        FieldLength := StrToIntDef(EditFieldLength.Text,DEFAULT_CSV_STR_FIELD);
        if FieldLength <> DEFAULT_CSV_STR_FIELD then
                s := s +':$'+IntToStr(FieldLength)
  end else begin
        s := s + ':'+FFieldTypeCh;
  end;
  result := s;
end;

function FieldNameOnly(csvfielddef:String):String;
var
 xpos:Integer;
begin
  xpos := Pos( ':', csvfielddef);
  if xpos = 0 then begin
        result := csvfielddef;
        exit;
  end;
  result := Copy(csvfielddef,1,xpos-1);
end;

procedure TJvCsvDefStrDialog.SpeedButtonAddClick(Sender: TObject);
var
 f,s,unique:String;
 t:Integer;
begin
   if FUpdating then exit;
   s:=MakeString;
   if s = '' then begin
      MessageBox( Self.Handle,'Must type a valid field name and select a field type. Field name must start with a letter A-Z and consist of letters and numbers only. All field names will be converted to uppercase before being used.','Add Failed',MB_OK or MB_ICONERROR );
      exit;  { not valid, can't add }
   end;
   // XXX Check Validity and Uniqueness before adding.
   f := FieldNameOnly(s);
   if (not ValidIdentifier(PChar(f)) ) then begin
             MessageBox( Self.Handle,PChar(s+': Field name is not a valid identifier'),'Add Failed',MB_OK or MB_ICONERROR );
             exit;
   end;
   for t := 0 to ListBoxFields.Items.Count -1 do begin
          unique := FieldNameOnly(ListBoxFields.Items[t]);
          if unique = f then begin
                MessageBox( Self.Handle,'Can''t add two fields with the same name! Select existing item and click ''Modify'' button to change its properties.','Add Failed',MB_OK or MB_ICONERROR );
                exit;
          end;
   end;
   
   try
     FUpdating := true;
     ListBoxFields.Items.Add(s);
     ListboxFields.ItemIndex := -1; // make sure no new item is selected, so we can add another.
     UpdateCsvStr;
     EditFieldName.Text := '';
     ActiveControl := EditFieldName;
   finally
     FUpdating := false;
   end;

end;

procedure TJvCsvDefStrDialog.SpeedButtonModClick(Sender: TObject);
var
 f,s,unique:String;
 selected,t:Integer;
begin
   if FUpdating then exit;
   s:=MakeString;
   if s = '' then begin
      MessageBox( Self.Handle,'Must type a valid field name and select a field type. Field name must start with a letter A-Z and consist of letters and numbers only. All field names will be converted to uppercase before being used.','Update Failed',MB_OK or MB_ICONERROR );
      exit;  { not valid, can't add }
   end;
   // XXX Check Validity and Uniqueness before adding.
   f := FieldNameOnly(s);
   if (not ValidIdentifier(PChar(f)) ) then begin
             MessageBox( Self.Handle,PChar(s+': Field name is not a valid identifier'),'Update Failed',MB_OK or MB_ICONERROR );
             exit;
   end;
   selected := ListBoxFields.ItemIndex;
   if selected <0 then begin
     MessageBox( Self.Handle,'No item is selected in the fields list. You can''t update nothing.','Update Failed',MB_OK or MB_ICONERROR );
     exit; // can't do that!
   end;
   for t := 0 to ListBoxFields.Items.Count -1 do begin
          unique := FieldNameOnly(ListBoxFields.Items[t]);
          if (t <> selected) and (unique = f) then begin
                MessageBox( Self.Handle,'Modifying the currently selected item would create two items with the same name. ','Update Failed',MB_OK or MB_ICONERROR );
                exit;
          end;
   end;
   try
     FUpdating := true;
     ListBoxFields.Items[selected] := s; // changes current item.
     UpdateCsvStr;
   finally
     FUpdating := false;
   end;

end;

procedure TJvCsvDefStrDialog.ListBoxFieldTypesClick(Sender: TObject);
begin
 if not FUpdating then begin
 
        try
              FUpdating := true;
              if ListBoxFieldTypes.ItemIndex = 1 then begin
                    LengthEnabled(DEFAULT_CSV_STR_FIELD);
              end else begin
                    LengthDisabled;
              end;
              ActiveControl :=  EditFieldName;
        finally
             FUpdating := false;
       end;

 end;
end;

procedure TJvCsvDefStrDialog.SpeedButtonMoveFieldUpClick(Sender: TObject);
var
 selected:Integer;
 tempStr:String;
begin
  selected :=  ListBoxFields.ItemIndex;
  if selected<= 0 then exit; // can't move an invalid item up, or item 0
  // swap selected, with selected-1 (moves selected item up)
  FUpdating := true;
  tempStr := ListboxFields.Items[selected-1];
  ListboxFields.Items[selected-1] := ListboxFields.Items[selected];
  ListboxFields.Items[selected] := tempStr;
  ListboxFields.ItemIndex := ListboxFields.ItemIndex -1;
  UpdateCsvStr;
  FUpdating := false;
end;


procedure TJvCsvDefStrDialog.SpeedButtonMoveFieldDownClick(Sender: TObject);
var
 selected:Integer;
 tempStr:String;
begin
  selected :=  ListBoxFields.ItemIndex;
  if selected< 0 then exit; // can't move an invalid item down
  if selected >= ListBoxFields.Items.Count-1 then exit; // can't move last item down
  
  // swap selected, with selected+1 (moves selected item down)
  FUpdating := true;
  tempStr := ListboxFields.Items[selected+1];
  ListboxFields.Items[selected+1] := ListboxFields.Items[selected];
  ListboxFields.Items[selected] := tempStr;
  ListboxFields.ItemIndex := ListboxFields.ItemIndex +1;
  UpdateCsvStr;
  FUpdating := false;
end;

procedure TJvCsvDefStrDialog.SpeedButtonDelClick(Sender: TObject);
var
  item:Integer;
begin
  item :=  ListBoxFields.ItemIndex;
  if item<0 then exit; // can't delete, nothing selected.
  FUpdating := true;
  ListboxFields.Items.Delete(item);
  ListboxFields.ItemIndex := -1;
  UpdateCsvStr;
  FUpdating := false;
end;

procedure TJvCsvDefStrDialog.ListBoxFieldsKeyUp(Sender: TObject;
  var Key:Word; Shift: TShiftState);
begin
// LabelKey.Caption := IntToStr(Key);
 if Key = 46 then // delete
    SpeedButtonDelClick(Sender);

end;

end.
