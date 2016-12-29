{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Db.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_Db;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF RTL240_UP}
  System.Generics.Collections,
  {$ENDIF RTL240_UP}
  Classes, Variants, DB;

type
  {$IFDEF COMPILER12_UP}
  TJvRecordBuffer = TRecordBuffer;  // Delphi 2009
  {$ELSE}
  TJvRecordBuffer = PAnsiChar;
  {$ENDIF COMPILER12_UP}
  
{ EDatabaseError }

{ TFieldDef }

{ constructor Create(Owner: TFieldDefs; Name: string; DataType: TFieldType; Size: Word; Required: Boolean; FieldNo: Integer) }

procedure TFieldDef_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFieldDef.Create(V2O(Args.Values[0]) as TFieldDefs, string(Args.Values[1]), Args.Values[2],
    Args.Values[3], Args.Values[4], Args.Values[5]));
end;

{ function CreateField(Owner: TComponent): TField; }

procedure TFieldDef_CreateField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFieldDef(Args.Obj).CreateField(V2O(Args.Values[0]) as TComponent));
end;

{ property Read InternalCalcField: Boolean }

procedure TFieldDef_Read_InternalCalcField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).InternalCalcField;
end;

{ property Write InternalCalcField(Value: Boolean) }

procedure TFieldDef_Write_InternalCalcField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFieldDef(Args.Obj).InternalCalcField := Value;
end;

{ property Read DataType: TFieldType }

procedure TFieldDef_Read_DataType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).DataType;
end;

{ property Read FieldClass: TFieldClass }

procedure TFieldDef_Read_FieldClass(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := C2V(TFieldDef(Args.Obj).FieldClass);
end;

{ property Read FieldNo: Integer }

procedure TFieldDef_Read_FieldNo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).FieldNo;
end;

{ property Read Name: string }

procedure TFieldDef_Read_Name(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).Name;
end;

{ property Read Precision: Integer }

procedure TFieldDef_Read_Precision(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).Precision;
end;

{ property Write Precision(Value: Integer) }

procedure TFieldDef_Write_Precision(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFieldDef(Args.Obj).Precision := Value;
end;

{ property Read Required: Boolean }

procedure TFieldDef_Read_Required(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).Required;
end;

{ property Read Size: Word }

procedure TFieldDef_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDef(Args.Obj).Size;
end;

{ TFieldDefs }

{ constructor Create(DataSet: TDataSet) }

procedure TFieldDefs_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFieldDefs.Create(V2O(Args.Values[0]) as TDataSet));
end;

{ procedure Add(const Name: string; DataType: TFieldType; Size: Word; Required: Boolean); }

procedure TFieldDefs_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFieldDefs(Args.Obj).Add(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure Assign(FieldDefs: TFieldDefs); }

procedure TFieldDefs_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFieldDefs(Args.Obj).Assign(V2O(Args.Values[0]) as TFieldDefs);
end;

{ procedure Clear; }

procedure TFieldDefs_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFieldDefs(Args.Obj).Clear;
end;

{ function Find(const Name: string): TFieldDef; }

procedure TFieldDefs_Find(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFieldDefs(Args.Obj).Find(Args.Values[0]));
end;

{ function IndexOf(const Name: string): Integer; }

procedure TFieldDefs_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDefs(Args.Obj).IndexOf(Args.Values[0]);
end;

{ procedure Update; }

procedure TFieldDefs_Update(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TFieldDefs(Args.Obj).Update;
end;

{ property Read Count: Integer }

procedure TFieldDefs_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFieldDefs(Args.Obj).Count;
end;

{ property Read Items[Integer]: TFieldDef }

procedure TFieldDefs_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFieldDefs(Args.Obj).Items[Args.Values[0]]);
end;

{ TField }

{ constructor Create(AOwner: TComponent) }

procedure TField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Assign(Source: TPersistent); }

procedure TField_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

(*
{ procedure AssignValue(const Value: TVarRec); }
procedure TField_AssignValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AssignValue(Args.Values[0]);
end;
*)

{ procedure Clear; }

procedure TField_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Clear;
end;

{ procedure FocusControl; }

procedure TField_FocusControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).FocusControl;
end;

{ function GetData(Buffer: Pointer): Boolean; }

procedure TField_GetData(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$WARNINGS OFF} // we want to call the deprecated function
  Value := TField(Args.Obj).GetData(V2P(Args.Values[0]));
  {$WARNINGS ON}
end;

{ function IsBlob: Boolean; }

procedure TField_IsBlob(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).IsBlob;
end;

{ function IsValidChar(InputChar: Char): Boolean; }

procedure TField_IsValidChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).IsValidChar(string(Args.Values[0])[1]);
end;

{ procedure RefreshLookupList; }

procedure TField_RefreshLookupList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).RefreshLookupList;
end;

{ procedure SetData(Buffer: Pointer); }

procedure TField_SetData(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$WARNINGS OFF} // we want to call the deprecated function
  TField(Args.Obj).SetData(V2P(Args.Values[0]));
  {$WARNINGS ON}
end;

{ procedure SetFieldType(Value: TFieldType); }

procedure TField_SetFieldType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).SetFieldType(Args.Values[0]);
end;

{ procedure Validate(Buffer: Pointer); }

procedure TField_Validate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$WARNINGS OFF} // we want to call the deprecated function
  TField(Args.Obj).Validate(V2P(Args.Values[0]));
  {$WARNINGS ON}
end;

{ property Read AsBoolean: Boolean }

procedure TField_Read_AsBoolean(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsBoolean;
end;

{ property Write AsBoolean(Value: Boolean) }

procedure TField_Write_AsBoolean(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsBoolean := Value;
end;

{ property Read AsCurrency: Currency }

procedure TField_Read_AsCurrency(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsCurrency;
end;

{ property Write AsCurrency(Value: Currency) }

procedure TField_Write_AsCurrency(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsCurrency := Value;
end;

{ property Read AsDateTime: TDateTime }

procedure TField_Read_AsDateTime(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsDateTime;
end;

{ property Write AsDateTime(Value: TDateTime) }

procedure TField_Write_AsDateTime(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsDateTime := Value;
end;

{ property Read AsFloat: Double }

procedure TField_Read_AsFloat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsFloat;
end;

{ property Write AsFloat(Value: Double) }

procedure TField_Write_AsFloat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsFloat := Value;
end;

{ property Read AsInteger: Longint }

procedure TField_Read_AsInteger(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsInteger;
end;

{ property Write AsInteger(Value: Longint) }

procedure TField_Write_AsInteger(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsInteger := Value;
end;

{ property Read AsString: string }

procedure TField_Read_AsString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsString;
end;

{ property Write AsString(Value: string) }

procedure TField_Write_AsString(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsString := Value;
end;

{ property Read AsVariant: Variant }

procedure TField_Read_AsVariant(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AsVariant;
end;

{ property Write AsVariant(Value: Variant) }

procedure TField_Write_AsVariant(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AsVariant := Value;
end;

{ property Read AttributeSet: string }

procedure TField_Read_AttributeSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).AttributeSet;
end;

{ property Write AttributeSet(Value: string) }

procedure TField_Write_AttributeSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).AttributeSet := Value;
end;

{ property Read Calculated: Boolean }

procedure TField_Read_Calculated(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Calculated;
end;

{ property Write Calculated(Value: Boolean) }

procedure TField_Write_Calculated(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Calculated := Value;
end;

{ property Read CanModify: Boolean }

procedure TField_Read_CanModify(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).CanModify;
end;

{ property Read CurValue: Variant }

procedure TField_Read_CurValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).CurValue;
end;

{ property Read DataSet: TDataSet }

procedure TField_Read_DataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TField(Args.Obj).DataSet);
end;

{ property Write DataSet(Value: TDataSet) }

procedure TField_Write_DataSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).DataSet := V2O(Value) as TDataSet;
end;

{ property Read DataSize: Word }

procedure TField_Read_DataSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DataSize;
end;

{ property Read DataType: TFieldType }

procedure TField_Read_DataType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DataType;
end;

{ property Read DisplayName: string }

procedure TField_Read_DisplayName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DisplayName;
end;

{ property Read DisplayText: string }

procedure TField_Read_DisplayText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DisplayText;
end;

{ property Read EditMask: string }

procedure TField_Read_EditMask(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).EditMask;
end;

{ property Write EditMask(Value: string) }

procedure TField_Write_EditMask(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).EditMask := Value;
end;

{ property Read EditMaskPtr: string }

procedure TField_Read_EditMaskPtr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).EditMaskPtr;
end;

{ property Read FieldNo: Integer }

procedure TField_Read_FieldNo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).FieldNo;
end;

{ property Read IsIndexField: Boolean }

procedure TField_Read_IsIndexField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).IsIndexField;
end;

{ property Read IsNull: Boolean }

procedure TField_Read_IsNull(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).IsNull;
end;

{ property Read Lookup: Boolean }

procedure TField_Read_Lookup(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Lookup;
end;

{ property Write Lookup(Value: Boolean) }

procedure TField_Write_Lookup(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Lookup := Value;
end;

{ property Read NewValue: Variant }

procedure TField_Read_NewValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).NewValue;
end;

{ property Write NewValue(Value: Variant) }

procedure TField_Write_NewValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).NewValue := Value;
end;

{ property Read Offset: word }

procedure TField_Read_Offset(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Offset;
end;

{ property Read OldValue: Variant }

procedure TField_Read_OldValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).OldValue;
end;

{ property Read Size: Word }

procedure TField_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Size;
end;

{ property Write Size(Value: Word) }

procedure TField_Write_Size(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Size := Value;
end;

{ property Read Text: string }

procedure TField_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Text;
end;

{ property Write Text(Value: string) }

procedure TField_Write_Text(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Text := Value;
end;

(*
{ property Read ValidChars: TFieldChars }
procedure TField_Read_ValidChars(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).ValidChars;
end;

{ property Write ValidChars(Value: TFieldChars) }
procedure TField_Write_ValidChars(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).ValidChars := Value;
end;
*)

{ property Read Value: Variant }

procedure TField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Value;
end;

{ property Write Value(Value: Variant) }

procedure TField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Value := Value;
end;

{ property Read Alignment: TAlignment }

procedure TField_Read_Alignment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Alignment;
end;

{ property Write Alignment(Value: TAlignment) }

procedure TField_Write_Alignment(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Alignment := Value;
end;

{ property Read CustomConstraint: string }

procedure TField_Read_CustomConstraint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).CustomConstraint;
end;

{ property Write CustomConstraint(Value: string) }

procedure TField_Write_CustomConstraint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).CustomConstraint := Value;
end;

{ property Read ConstraintErrorMessage: string }

procedure TField_Read_ConstraintErrorMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).ConstraintErrorMessage;
end;

{ property Write ConstraintErrorMessage(Value: string) }

procedure TField_Write_ConstraintErrorMessage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).ConstraintErrorMessage := Value;
end;

{ property Read DefaultExpression: string }

procedure TField_Read_DefaultExpression(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DefaultExpression;
end;

{ property Write DefaultExpression(Value: string) }

procedure TField_Write_DefaultExpression(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).DefaultExpression := Value;
end;

{ property Read DisplayLabel: string }

procedure TField_Read_DisplayLabel(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DisplayLabel;
end;

{ property Write DisplayLabel(Value: string) }

procedure TField_Write_DisplayLabel(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).DisplayLabel := Value;
end;

{ property Read DisplayWidth: Integer }

procedure TField_Read_DisplayWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).DisplayWidth;
end;

{ property Write DisplayWidth(Value: Integer) }

procedure TField_Write_DisplayWidth(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).DisplayWidth := Value;
end;

{ property Read FieldKind: TFieldKind }

procedure TField_Read_FieldKind(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).FieldKind;
end;

{ property Write FieldKind(Value: TFieldKind) }

procedure TField_Write_FieldKind(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).FieldKind := Value;
end;

{ property Read FieldName: string }

procedure TField_Read_FieldName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).FieldName;
end;

{ property Write FieldName(Value: string) }

procedure TField_Write_FieldName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).FieldName := Value;
end;

{ property Read HasConstraints: Boolean }

procedure TField_Read_HasConstraints(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).HasConstraints;
end;

{ property Read Index: Integer }

procedure TField_Read_Index(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Index;
end;

{ property Write Index(Value: Integer) }

procedure TField_Write_Index(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Index := Value;
end;

{ property Read ImportedConstraint: string }

procedure TField_Read_ImportedConstraint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).ImportedConstraint;
end;

{ property Write ImportedConstraint(Value: string) }

procedure TField_Write_ImportedConstraint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).ImportedConstraint := Value;
end;

{ property Read LookupDataSet: TDataSet }

procedure TField_Read_LookupDataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TField(Args.Obj).LookupDataSet);
end;

{ property Write LookupDataSet(Value: TDataSet) }

procedure TField_Write_LookupDataSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).LookupDataSet := V2O(Value) as TDataSet;
end;

{ property Read LookupKeyFields: string }

procedure TField_Read_LookupKeyFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).LookupKeyFields;
end;

{ property Write LookupKeyFields(Value: string) }

procedure TField_Write_LookupKeyFields(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).LookupKeyFields := Value;
end;

{ property Read LookupResultField: string }

procedure TField_Read_LookupResultField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).LookupResultField;
end;

{ property Write LookupResultField(Value: string) }

procedure TField_Write_LookupResultField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).LookupResultField := Value;
end;

{ property Read KeyFields: string }

procedure TField_Read_KeyFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).KeyFields;
end;

{ property Write KeyFields(Value: string) }

procedure TField_Write_KeyFields(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).KeyFields := Value;
end;

{ property Read LookupCache: Boolean }

procedure TField_Read_LookupCache(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).LookupCache;
end;

{ property Write LookupCache(Value: Boolean) }

procedure TField_Write_LookupCache(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).LookupCache := Value;
end;

{ property Read Origin: string }

procedure TField_Read_Origin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Origin;
end;

{ property Write Origin(Value: string) }

procedure TField_Write_Origin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Origin := Value;
end;

{ property Read ReadOnly: Boolean }

procedure TField_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TField_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).ReadOnly := Value;
end;

{ property Read Required: Boolean }

procedure TField_Read_Required(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Required;
end;

{ property Write Required(Value: Boolean) }

procedure TField_Write_Required(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Required := Value;
end;

{ property Read Visible: Boolean }

procedure TField_Read_Visible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TField(Args.Obj).Visible;
end;

{ property Write Visible(Value: Boolean) }

procedure TField_Write_Visible(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TField(Args.Obj).Visible := Value;
end;

{ TStringField }

{ constructor Create(AOwner: TComponent) }

procedure TStringField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStringField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Value: string }

procedure TStringField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringField(Args.Obj).Value;
end;

{ property Write Value(Value: string) }

procedure TStringField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringField(Args.Obj).Value := AnsiString(VarToStr(Value));  // Potential data loss in D2009 because of cast to AnsiString
end;

{ property Read Transliterate: Boolean }

procedure TStringField_Read_Transliterate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringField(Args.Obj).Transliterate;
end;

{ property Write Transliterate(Value: Boolean) }

procedure TStringField_Write_Transliterate(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringField(Args.Obj).Transliterate := Value;
end;

{ TNumericField }

{ constructor Create(AOwner: TComponent) }

procedure TNumericField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TNumericField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read DisplayFormat: string }

procedure TNumericField_Read_DisplayFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TNumericField(Args.Obj).DisplayFormat;
end;

{ property Write DisplayFormat(Value: string) }

procedure TNumericField_Write_DisplayFormat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TNumericField(Args.Obj).DisplayFormat := Value;
end;

{ property Read EditFormat: string }

procedure TNumericField_Read_EditFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TNumericField(Args.Obj).EditFormat;
end;

{ property Write EditFormat(Value: string) }

procedure TNumericField_Write_EditFormat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TNumericField(Args.Obj).EditFormat := Value;
end;

{ TIntegerField }

{ constructor Create(AOwner: TComponent) }

procedure TIntegerField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIntegerField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Value: Longint }

procedure TIntegerField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIntegerField(Args.Obj).Value;
end;

{ property Write Value(Value: Longint) }

procedure TIntegerField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TIntegerField(Args.Obj).Value := Value;
end;

{ property Read MaxValue: Longint }

procedure TIntegerField_Read_MaxValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIntegerField(Args.Obj).MaxValue;
end;

{ property Write MaxValue(Value: Longint) }

procedure TIntegerField_Write_MaxValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TIntegerField(Args.Obj).MaxValue := Value;
end;

{ property Read MinValue: Longint }

procedure TIntegerField_Read_MinValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIntegerField(Args.Obj).MinValue;
end;

{ property Write MinValue(Value: Longint) }

procedure TIntegerField_Write_MinValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TIntegerField(Args.Obj).MinValue := Value;
end;

{ TSmallintField }

{ constructor Create(AOwner: TComponent) }

procedure TSmallintField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSmallintField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TWordField }

{ constructor Create(AOwner: TComponent) }

procedure TWordField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TWordField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TAutoIncField }

{ constructor Create(AOwner: TComponent) }

procedure TAutoIncField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TAutoIncField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TFloatField }

{ constructor Create(AOwner: TComponent) }

procedure TFloatField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFloatField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Value: Double }

procedure TFloatField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFloatField(Args.Obj).Value;
end;

{ property Write Value(Value: Double) }

procedure TFloatField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFloatField(Args.Obj).Value := Value;
end;

{ property Read Currency: Boolean }

procedure TFloatField_Read_Currency(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFloatField(Args.Obj).Currency;
end;

{ property Write Currency(Value: Boolean) }

procedure TFloatField_Write_Currency(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFloatField(Args.Obj).Currency := Value;
end;

{ property Read MaxValue: Double }

procedure TFloatField_Read_MaxValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFloatField(Args.Obj).MaxValue;
end;

{ property Write MaxValue(Value: Double) }

procedure TFloatField_Write_MaxValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFloatField(Args.Obj).MaxValue := Value;
end;

{ property Read MinValue: Double }

procedure TFloatField_Read_MinValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFloatField(Args.Obj).MinValue;
end;

{ property Write MinValue(Value: Double) }

procedure TFloatField_Write_MinValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFloatField(Args.Obj).MinValue := Value;
end;

{ property Read Precision: Integer }

procedure TFloatField_Read_Precision(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFloatField(Args.Obj).Precision;
end;

{ property Write Precision(Value: Integer) }

procedure TFloatField_Write_Precision(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFloatField(Args.Obj).Precision := Value;
end;

{ TCurrencyField }

{ constructor Create(AOwner: TComponent) }

procedure TCurrencyField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCurrencyField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TBooleanField }

{ constructor Create(AOwner: TComponent) }

procedure TBooleanField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBooleanField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Value: Boolean }

procedure TBooleanField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBooleanField(Args.Obj).Value;
end;

{ property Write Value(Value: Boolean) }

procedure TBooleanField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBooleanField(Args.Obj).Value := Value;
end;

{ property Read DisplayValues: string }

procedure TBooleanField_Read_DisplayValues(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBooleanField(Args.Obj).DisplayValues;
end;

{ property Write DisplayValues(Value: string) }

procedure TBooleanField_Write_DisplayValues(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBooleanField(Args.Obj).DisplayValues := Value;
end;

{ TDateTimeField }

{ constructor Create(AOwner: TComponent) }

procedure TDateTimeField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDateTimeField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Value: TDateTime }

procedure TDateTimeField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDateTimeField(Args.Obj).Value;
end;

{ property Write Value(Value: TDateTime) }

procedure TDateTimeField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDateTimeField(Args.Obj).Value := Value;
end;

{ property Read DisplayFormat: string }

procedure TDateTimeField_Read_DisplayFormat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDateTimeField(Args.Obj).DisplayFormat;
end;

{ property Write DisplayFormat(Value: string) }

procedure TDateTimeField_Write_DisplayFormat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDateTimeField(Args.Obj).DisplayFormat := Value;
end;

{ TDateField }

{ constructor Create(AOwner: TComponent) }

procedure TDateField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDateField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TTimeField }

{ constructor Create(AOwner: TComponent) }

procedure TTimeField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTimeField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TBinaryField }

{ constructor Create(AOwner: TComponent) }

procedure TBinaryField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBinaryField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TBytesField }

{ constructor Create(AOwner: TComponent) }

procedure TBytesField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBytesField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TVarBytesField }

{ constructor Create(AOwner: TComponent) }

procedure TVarBytesField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TVarBytesField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TBCDField }

{ constructor Create(AOwner: TComponent) }

procedure TBCDField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBCDField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Value: Currency }

procedure TBCDField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBCDField(Args.Obj).Value;
end;

{ property Write Value(Value: Currency) }

procedure TBCDField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBCDField(Args.Obj).Value := Value;
end;

{ property Read Currency: Boolean }

procedure TBCDField_Read_Currency(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBCDField(Args.Obj).Currency;
end;

{ property Write Currency(Value: Boolean) }

procedure TBCDField_Write_Currency(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBCDField(Args.Obj).Currency := Value;
end;

{ property Read MaxValue: Currency }

procedure TBCDField_Read_MaxValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBCDField(Args.Obj).MaxValue;
end;

{ property Write MaxValue(Value: Currency) }

procedure TBCDField_Write_MaxValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBCDField(Args.Obj).MaxValue := Value;
end;

{ property Read MinValue: Currency }

procedure TBCDField_Read_MinValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBCDField(Args.Obj).MinValue;
end;

{ property Write MinValue(Value: Currency) }

procedure TBCDField_Write_MinValue(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBCDField(Args.Obj).MinValue := Value;
end;

{ TBlobField }

{ constructor Create(AOwner: TComponent) }

procedure TBlobField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBlobField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Assign(Source: TPersistent); }

procedure TBlobField_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure Clear; }

procedure TBlobField_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).Clear;
end;

{ function IsBlob: Boolean; }

procedure TBlobField_IsBlob(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobField(Args.Obj).IsBlob;
end;

{ procedure LoadFromFile(const FileName: string); }

procedure TBlobField_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).LoadFromFile(Args.Values[0]);
end;

{ procedure LoadFromStream(Stream: TStream); }

procedure TBlobField_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure SaveToFile(const FileName: string); }

procedure TBlobField_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).SaveToFile(Args.Values[0]);
end;

{ procedure SaveToStream(Stream: TStream); }

procedure TBlobField_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure SetFieldType(Value: TFieldType); }

procedure TBlobField_SetFieldType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).SetFieldType(Args.Values[0]);
end;

{ property Read BlobSize: Integer }

procedure TBlobField_Read_BlobSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobField(Args.Obj).BlobSize;
end;

{ property Read Modified: Boolean }

procedure TBlobField_Read_Modified(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobField(Args.Obj).Modified;
end;

{ property Write Modified(Value: Boolean) }

procedure TBlobField_Write_Modified(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).Modified := Value;
end;

{ property Read Value: string }

procedure TBlobField_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobField(Args.Obj).Value;
end;

{ property Write Value(Value: string) }

procedure TBlobField_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).Value := Value;
end;

{ property Read Transliterate: Boolean }

procedure TBlobField_Read_Transliterate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobField(Args.Obj).Transliterate;
end;

{ property Write Transliterate(Value: Boolean) }

procedure TBlobField_Write_Transliterate(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).Transliterate := Value;
end;

{ property Read BlobType: TBlobType }

procedure TBlobField_Read_BlobType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobField(Args.Obj).BlobType;
end;

{ property Write BlobType(Value: TBlobType) }

procedure TBlobField_Write_BlobType(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobField(Args.Obj).BlobType := Value;
end;

{ TMemoField }

{ constructor Create(AOwner: TComponent) }

procedure TMemoField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TMemoField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TGraphicField }

{ constructor Create(AOwner: TComponent) }

procedure TGraphicField_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TGraphicField.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TIndexDef }

{ constructor Create(Owner: TIndexDefs; Name: string; Fields: string; Options: TIndexOptions) }

procedure TIndexDef_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIndexDef.Create(V2O(Args.Values[0]) as TIndexDefs, string(Args.Values[1]), string(Args.Values[2]),
    TIndexOptions(Byte(V2S(Args.Values[3])))));
end;

{ property Read Expression: string }

procedure TIndexDef_Read_Expression(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDef(Args.Obj).Expression;
end;

{ property Read Fields: string }

procedure TIndexDef_Read_Fields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDef(Args.Obj).Fields;
end;

{ property Read Name: string }

procedure TIndexDef_Read_Name(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDef(Args.Obj).Name;
end;

{ property Read Options: TIndexOptions }

procedure TIndexDef_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TIndexDef(Args.Obj).Options));
end;

{ property Read Source: string }

procedure TIndexDef_Read_Source(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDef(Args.Obj).Source;
end;

{ property Write Source(Value: string) }

procedure TIndexDef_Write_Source(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TIndexDef(Args.Obj).Source := Value;
end;

{ TIndexDefs }

{ constructor Create(DataSet: TDataSet) }

procedure TIndexDefs_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIndexDefs.Create(V2O(Args.Values[0]) as TDataSet));
end;

{ procedure Add(const Name, Fields: string; Options: TIndexOptions); }

procedure TIndexDefs_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIndexDefs(Args.Obj).Add(Args.Values[0], Args.Values[1], TIndexOptions(Byte(V2S(Args.Values[2]))));
end;

{ procedure Assign(IndexDefs: TIndexDefs); }

procedure TIndexDefs_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIndexDefs(Args.Obj).Assign(V2O(Args.Values[0]) as TIndexDefs);
end;

{ procedure Clear; }

procedure TIndexDefs_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIndexDefs(Args.Obj).Clear;
end;

{ function FindIndexForFields(const Fields: string): TIndexDef; }

procedure TIndexDefs_FindIndexForFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIndexDefs(Args.Obj).FindIndexForFields(Args.Values[0]));
end;

{ function GetIndexForFields(const Fields: string; CaseInsensitive: Boolean): TIndexDef; }

procedure TIndexDefs_GetIndexForFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIndexDefs(Args.Obj).GetIndexForFields(Args.Values[0], Args.Values[1]));
end;

{ function IndexOf(const Name: string): Integer; }

procedure TIndexDefs_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDefs(Args.Obj).IndexOf(Args.Values[0]);
end;

{ procedure Update; }

procedure TIndexDefs_Update(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TIndexDefs(Args.Obj).Update;
end;

{ property Read Count: Integer }

procedure TIndexDefs_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDefs(Args.Obj).Count;
end;

{ property Read Items[Integer]: TIndexDef }

procedure TIndexDefs_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TIndexDefs(Args.Obj).Items[Args.Values[0]]);
end;

{ property Read Updated: Boolean }

procedure TIndexDefs_Read_Updated(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TIndexDefs(Args.Obj).Updated;
end;

{ property Write Updated(Value: Boolean) }

procedure TIndexDefs_Write_Updated(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TIndexDefs(Args.Obj).Updated := Value;
end;

{ TDataLink }

{ constructor Create }

procedure TDataLink_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataLink.Create);
end;

{ function Edit: Boolean; }

procedure TDataLink_Edit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).Edit;
end;

{ procedure UpdateRecord; }

procedure TDataLink_UpdateRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataLink(Args.Obj).UpdateRecord;
end;

{ property Read Active: Boolean }

procedure TDataLink_Read_Active(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).Active;
end;

{ property Read ActiveRecord: Integer }

procedure TDataLink_Read_ActiveRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).ActiveRecord;
end;

{ property Write ActiveRecord(Value: Integer) }

procedure TDataLink_Write_ActiveRecord(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataLink(Args.Obj).ActiveRecord := Value;
end;

{ property Read BufferCount: Integer }

procedure TDataLink_Read_BufferCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).BufferCount;
end;

{ property Write BufferCount(Value: Integer) }

procedure TDataLink_Write_BufferCount(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataLink(Args.Obj).BufferCount := Value;
end;

{ property Read DataSet: TDataSet }

procedure TDataLink_Read_DataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataLink(Args.Obj).DataSet);
end;

{ property Read DataSource: TDataSource }

procedure TDataLink_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataLink(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDataLink_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataLink(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read DataSourceFixed: Boolean }

procedure TDataLink_Read_DataSourceFixed(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).DataSourceFixed;
end;

{ property Write DataSourceFixed(Value: Boolean) }

procedure TDataLink_Write_DataSourceFixed(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataLink(Args.Obj).DataSourceFixed := Value;
end;

{ property Read Editing: Boolean }

procedure TDataLink_Read_Editing(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).Editing;
end;

{ property Read ReadOnly: Boolean }

procedure TDataLink_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDataLink_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataLink(Args.Obj).ReadOnly := Value;
end;

{ property Read RecordCount: Integer }

procedure TDataLink_Read_RecordCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataLink(Args.Obj).RecordCount;
end;

{ TDataSource }

{ constructor Create(AOwner: TComponent) }

procedure TDataSource_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSource.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Edit; }

procedure TDataSource_Edit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSource(Args.Obj).Edit;
end;

{ function IsLinkedTo(DataSet: TDataSet): Boolean; }

procedure TDataSource_IsLinkedTo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSource(Args.Obj).IsLinkedTo(V2O(Args.Values[0]) as TDataSet);
end;

{ property Read State: TDataSetState }

procedure TDataSource_Read_State(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSource(Args.Obj).State;
end;

{ property Read AutoEdit: Boolean }

procedure TDataSource_Read_AutoEdit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSource(Args.Obj).AutoEdit;
end;

{ property Write AutoEdit(Value: Boolean) }

procedure TDataSource_Write_AutoEdit(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSource(Args.Obj).AutoEdit := Value;
end;

{ property Read DataSet: TDataSet }

procedure TDataSource_Read_DataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSource(Args.Obj).DataSet);
end;

{ property Write DataSet(Value: TDataSet) }

procedure TDataSource_Write_DataSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSource(Args.Obj).DataSet := V2O(Value) as TDataSet;
end;

{ property Read Enabled: Boolean }

procedure TDataSource_Read_Enabled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSource(Args.Obj).Enabled;
end;

{ property Write Enabled(Value: Boolean) }

procedure TDataSource_Write_Enabled(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSource(Args.Obj).Enabled := Value;
end;

{ TCheckConstraint }

{ procedure Assign(Source: TPersistent); }

procedure TCheckConstraint_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCheckConstraint(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ function GetDisplayName: string; }

procedure TCheckConstraint_GetDisplayName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCheckConstraint(Args.Obj).GetDisplayName;
end;

{ property Read CustomConstraint: string }

procedure TCheckConstraint_Read_CustomConstraint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCheckConstraint(Args.Obj).CustomConstraint;
end;

{ property Write CustomConstraint(Value: string) }

procedure TCheckConstraint_Write_CustomConstraint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCheckConstraint(Args.Obj).CustomConstraint := Value;
end;

{ property Read ErrorMessage: string }

procedure TCheckConstraint_Read_ErrorMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCheckConstraint(Args.Obj).ErrorMessage;
end;

{ property Write ErrorMessage(Value: string) }

procedure TCheckConstraint_Write_ErrorMessage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCheckConstraint(Args.Obj).ErrorMessage := Value;
end;

{ property Read FromDictionary: Boolean }

procedure TCheckConstraint_Read_FromDictionary(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCheckConstraint(Args.Obj).FromDictionary;
end;

{ property Write FromDictionary(Value: Boolean) }

procedure TCheckConstraint_Write_FromDictionary(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCheckConstraint(Args.Obj).FromDictionary := Value;
end;

{ property Read ImportedConstraint: string }

procedure TCheckConstraint_Read_ImportedConstraint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCheckConstraint(Args.Obj).ImportedConstraint;
end;

{ property Write ImportedConstraint(Value: string) }

procedure TCheckConstraint_Write_ImportedConstraint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCheckConstraint(Args.Obj).ImportedConstraint := Value;
end;

{ TCheckConstraints }

{ constructor Create(Owner: TPersistent) }

procedure TCheckConstraints_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCheckConstraints.Create(V2O(Args.Values[0]) as TPersistent));
end;

{ function Add: TCheckConstraint; }

procedure TCheckConstraints_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCheckConstraints(Args.Obj).Add);
end;

{ property Read Items[Integer]: TCheckConstraint }

procedure TCheckConstraints_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCheckConstraints(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TCheckConstraint }

procedure TCheckConstraints_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCheckConstraints(Args.Obj).Items[Args.Values[0]] := V2O(Value) as TCheckConstraint;
end;

{ TDataSet }

{ function ActiveBuffer: PChar; }

procedure TDataSet_ActiveBuffer(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := string(PString(TDataSet(Args.Obj).ActiveBuffer));
end;

{ procedure Append; }

procedure TDataSet_Append(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Append;
end;

{ procedure AppendRecord(const Values: array of const); }

procedure TDataSet_AppendRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(0);
  TDataSet(Args.Obj).AppendRecord(Slice(Args.OA^, Args.OAS));
end;

{ function BookmarkValid(Bookmark: TBookmark): Boolean; }

procedure TDataSet_BookmarkValid(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).BookmarkValid({$IFDEF RTL200_UP}TBookmark{$ENDIF RTL200_UP}(V2P(Args.Values[0])));
end;

{ procedure Cancel; }

procedure TDataSet_Cancel(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Cancel;
end;

{ procedure CheckBrowseMode; }

procedure TDataSet_CheckBrowseMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).CheckBrowseMode;
end;

{ procedure ClearFields; }

procedure TDataSet_ClearFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).ClearFields;
end;

{ procedure Close; }

procedure TDataSet_Close(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Close;
end;

{ function ControlsDisabled: Boolean; }

procedure TDataSet_ControlsDisabled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).ControlsDisabled;
end;

{ function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; }

procedure TDataSet_CompareBookmarks(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).CompareBookmarks({$IFDEF RTL200_UP}TBookmark{$ENDIF RTL200_UP}(V2P(Args.Values[0])), {$IFDEF RTL200_UP}TBookmark{$ENDIF RTL200_UP}(V2P(Args.Values[1])));
end;

{ function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; }

procedure TDataSet_CreateBlobStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).CreateBlobStream(V2O(Args.Values[0]) as TField, Args.Values[1]));
end;

{ procedure CursorPosChanged; }

procedure TDataSet_CursorPosChanged(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).CursorPosChanged;
end;

{ procedure Delete; }

procedure TDataSet_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Delete;
end;

{ procedure DisableControls; }

procedure TDataSet_DisableControls(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).DisableControls;
end;

{ procedure Edit; }

procedure TDataSet_Edit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Edit;
end;

{ procedure EnableControls; }

procedure TDataSet_EnableControls(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).EnableControls;
end;

{ function FieldByName(const FieldName: string): TField; }

procedure TDataSet_FieldByName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).FieldByName(Args.Values[0]));
end;

{ function FindField(const FieldName: string): TField; }

procedure TDataSet_FindField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).FindField(Args.Values[0]));
end;

{ function FindFirst: Boolean; }

procedure TDataSet_FindFirst(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).FindFirst;
end;

{ function FindLast: Boolean; }

procedure TDataSet_FindLast(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).FindLast;
end;

{ function FindNext: Boolean; }

procedure TDataSet_FindNext(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).FindNext;
end;

{ function FindPrior: Boolean; }

procedure TDataSet_FindPrior(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).FindPrior;
end;

{ procedure First; }

procedure TDataSet_First(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).First;
end;

{ procedure FreeBookmark(Bookmark: TBookmark); }

procedure TDataSet_FreeBookmark(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).FreeBookmark({$IFDEF RTL200_UP}TBookmark{$ENDIF RTL200_UP}(V2P(Args.Values[0])));
end;

{ function GetBookmark: TBookmark; }

procedure TDataSet_GetBookmark(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TDataSet(Args.Obj).GetBookmark);
end;

{ function GetCurrentRecord(Buffer: PChar): Boolean; }

procedure TDataSet_GetCurrentRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).GetCurrentRecord({$IFDEF RTL250_UP}TRecBuf{$ENDIF}(TJvRecordBuffer(AnsiString(Args.Values[0]))));
end;

{ procedure GetFieldList(List: TList; const FieldNames: string); }

procedure TDataSet_GetFieldList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).GetFieldList(V2O(Args.Values[0]) as TList{$IFDEF RTL240_UP}<TField>{$ENDIF RTL240_UP}, Args.Values[1]);
end;

{ procedure GetFieldNames(List: TStrings); }

procedure TDataSet_GetFieldNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF COMPILER10_UP}
  {$WARN SYMBOL_DEPRECATED OFF}
  TDataSet(Args.Obj).GetFieldNames(V2O(Args.Values[0]) as TStrings);
  {$WARN SYMBOL_DEPRECATED ON}
  {$ELSE}
  TDataSet(Args.Obj).GetFieldNames(V2O(Args.Values[0]) as TStrings);
  {$ENDIF COMPILER10_UP}
end;

{ procedure GotoBookmark(Bookmark: TBookmark); }

procedure TDataSet_GotoBookmark(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).GotoBookmark({$IFDEF RTL200_UP}TBookmark{$ENDIF RTL200_UP}(V2P(Args.Values[0])));
end;

{ procedure Insert; }

procedure TDataSet_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Insert;
end;

{ procedure InsertRecord(const Values: array of const); }

procedure TDataSet_InsertRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(0);
  TDataSet(Args.Obj).InsertRecord(Slice(Args.OA^, Args.OAS));
end;

{ function IsEmpty: Boolean; }

procedure TDataSet_IsEmpty(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).IsEmpty;
end;

{ function IsLinkedTo(DataSource: TDataSource): Boolean; }

procedure TDataSet_IsLinkedTo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).IsLinkedTo(V2O(Args.Values[0]) as TDataSource);
end;

{ function IsSequenced: Boolean; }

procedure TDataSet_IsSequenced(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).IsSequenced;
end;

{ procedure Last; }

procedure TDataSet_Last(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Last;
end;

{ function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; }

procedure TDataSet_Locate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Locate(Args.Values[0], Args.Values[1], TLocateOptions(Byte(V2S(Args.Values[2]))));
end;

{ function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; }

procedure TDataSet_Lookup(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Lookup(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function MoveBy(Distance: Integer): Integer; }

procedure TDataSet_MoveBy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).MoveBy(Args.Values[0]);
end;

{ procedure Next; }

procedure TDataSet_Next(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Next;
end;

{ procedure Open; }

procedure TDataSet_Open(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Open;
end;

{ procedure Post; }

procedure TDataSet_Post(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Post;
end;

{ procedure Prior; }

procedure TDataSet_Prior(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Prior;
end;

{ procedure Refresh; }

procedure TDataSet_Refresh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Refresh;
end;

{ procedure Resync(Mode: TResyncMode); }

procedure TDataSet_Resync(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Resync(TResyncMode(Byte(V2S(Args.Values[0]))));
end;

{ procedure SetFields(const Values: array of const); }

procedure TDataSet_SetFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(0);
  TDataSet(Args.Obj).SetFields(Slice(Args.OA^, Args.OAS));
end;

{ procedure Translate(Src, Dest: PChar; ToOem: Boolean); }

{$WARNINGS OFF} // we want to call the deprecated function
procedure TDataSet_Translate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Translate(PAnsiChar(AnsiString(Args.Values[0])),
                               PAnsiChar(AnsiString(Args.Values[1])),
                               Args.Values[2]);
end;
{$WARNINGS ON}

{ procedure UpdateCursorPos; }

procedure TDataSet_UpdateCursorPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).UpdateCursorPos;
end;

{ procedure UpdateRecord; }

procedure TDataSet_UpdateRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).UpdateRecord;
end;

{ property Read BOF: Boolean }

procedure TDataSet_Read_BOF(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).BOF;
end;

{ property Read Bookmark: TBookmarkStr }

procedure TDataSet_Read_Bookmark(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Bookmark;
end;

{ property Write Bookmark(Value: TBookmarkStr) }

procedure TDataSet_Write_Bookmark(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Bookmark := Value;
end;

{ property Read CanModify: Boolean }

procedure TDataSet_Read_CanModify(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).CanModify;
end;

{ property Read DataSource: TDataSource }

procedure TDataSet_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).DataSource);
end;

{ property Read DefaultFields: Boolean }

procedure TDataSet_Read_DefaultFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$WARNINGS OFF}
  Value := TDataSet(Args.Obj).DefaultFields;
  {$WARNINGS ON}
end;

{ property Read Designer: TDataSetDesigner }

procedure TDataSet_Read_Designer(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).Designer);
end;

{ property Read EOF: Boolean }

procedure TDataSet_Read_EOF(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).EOF;
end;

{ property Read FieldCount: Integer }

procedure TDataSet_Read_FieldCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).FieldCount;
end;

{ property Read FieldDefs: TFieldDefs }

procedure TDataSet_Read_FieldDefs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).FieldDefs);
end;

{ property Write FieldDefs(Value: TFieldDefs) }

procedure TDataSet_Write_FieldDefs(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).FieldDefs := V2O(Value) as TFieldDefs;
end;

{ property Read Fields[Integer]: TField }

procedure TDataSet_Read_Fields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataSet(Args.Obj).Fields[Args.Values[0]]);
end;

{ property Read FieldValues[string]: Variant }

procedure TDataSet_Read_FieldValues(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).FieldValues[Args.Values[0]];
end;

{ property Write FieldValues[string]: Variant }

procedure TDataSet_Write_FieldValues(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).FieldValues[Args.Values[0]] := Value;
end;

{ property Read Found: Boolean }

procedure TDataSet_Read_Found(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Found;
end;

{ property Read Modified: Boolean }

procedure TDataSet_Read_Modified(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Modified;
end;

{ property Read RecordCount: Integer }

procedure TDataSet_Read_RecordCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).RecordCount;
end;

{ property Read RecNo: Integer }

procedure TDataSet_Read_RecNo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).RecNo;
end;

{ property Write RecNo(Value: Integer) }

procedure TDataSet_Write_RecNo(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).RecNo := Value;
end;

{ property Read RecordSize: Word }

procedure TDataSet_Read_RecordSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).RecordSize;
end;

{ property Read State: TDataSetState }

procedure TDataSet_Read_State(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).State;
end;

{ property Read Filter: string }

procedure TDataSet_Read_Filter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Filter;
end;

{ property Write Filter(Value: string) }

procedure TDataSet_Write_Filter(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Filter := Value;
end;

{ property Read Filtered: Boolean }

procedure TDataSet_Read_Filtered(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Filtered;
end;

{ property Write Filtered(Value: Boolean) }

procedure TDataSet_Write_Filtered(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Filtered := Value;
end;

{ property Read FilterOptions: TFilterOptions }

procedure TDataSet_Read_FilterOptions(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TDataSet(Args.Obj).FilterOptions));
end;

{ property Write FilterOptions(Value: TFilterOptions) }

procedure TDataSet_Write_FilterOptions(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).FilterOptions := TFilterOptions(Byte(V2S(Value)));
end;

{ property Read Active: Boolean }

procedure TDataSet_Read_Active(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).Active;
end;

{ property Write Active(Value: Boolean) }

procedure TDataSet_Write_Active(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).Active := Value;
end;

{ property Read AutoCalcFields: Boolean }

procedure TDataSet_Read_AutoCalcFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDataSet(Args.Obj).AutoCalcFields;
end;

{ property Write AutoCalcFields(Value: Boolean) }

procedure TDataSet_Write_AutoCalcFields(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDataSet(Args.Obj).AutoCalcFields := Value;
end;

type

  TJvInterpreterDbEvent = class(TJvInterpreterEvent)
  private
    procedure FieldNotifyEvent(Sender: TField);
    procedure FieldGetTextEvent(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure FieldSetTextEvent(Sender: TField; const Text: string);
    procedure DataChangeEvent(Sender: TObject; Field: TField);
    procedure DataSetNotifyEvent(DataSet: TDataSet);
    procedure DataSetErrorEvent(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    procedure FilterRecordEvent(DataSet: TDataSet; var Accept: Boolean);
  end;

procedure TJvInterpreterDbEvent.FieldNotifyEvent(Sender: TField);
begin
  CallFunction(nil, [O2V(Sender)]);
end;

procedure TJvInterpreterDbEvent.FieldGetTextEvent(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  CallFunction(nil, [O2V(Sender), Text, DisplayText]);
  Text := Args.Values[1];
end;

procedure TJvInterpreterDbEvent.FieldSetTextEvent(Sender: TField; const Text: string);
begin
  CallFunction(nil, [O2V(Sender), Text]);
end;

procedure TJvInterpreterDbEvent.DataChangeEvent(Sender: TObject; Field: TField);
begin
  CallFunction(nil, [O2V(Sender), O2V(Field)]);
end;

procedure TJvInterpreterDbEvent.DataSetNotifyEvent(DataSet: TDataSet);
begin
  CallFunction(nil, [O2V(DataSet)]);
end;

procedure TJvInterpreterDbEvent.DataSetErrorEvent(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
begin
  CallFunction(nil, [O2V(DataSet), O2V(E), Action]);
  Action := Args.Values[2];
end;

procedure TJvInterpreterDbEvent.FilterRecordEvent(DataSet: TDataSet; var Accept: Boolean);
begin
  CallFunction(nil, [O2V(DataSet), Accept]);
  Accept := Args.Values[1];
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
 cDb = 'Db';
begin
  with JvInterpreterAdapter do
  begin
    { TDataSetState }
    AddConst(cDb, 'dsInactive', Ord(dsInactive));
    AddConst(cDb, 'dsBrowse', Ord(dsBrowse));
    AddConst(cDb, 'dsEdit', Ord(dsEdit));
    AddConst(cDb, 'dsInsert', Ord(dsInsert));
    AddConst(cDb, 'dsSetKey', Ord(dsSetKey));
    AddConst(cDb, 'dsCalcFields', Ord(dsCalcFields));
    AddConst(cDb, 'dsFilter', Ord(dsFilter));
    AddConst(cDb, 'dsNewValue', Ord(dsNewValue));
    AddConst(cDb, 'dsOldValue', Ord(dsOldValue));
    AddConst(cDb, 'dsCurValue', Ord(dsCurValue));
    { TDataEvent }
    AddConst(cDb, 'deFieldChange', Ord(deFieldChange));
    AddConst(cDb, 'deRecordChange', Ord(deRecordChange));
    AddConst(cDb, 'deDataSetChange', Ord(deDataSetChange));
    AddConst(cDb, 'deDataSetScroll', Ord(deDataSetScroll));
    AddConst(cDb, 'deLayoutChange', Ord(deLayoutChange));
    AddConst(cDb, 'deUpdateRecord', Ord(deUpdateRecord));
    AddConst(cDb, 'deUpdateState', Ord(deUpdateState));
    AddConst(cDb, 'deCheckBrowseMode', Ord(deCheckBrowseMode));
    AddConst(cDb, 'dePropertyChange', Ord(dePropertyChange));
    AddConst(cDb, 'deFieldListChange', Ord(deFieldListChange));
    AddConst(cDb, 'deFocusControl', Ord(deFocusControl));
    { TUpdateStatus }
    AddConst(cDb, 'usUnmodified', Ord(usUnmodified));
    AddConst(cDb, 'usModified', Ord(usModified));
    AddConst(cDb, 'usInserted', Ord(usInserted));
    AddConst(cDb, 'usDeleted', Ord(usDeleted));
    { EDatabaseError }
    AddClass(cDb, EDatabaseError, 'EDatabaseError');
    { TFieldType }
    AddConst(cDb, 'ftUnknown', Ord(ftUnknown));
    AddConst(cDb, 'ftString', Ord(ftString));
    AddConst(cDb, 'ftSmallint', Ord(ftSmallint));
    AddConst(cDb, 'ftInteger', Ord(ftInteger));
    AddConst(cDb, 'ftWord', Ord(ftWord));
    AddConst(cDb, 'ftBoolean', Ord(ftBoolean));
    AddConst(cDb, 'ftFloat', Ord(ftFloat));
    AddConst(cDb, 'ftCurrency', Ord(ftCurrency));
    AddConst(cDb, 'ftBCD', Ord(ftBCD));
    AddConst(cDb, 'ftDate', Ord(ftDate));
    AddConst(cDb, 'ftTime', Ord(ftTime));
    AddConst(cDb, 'ftDateTime', Ord(ftDateTime));
    AddConst(cDb, 'ftBytes', Ord(ftBytes));
    AddConst(cDb, 'ftVarBytes', Ord(ftVarBytes));
    AddConst(cDb, 'ftAutoInc', Ord(ftAutoInc));
    AddConst(cDb, 'ftBlob', Ord(ftBlob));
    AddConst(cDb, 'ftMemo', Ord(ftMemo));
    AddConst(cDb, 'ftGraphic', Ord(ftGraphic));
    AddConst(cDb, 'ftFmtMemo', Ord(ftFmtMemo));
    AddConst(cDb, 'ftParadoxOle', Ord(ftParadoxOle));
    AddConst(cDb, 'ftDBaseOle', Ord(ftDBaseOle));
    AddConst(cDb, 'ftTypedBinary', Ord(ftTypedBinary));
    AddConst(cDb, 'ftCursor', Ord(ftCursor));
    AddConst(cDb, 'ftFMTBCD', Ord(ftFMTBCD));
    AddConst(cDb, 'ftTimestamp', Ord(ftTimestamp));
    {$IFDEF COMPILER10_UP}
    AddConst(cDb, 'ftFixedWideChar', Ord(ftFixedWideChar));
    AddConst(cDb, 'ftWideMemo', Ord(ftWideMemo));
    AddConst(cDb, 'ftOraTimeStamp', Ord(ftOraTimeStamp));
    AddConst(cDb, 'ftOraInterval', Ord(ftOraInterval));
    {$ENDIF COMPILER10_UP}
    {$IFDEF COMPILER12_UP}
    AddConst(cDb, 'ftLongWord', Ord(ftLongWord));
    AddConst(cDb, 'ftShortint', Ord(ftShortint));
    AddConst(cDb, 'ftByte', Ord(ftByte));
    AddConst(cDb, 'ftExtended', Ord(ftExtended));
    AddConst(cDb, 'ftConnection', Ord(ftConnection));
    AddConst(cDb, 'ftParams', Ord(ftParams));
    AddConst(cDb, 'ftStream', Ord(ftStream));
    {$ENDIF COMPILER12_UP}
    { TFieldDef }
    AddClass(cDb, TFieldDef, 'TFieldDef');
    AddGet(TFieldDef, 'Create', TFieldDef_Create, 6, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TFieldDef, 'CreateField', TFieldDef_CreateField, 1, [varEmpty], varEmpty);
    AddGet(TFieldDef, 'InternalCalcField', TFieldDef_Read_InternalCalcField, 0, [varEmpty], varEmpty);
    AddSet(TFieldDef, 'InternalCalcField', TFieldDef_Write_InternalCalcField, 0, [varEmpty]);
    AddGet(TFieldDef, 'DataType', TFieldDef_Read_DataType, 0, [varEmpty], varEmpty);
    AddGet(TFieldDef, 'FieldClass', TFieldDef_Read_FieldClass, 0, [varEmpty], varEmpty);
    AddGet(TFieldDef, 'FieldNo', TFieldDef_Read_FieldNo, 0, [varEmpty], varEmpty);
    AddGet(TFieldDef, 'Name', TFieldDef_Read_Name, 0, [varEmpty], varEmpty);
    AddGet(TFieldDef, 'Precision', TFieldDef_Read_Precision, 0, [varEmpty], varEmpty);
    AddSet(TFieldDef, 'Precision', TFieldDef_Write_Precision, 0, [varEmpty]);
    AddGet(TFieldDef, 'Required', TFieldDef_Read_Required, 0, [varEmpty], varEmpty);
    AddGet(TFieldDef, 'Size', TFieldDef_Read_Size, 0, [varEmpty], varEmpty);
    { TFieldDefs }
    AddClass(cDb, TFieldDefs, 'TFieldDefs');
    AddGet(TFieldDefs, 'Create', TFieldDefs_Create, 1, [varEmpty], varEmpty);
    AddGet(TFieldDefs, 'Add', TFieldDefs_Add, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TFieldDefs, 'Assign', TFieldDefs_Assign, 1, [varEmpty], varEmpty);
    AddGet(TFieldDefs, 'Clear', TFieldDefs_Clear, 0, [varEmpty], varEmpty);
    AddGet(TFieldDefs, 'Find', TFieldDefs_Find, 1, [varEmpty], varEmpty);
    AddGet(TFieldDefs, 'IndexOf', TFieldDefs_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TFieldDefs, 'Update', TFieldDefs_Update, 0, [varEmpty], varEmpty);
    AddGet(TFieldDefs, 'Count', TFieldDefs_Read_Count, 0, [varEmpty], varEmpty);
    AddIGet(TFieldDefs, 'Items', TFieldDefs_Read_Items, 1, [varEmpty], varEmpty);
    { TFieldKind }
    AddConst(cDb, 'fkData', Ord(fkData));
    AddConst(cDb, 'fkCalculated', Ord(fkCalculated));
    AddConst(cDb, 'fkLookup', Ord(fkLookup));
    AddConst(cDb, 'fkInternalCalc', Ord(fkInternalCalc));
    { TField }
    AddClass(cDb, TField, 'TField');
    AddGet(TField, 'Create', TField_Create, 1, [varEmpty], varEmpty);
    AddGet(TField, 'Assign', TField_Assign, 1, [varEmpty], varEmpty);
    // AddGet(TField, 'AssignValue', TField_AssignValue, 1, [varEmpty], nil);
    AddGet(TField, 'Clear', TField_Clear, 0, [varEmpty], varEmpty);
    AddGet(TField, 'FocusControl', TField_FocusControl, 0, [varEmpty], varEmpty);
    AddGet(TField, 'GetData', TField_GetData, 1, [varEmpty], varEmpty);
    AddGet(TField, 'IsBlob', TField_IsBlob, 0, [varEmpty], varEmpty);
    AddGet(TField, 'IsValidChar', TField_IsValidChar, 1, [varEmpty], varEmpty);
    AddGet(TField, 'RefreshLookupList', TField_RefreshLookupList, 0, [varEmpty], varEmpty);
    AddGet(TField, 'SetData', TField_SetData, 1, [varEmpty], varEmpty);
    AddGet(TField, 'SetFieldType', TField_SetFieldType, 1, [varEmpty], varEmpty);
    AddGet(TField, 'Validate', TField_Validate, 1, [varEmpty], varEmpty);
    AddGet(TField, 'AsBoolean', TField_Read_AsBoolean, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsBoolean', TField_Write_AsBoolean, 0, [varEmpty]);
    AddGet(TField, 'AsCurrency', TField_Read_AsCurrency, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsCurrency', TField_Write_AsCurrency, 0, [varEmpty]);
    AddGet(TField, 'AsDateTime', TField_Read_AsDateTime, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsDateTime', TField_Write_AsDateTime, 0, [varEmpty]);
    AddGet(TField, 'AsFloat', TField_Read_AsFloat, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsFloat', TField_Write_AsFloat, 0, [varEmpty]);
    AddGet(TField, 'AsInteger', TField_Read_AsInteger, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsInteger', TField_Write_AsInteger, 0, [varEmpty]);
    AddGet(TField, 'AsString', TField_Read_AsString, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsString', TField_Write_AsString, 0, [varEmpty]);
    AddGet(TField, 'AsVariant', TField_Read_AsVariant, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AsVariant', TField_Write_AsVariant, 0, [varEmpty]);
    AddGet(TField, 'AttributeSet', TField_Read_AttributeSet, 0, [varEmpty], varEmpty);
    AddSet(TField, 'AttributeSet', TField_Write_AttributeSet, 0, [varEmpty]);
    AddGet(TField, 'Calculated', TField_Read_Calculated, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Calculated', TField_Write_Calculated, 0, [varEmpty]);
    AddGet(TField, 'CanModify', TField_Read_CanModify, 0, [varEmpty], varEmpty);
    AddGet(TField, 'CurValue', TField_Read_CurValue, 0, [varEmpty], varEmpty);
    AddGet(TField, 'DataSet', TField_Read_DataSet, 0, [varEmpty], varEmpty);
    AddSet(TField, 'DataSet', TField_Write_DataSet, 0, [varEmpty]);
    AddGet(TField, 'DataSize', TField_Read_DataSize, 0, [varEmpty], varEmpty);
    AddGet(TField, 'DataType', TField_Read_DataType, 0, [varEmpty], varEmpty);
    AddGet(TField, 'DisplayName', TField_Read_DisplayName, 0, [varEmpty], varEmpty);
    AddGet(TField, 'DisplayText', TField_Read_DisplayText, 0, [varEmpty], varEmpty);
    AddGet(TField, 'EditMask', TField_Read_EditMask, 0, [varEmpty], varEmpty);
    AddSet(TField, 'EditMask', TField_Write_EditMask, 0, [varEmpty]);
    AddGet(TField, 'EditMaskPtr', TField_Read_EditMaskPtr, 0, [varEmpty], varEmpty);
    AddGet(TField, 'FieldNo', TField_Read_FieldNo, 0, [varEmpty], varEmpty);
    AddGet(TField, 'IsIndexField', TField_Read_IsIndexField, 0, [varEmpty], varEmpty);
    AddGet(TField, 'IsNull', TField_Read_IsNull, 0, [varEmpty], varEmpty);
    AddGet(TField, 'Lookup', TField_Read_Lookup, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Lookup', TField_Write_Lookup, 0, [varEmpty]);
    AddGet(TField, 'NewValue', TField_Read_NewValue, 0, [varEmpty], varEmpty);
    AddSet(TField, 'NewValue', TField_Write_NewValue, 0, [varEmpty]);
    AddGet(TField, 'Offset', TField_Read_Offset, 0, [varEmpty], varEmpty);
    AddGet(TField, 'OldValue', TField_Read_OldValue, 0, [varEmpty], varEmpty);
    AddGet(TField, 'Size', TField_Read_Size, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Size', TField_Write_Size, 0, [varEmpty]);
    AddGet(TField, 'Text', TField_Read_Text, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Text', TField_Write_Text, 0, [varEmpty]);
    { AddGet(TField, 'ValidChars', TField_Read_ValidChars, 0, [varEmpty], nil);
    AddSet(TField, 'ValidChars', TField_Write_ValidChars, 0, [varEmpty]); }
    AddGet(TField, 'Value', TField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Value', TField_Write_Value, 0, [varEmpty]);
    AddGet(TField, 'Alignment', TField_Read_Alignment, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Alignment', TField_Write_Alignment, 0, [varEmpty]);
    AddGet(TField, 'CustomConstraint', TField_Read_CustomConstraint, 0, [varEmpty], varEmpty);
    AddSet(TField, 'CustomConstraint', TField_Write_CustomConstraint, 0, [varEmpty]);
    AddGet(TField, 'ConstraintErrorMessage', TField_Read_ConstraintErrorMessage, 0, [varEmpty], varEmpty);
    AddSet(TField, 'ConstraintErrorMessage', TField_Write_ConstraintErrorMessage, 0, [varEmpty]);
    AddGet(TField, 'DefaultExpression', TField_Read_DefaultExpression, 0, [varEmpty], varEmpty);
    AddSet(TField, 'DefaultExpression', TField_Write_DefaultExpression, 0, [varEmpty]);
    AddGet(TField, 'DisplayLabel', TField_Read_DisplayLabel, 0, [varEmpty], varEmpty);
    AddSet(TField, 'DisplayLabel', TField_Write_DisplayLabel, 0, [varEmpty]);
    AddGet(TField, 'DisplayWidth', TField_Read_DisplayWidth, 0, [varEmpty], varEmpty);
    AddSet(TField, 'DisplayWidth', TField_Write_DisplayWidth, 0, [varEmpty]);
    AddGet(TField, 'FieldKind', TField_Read_FieldKind, 0, [varEmpty], varEmpty);
    AddSet(TField, 'FieldKind', TField_Write_FieldKind, 0, [varEmpty]);
    AddGet(TField, 'FieldName', TField_Read_FieldName, 0, [varEmpty], varEmpty);
    AddSet(TField, 'FieldName', TField_Write_FieldName, 0, [varEmpty]);
    AddGet(TField, 'HasConstraints', TField_Read_HasConstraints, 0, [varEmpty], varEmpty);
    AddGet(TField, 'Index', TField_Read_Index, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Index', TField_Write_Index, 0, [varEmpty]);
    AddGet(TField, 'ImportedConstraint', TField_Read_ImportedConstraint, 0, [varEmpty], varEmpty);
    AddSet(TField, 'ImportedConstraint', TField_Write_ImportedConstraint, 0, [varEmpty]);
    AddGet(TField, 'LookupDataSet', TField_Read_LookupDataSet, 0, [varEmpty], varEmpty);
    AddSet(TField, 'LookupDataSet', TField_Write_LookupDataSet, 0, [varEmpty]);
    AddGet(TField, 'LookupKeyFields', TField_Read_LookupKeyFields, 0, [varEmpty], varEmpty);
    AddSet(TField, 'LookupKeyFields', TField_Write_LookupKeyFields, 0, [varEmpty]);
    AddGet(TField, 'LookupResultField', TField_Read_LookupResultField, 0, [varEmpty], varEmpty);
    AddSet(TField, 'LookupResultField', TField_Write_LookupResultField, 0, [varEmpty]);
    AddGet(TField, 'KeyFields', TField_Read_KeyFields, 0, [varEmpty], varEmpty);
    AddSet(TField, 'KeyFields', TField_Write_KeyFields, 0, [varEmpty]);
    AddGet(TField, 'LookupCache', TField_Read_LookupCache, 0, [varEmpty], varEmpty);
    AddSet(TField, 'LookupCache', TField_Write_LookupCache, 0, [varEmpty]);
    AddGet(TField, 'Origin', TField_Read_Origin, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Origin', TField_Write_Origin, 0, [varEmpty]);
    AddGet(TField, 'ReadOnly', TField_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TField, 'ReadOnly', TField_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TField, 'Required', TField_Read_Required, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Required', TField_Write_Required, 0, [varEmpty]);
    AddGet(TField, 'Visible', TField_Read_Visible, 0, [varEmpty], varEmpty);
    AddSet(TField, 'Visible', TField_Write_Visible, 0, [varEmpty]);
    { TStringField }
    AddClass(cDb, TStringField, 'TStringField');
    AddGet(TStringField, 'Create', TStringField_Create, 1, [varEmpty], varEmpty);
    AddGet(TStringField, 'Value', TStringField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TStringField, 'Value', TStringField_Write_Value, 0, [varEmpty]);
    AddGet(TStringField, 'Transliterate', TStringField_Read_Transliterate, 0, [varEmpty], varEmpty);
    AddSet(TStringField, 'Transliterate', TStringField_Write_Transliterate, 0, [varEmpty]);
    { TNumericField }
    AddClass(cDb, TNumericField, 'TNumericField');
    AddGet(TNumericField, 'Create', TNumericField_Create, 1, [varEmpty], varEmpty);
    AddGet(TNumericField, 'DisplayFormat', TNumericField_Read_DisplayFormat, 0, [varEmpty], varEmpty);
    AddSet(TNumericField, 'DisplayFormat', TNumericField_Write_DisplayFormat, 0, [varEmpty]);
    AddGet(TNumericField, 'EditFormat', TNumericField_Read_EditFormat, 0, [varEmpty], varEmpty);
    AddSet(TNumericField, 'EditFormat', TNumericField_Write_EditFormat, 0, [varEmpty]);
    { TIntegerField }
    AddClass(cDb, TIntegerField, 'TIntegerField');
    AddGet(TIntegerField, 'Create', TIntegerField_Create, 1, [varEmpty], varEmpty);
    AddGet(TIntegerField, 'Value', TIntegerField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TIntegerField, 'Value', TIntegerField_Write_Value, 0, [varEmpty]);
    AddGet(TIntegerField, 'MaxValue', TIntegerField_Read_MaxValue, 0, [varEmpty], varEmpty);
    AddSet(TIntegerField, 'MaxValue', TIntegerField_Write_MaxValue, 0, [varEmpty]);
    AddGet(TIntegerField, 'MinValue', TIntegerField_Read_MinValue, 0, [varEmpty], varEmpty);
    AddSet(TIntegerField, 'MinValue', TIntegerField_Write_MinValue, 0, [varEmpty]);
    { TSmallintField }
    AddClass(cDb, TSmallintField, 'TSmallintField');
    AddGet(TSmallintField, 'Create', TSmallintField_Create, 1, [varEmpty], varEmpty);
    { TWordField }
    AddClass(cDb, TWordField, 'TWordField');
    AddGet(TWordField, 'Create', TWordField_Create, 1, [varEmpty], varEmpty);
    { TAutoIncField }
    AddClass(cDb, TAutoIncField, 'TAutoIncField');
    AddGet(TAutoIncField, 'Create', TAutoIncField_Create, 1, [varEmpty], varEmpty);
    { TFloatField }
    AddClass(cDb, TFloatField, 'TFloatField');
    AddGet(TFloatField, 'Create', TFloatField_Create, 1, [varEmpty], varEmpty);
    AddGet(TFloatField, 'Value', TFloatField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TFloatField, 'Value', TFloatField_Write_Value, 0, [varEmpty]);
    AddGet(TFloatField, 'Currency', TFloatField_Read_Currency, 0, [varEmpty], varEmpty);
    AddSet(TFloatField, 'Currency', TFloatField_Write_Currency, 0, [varEmpty]);
    AddGet(TFloatField, 'MaxValue', TFloatField_Read_MaxValue, 0, [varEmpty], varEmpty);
    AddSet(TFloatField, 'MaxValue', TFloatField_Write_MaxValue, 0, [varEmpty]);
    AddGet(TFloatField, 'MinValue', TFloatField_Read_MinValue, 0, [varEmpty], varEmpty);
    AddSet(TFloatField, 'MinValue', TFloatField_Write_MinValue, 0, [varEmpty]);
    AddGet(TFloatField, 'Precision', TFloatField_Read_Precision, 0, [varEmpty], varEmpty);
    AddSet(TFloatField, 'Precision', TFloatField_Write_Precision, 0, [varEmpty]);
    { TCurrencyField }
    AddClass(cDb, TCurrencyField, 'TCurrencyField');
    AddGet(TCurrencyField, 'Create', TCurrencyField_Create, 1, [varEmpty], varEmpty);
    { TBooleanField }
    AddClass(cDb, TBooleanField, 'TBooleanField');
    AddGet(TBooleanField, 'Create', TBooleanField_Create, 1, [varEmpty], varEmpty);
    AddGet(TBooleanField, 'Value', TBooleanField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TBooleanField, 'Value', TBooleanField_Write_Value, 0, [varEmpty]);
    AddGet(TBooleanField, 'DisplayValues', TBooleanField_Read_DisplayValues, 0, [varEmpty], varEmpty);
    AddSet(TBooleanField, 'DisplayValues', TBooleanField_Write_DisplayValues, 0, [varEmpty]);
    { TDateTimeField }
    AddClass(cDb, TDateTimeField, 'TDateTimeField');
    AddGet(TDateTimeField, 'Create', TDateTimeField_Create, 1, [varEmpty], varEmpty);
    AddGet(TDateTimeField, 'Value', TDateTimeField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TDateTimeField, 'Value', TDateTimeField_Write_Value, 0, [varEmpty]);
    AddGet(TDateTimeField, 'DisplayFormat', TDateTimeField_Read_DisplayFormat, 0, [varEmpty], varEmpty);
    AddSet(TDateTimeField, 'DisplayFormat', TDateTimeField_Write_DisplayFormat, 0, [varEmpty]);
    { TDateField }
    AddClass(cDb, TDateField, 'TDateField');
    AddGet(TDateField, 'Create', TDateField_Create, 1, [varEmpty], varEmpty);
    { TTimeField }
    AddClass(cDb, TTimeField, 'TTimeField');
    AddGet(TTimeField, 'Create', TTimeField_Create, 1, [varEmpty], varEmpty);
    { TBinaryField }
    AddClass(cDb, TBinaryField, 'TBinaryField');
    AddGet(TBinaryField, 'Create', TBinaryField_Create, 1, [varEmpty], varEmpty);
    { TBytesField }
    AddClass(cDb, TBytesField, 'TBytesField');
    AddGet(TBytesField, 'Create', TBytesField_Create, 1, [varEmpty], varEmpty);
    { TVarBytesField }
    AddClass(cDb, TVarBytesField, 'TVarBytesField');
    AddGet(TVarBytesField, 'Create', TVarBytesField_Create, 1, [varEmpty], varEmpty);
    { TBCDField }
    AddClass(cDb, TBCDField, 'TBCDField');
    AddGet(TBCDField, 'Create', TBCDField_Create, 1, [varEmpty], varEmpty);
    AddGet(TBCDField, 'Value', TBCDField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TBCDField, 'Value', TBCDField_Write_Value, 0, [varEmpty]);
    AddGet(TBCDField, 'Currency', TBCDField_Read_Currency, 0, [varEmpty], varEmpty);
    AddSet(TBCDField, 'Currency', TBCDField_Write_Currency, 0, [varEmpty]);
    AddGet(TBCDField, 'MaxValue', TBCDField_Read_MaxValue, 0, [varEmpty], varEmpty);
    AddSet(TBCDField, 'MaxValue', TBCDField_Write_MaxValue, 0, [varEmpty]);
    AddGet(TBCDField, 'MinValue', TBCDField_Read_MinValue, 0, [varEmpty], varEmpty);
    AddSet(TBCDField, 'MinValue', TBCDField_Write_MinValue, 0, [varEmpty]);
    { TBlobField }
    AddClass(cDb, TBlobField, 'TBlobField');
    AddGet(TBlobField, 'Create', TBlobField_Create, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'Assign', TBlobField_Assign, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'Clear', TBlobField_Clear, 0, [varEmpty], varEmpty);
    AddGet(TBlobField, 'IsBlob', TBlobField_IsBlob, 0, [varEmpty], varEmpty);
    AddGet(TBlobField, 'LoadFromFile', TBlobField_LoadFromFile, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'LoadFromStream', TBlobField_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'SaveToFile', TBlobField_SaveToFile, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'SaveToStream', TBlobField_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'SetFieldType', TBlobField_SetFieldType, 1, [varEmpty], varEmpty);
    AddGet(TBlobField, 'BlobSize', TBlobField_Read_BlobSize, 0, [varEmpty], varEmpty);
    AddGet(TBlobField, 'Modified', TBlobField_Read_Modified, 0, [varEmpty], varEmpty);
    AddSet(TBlobField, 'Modified', TBlobField_Write_Modified, 0, [varEmpty]);
    AddGet(TBlobField, 'Value', TBlobField_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TBlobField, 'Value', TBlobField_Write_Value, 0, [varEmpty]);
    AddGet(TBlobField, 'Transliterate', TBlobField_Read_Transliterate, 0, [varEmpty], varEmpty);
    AddSet(TBlobField, 'Transliterate', TBlobField_Write_Transliterate, 0, [varEmpty]);
    AddGet(TBlobField, 'BlobType', TBlobField_Read_BlobType, 0, [varEmpty], varEmpty);
    AddSet(TBlobField, 'BlobType', TBlobField_Write_BlobType, 0, [varEmpty]);
    { TMemoField }
    AddClass(cDb, TMemoField, 'TMemoField');
    AddGet(TMemoField, 'Create', TMemoField_Create, 1, [varEmpty], varEmpty);
    { TGraphicField }
    AddClass(cDb, TGraphicField, 'TGraphicField');
    AddGet(TGraphicField, 'Create', TGraphicField_Create, 1, [varEmpty], varEmpty);
    { TIndexOptions }
    AddConst(cDb, 'ixPrimary', Ord(ixPrimary));
    AddConst(cDb, 'ixUnique', Ord(ixUnique));
    AddConst(cDb, 'ixDescending', Ord(ixDescending));
    AddConst(cDb, 'ixCaseInsensitive', Ord(ixCaseInsensitive));
    AddConst(cDb, 'ixExpression', Ord(ixExpression));
    { TIndexDef }
    AddClass(cDb, TIndexDef, 'TIndexDef');
    AddGet(TIndexDef, 'Create', TIndexDef_Create, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TIndexDef, 'Expression', TIndexDef_Read_Expression, 0, [varEmpty], varEmpty);
    AddGet(TIndexDef, 'Fields', TIndexDef_Read_Fields, 0, [varEmpty], varEmpty);
    AddGet(TIndexDef, 'Name', TIndexDef_Read_Name, 0, [varEmpty], varEmpty);
    AddGet(TIndexDef, 'Options', TIndexDef_Read_Options, 0, [varEmpty], varEmpty);
    AddGet(TIndexDef, 'Source', TIndexDef_Read_Source, 0, [varEmpty], varEmpty);
    AddSet(TIndexDef, 'Source', TIndexDef_Write_Source, 0, [varEmpty]);
    { TIndexDefs }
    AddClass(cDb, TIndexDefs, 'TIndexDefs');
    AddGet(TIndexDefs, 'Create', TIndexDefs_Create, 1, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'Add', TIndexDefs_Add, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TIndexDefs, 'Assign', TIndexDefs_Assign, 1, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'Clear', TIndexDefs_Clear, 0, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'FindIndexForFields', TIndexDefs_FindIndexForFields, 1, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'GetIndexForFields', TIndexDefs_GetIndexForFields, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TIndexDefs, 'IndexOf', TIndexDefs_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'Update', TIndexDefs_Update, 0, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'Count', TIndexDefs_Read_Count, 0, [varEmpty], varEmpty);
    AddIGet(TIndexDefs, 'Items', TIndexDefs_Read_Items, 1, [varEmpty], varEmpty);
    AddIDGet(TIndexDefs, TIndexDefs_Read_Items, 1, [varEmpty], varEmpty);
    AddGet(TIndexDefs, 'Updated', TIndexDefs_Read_Updated, 0, [varEmpty], varEmpty);
    AddSet(TIndexDefs, 'Updated', TIndexDefs_Write_Updated, 0, [varEmpty]);
    { TDataLink }
    AddClass(cDb, TDataLink, 'TDataLink');
    AddGet(TDataLink, 'Create', TDataLink_Create, 0, [varEmpty], varEmpty);
    AddGet(TDataLink, 'Edit', TDataLink_Edit, 0, [varEmpty], varEmpty);
    AddGet(TDataLink, 'UpdateRecord', TDataLink_UpdateRecord, 0, [varEmpty], varEmpty);
    AddGet(TDataLink, 'Active', TDataLink_Read_Active, 0, [varEmpty], varEmpty);
    AddGet(TDataLink, 'ActiveRecord', TDataLink_Read_ActiveRecord, 0, [varEmpty], varEmpty);
    AddSet(TDataLink, 'ActiveRecord', TDataLink_Write_ActiveRecord, 0, [varEmpty]);
    AddGet(TDataLink, 'BufferCount', TDataLink_Read_BufferCount, 0, [varEmpty], varEmpty);
    AddSet(TDataLink, 'BufferCount', TDataLink_Write_BufferCount, 0, [varEmpty]);
    AddGet(TDataLink, 'DataSet', TDataLink_Read_DataSet, 0, [varEmpty], varEmpty);
    AddGet(TDataLink, 'DataSource', TDataLink_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDataLink, 'DataSource', TDataLink_Write_DataSource, 0, [varEmpty]);
    AddGet(TDataLink, 'DataSourceFixed', TDataLink_Read_DataSourceFixed, 0, [varEmpty], varEmpty);
    AddSet(TDataLink, 'DataSourceFixed', TDataLink_Write_DataSourceFixed, 0, [varEmpty]);
    AddGet(TDataLink, 'Editing', TDataLink_Read_Editing, 0, [varEmpty], varEmpty);
    AddGet(TDataLink, 'ReadOnly', TDataLink_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDataLink, 'ReadOnly', TDataLink_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TDataLink, 'RecordCount', TDataLink_Read_RecordCount, 0, [varEmpty], varEmpty);
    { TDataSource }
    AddClass(cDb, TDataSource, 'TDataSource');
    AddGet(TDataSource, 'Create', TDataSource_Create, 1, [varEmpty], varEmpty);
    AddGet(TDataSource, 'Edit', TDataSource_Edit, 0, [varEmpty], varEmpty);
    AddGet(TDataSource, 'IsLinkedTo', TDataSource_IsLinkedTo, 1, [varEmpty], varEmpty);
    AddGet(TDataSource, 'State', TDataSource_Read_State, 0, [varEmpty], varEmpty);
    AddGet(TDataSource, 'AutoEdit', TDataSource_Read_AutoEdit, 0, [varEmpty], varEmpty);
    AddSet(TDataSource, 'AutoEdit', TDataSource_Write_AutoEdit, 0, [varEmpty]);
    AddGet(TDataSource, 'DataSet', TDataSource_Read_DataSet, 0, [varEmpty], varEmpty);
    AddSet(TDataSource, 'DataSet', TDataSource_Write_DataSet, 0, [varEmpty]);
    AddGet(TDataSource, 'Enabled', TDataSource_Read_Enabled, 0, [varEmpty], varEmpty);
    AddSet(TDataSource, 'Enabled', TDataSource_Write_Enabled, 0, [varEmpty]);
    { TCheckConstraint }
    AddClass(cDb, TCheckConstraint, 'TCheckConstraint');
    AddGet(TCheckConstraint, 'Assign', TCheckConstraint_Assign, 1, [varEmpty], varEmpty);
    AddGet(TCheckConstraint, 'GetDisplayName', TCheckConstraint_GetDisplayName, 0, [varEmpty], varEmpty);
    AddGet(TCheckConstraint, 'CustomConstraint', TCheckConstraint_Read_CustomConstraint, 0, [varEmpty], varEmpty);
    AddSet(TCheckConstraint, 'CustomConstraint', TCheckConstraint_Write_CustomConstraint, 0, [varEmpty]);
    AddGet(TCheckConstraint, 'ErrorMessage', TCheckConstraint_Read_ErrorMessage, 0, [varEmpty], varEmpty);
    AddSet(TCheckConstraint, 'ErrorMessage', TCheckConstraint_Write_ErrorMessage, 0, [varEmpty]);
    AddGet(TCheckConstraint, 'FromDictionary', TCheckConstraint_Read_FromDictionary, 0, [varEmpty], varEmpty);
    AddSet(TCheckConstraint, 'FromDictionary', TCheckConstraint_Write_FromDictionary, 0, [varEmpty]);
    AddGet(TCheckConstraint, 'ImportedConstraint', TCheckConstraint_Read_ImportedConstraint, 0, [varEmpty], varEmpty);
    AddSet(TCheckConstraint, 'ImportedConstraint', TCheckConstraint_Write_ImportedConstraint, 0, [varEmpty]);
    { TCheckConstraints }
    AddClass(cDb, TCheckConstraints, 'TCheckConstraints');
    AddGet(TCheckConstraints, 'Create', TCheckConstraints_Create, 1, [varEmpty], varEmpty);
    AddGet(TCheckConstraints, 'Add', TCheckConstraints_Add, 0, [varEmpty], varEmpty);
    AddIGet(TCheckConstraints, 'Items', TCheckConstraints_Read_Items, 1, [varEmpty], varEmpty);
    // (rom) varEmpty replaced by varNull
    AddISet(TCheckConstraints, 'Items', TCheckConstraints_Write_Items, 1, [varNull]);
    { TBookmarkFlag }
    AddConst(cDb, 'bfCurrent', Ord(bfCurrent));
    AddConst(cDb, 'bfBOF', Ord(bfBOF));
    AddConst(cDb, 'bfEOF', Ord(bfEOF));
    AddConst(cDb, 'bfInserted', Ord(bfInserted));
    { TGetMode }
    AddConst(cDb, 'gmCurrent', Ord(gmCurrent));
    AddConst(cDb, 'gmNext', Ord(gmNext));
    AddConst(cDb, 'gmPrior', Ord(gmPrior));
    { TGetResult }
    AddConst(cDb, 'grOK', Ord(grOK));
    AddConst(cDb, 'grBOF', Ord(grBOF));
    AddConst(cDb, 'grEOF', Ord(grEOF));
    AddConst(cDb, 'grError', Ord(grError));
    { TResyncMode }
    AddConst(cDb, 'rmExact', Ord(rmExact));
    AddConst(cDb, 'rmCenter', Ord(rmCenter));
    { TDataAction }
    AddConst(cDb, 'daFail', Ord(daFail));
    AddConst(cDb, 'daAbort', Ord(daAbort));
    AddConst(cDb, 'daRetry', Ord(daRetry));
    { TUpdateKind }
    AddConst(cDb, 'ukModify', Ord(ukModify));
    AddConst(cDb, 'ukInsert', Ord(ukInsert));
    AddConst(cDb, 'ukDelete', Ord(ukDelete));
    { TBlobStreamMode }
    AddConst(cDb, 'bmRead', Ord(bmRead));
    AddConst(cDb, 'bmWrite', Ord(bmWrite));
    AddConst(cDb, 'bmReadWrite', Ord(bmReadWrite));
    { TLocateOption }
    AddConst(cDb, 'loCaseInsensitive', Ord(loCaseInsensitive));
    AddConst(cDb, 'loPartialKey', Ord(loPartialKey));
    { TFilterOption }
    AddConst(cDb, 'foCaseInsensitive', Ord(foCaseInsensitive));
    AddConst(cDb, 'foNoPartialCompare', Ord(foNoPartialCompare));
    { TDataSet }
    AddClass(cDb, TDataSet, 'TDataSet');
    AddGet(TDataSet, 'ActiveBuffer', TDataSet_ActiveBuffer, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Append', TDataSet_Append, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'AppendRecord', TDataSet_AppendRecord, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'BookmarkValid', TDataSet_BookmarkValid, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Cancel', TDataSet_Cancel, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'CheckBrowseMode', TDataSet_CheckBrowseMode, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'ClearFields', TDataSet_ClearFields, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Close', TDataSet_Close, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'ControlsDisabled', TDataSet_ControlsDisabled, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'CompareBookmarks', TDataSet_CompareBookmarks, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TDataSet, 'CreateBlobStream', TDataSet_CreateBlobStream, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TDataSet, 'CursorPosChanged', TDataSet_CursorPosChanged, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Delete', TDataSet_Delete, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'DisableControls', TDataSet_DisableControls, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Edit', TDataSet_Edit, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'EnableControls', TDataSet_EnableControls, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FieldByName', TDataSet_FieldByName, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FindField', TDataSet_FindField, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FindFirst', TDataSet_FindFirst, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FindLast', TDataSet_FindLast, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FindNext', TDataSet_FindNext, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FindPrior', TDataSet_FindPrior, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'First', TDataSet_First, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FreeBookmark', TDataSet_FreeBookmark, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'GetBookmark', TDataSet_GetBookmark, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'GetCurrentRecord', TDataSet_GetCurrentRecord, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'GetFieldList', TDataSet_GetFieldList, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TDataSet, 'GetFieldNames', TDataSet_GetFieldNames, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'GotoBookmark', TDataSet_GotoBookmark, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Insert', TDataSet_Insert, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'InsertRecord', TDataSet_InsertRecord, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'IsEmpty', TDataSet_IsEmpty, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'IsLinkedTo', TDataSet_IsLinkedTo, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'IsSequenced', TDataSet_IsSequenced, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Last', TDataSet_Last, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Locate', TDataSet_Locate, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDataSet, 'Lookup', TDataSet_Lookup, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDataSet, 'MoveBy', TDataSet_MoveBy, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Next', TDataSet_Next, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Open', TDataSet_Open, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Post', TDataSet_Post, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Prior', TDataSet_Prior, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Refresh', TDataSet_Refresh, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Resync', TDataSet_Resync, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'SetFields', TDataSet_SetFields, 1, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Translate', TDataSet_Translate, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDataSet, 'UpdateCursorPos', TDataSet_UpdateCursorPos, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'UpdateRecord', TDataSet_UpdateRecord, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'BOF', TDataSet_Read_BOF, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Bookmark', TDataSet_Read_Bookmark, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'Bookmark', TDataSet_Write_Bookmark, 0, [varEmpty]);
    AddGet(TDataSet, 'CanModify', TDataSet_Read_CanModify, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'DataSource', TDataSet_Read_DataSource, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'DefaultFields', TDataSet_Read_DefaultFields, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Designer', TDataSet_Read_Designer, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'EOF', TDataSet_Read_EOF, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FieldCount', TDataSet_Read_FieldCount, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'FieldDefs', TDataSet_Read_FieldDefs, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'FieldDefs', TDataSet_Write_FieldDefs, 0, [varEmpty]);
    AddIGet(TDataSet, 'Fields', TDataSet_Read_Fields, 1, [varEmpty], varEmpty);
    AddIGet(TDataSet, 'FieldValues', TDataSet_Read_FieldValues, 1, [varEmpty], varEmpty);
    // (rom) varEmpty replaced by varNull
    AddISet(TDataSet, 'FieldValues', TDataSet_Write_FieldValues, 1, [varNull]);
    AddIDGet(TDataSet, TDataSet_Read_FieldValues, 1, [varEmpty], varEmpty);
    // (rom) varEmpty replaced by varNull
    AddIDSet(TDataSet, TDataSet_Write_FieldValues, 1, [varNull]);
    AddGet(TDataSet, 'Found', TDataSet_Read_Found, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Modified', TDataSet_Read_Modified, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'RecordCount', TDataSet_Read_RecordCount, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'RecNo', TDataSet_Read_RecNo, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'RecNo', TDataSet_Write_RecNo, 0, [varEmpty]);
    AddGet(TDataSet, 'RecordSize', TDataSet_Read_RecordSize, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'State', TDataSet_Read_State, 0, [varEmpty], varEmpty);
    AddGet(TDataSet, 'Filter', TDataSet_Read_Filter, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'Filter', TDataSet_Write_Filter, 0, [varEmpty]);
    AddGet(TDataSet, 'Filtered', TDataSet_Read_Filtered, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'Filtered', TDataSet_Write_Filtered, 0, [varEmpty]);
    AddGet(TDataSet, 'FilterOptions', TDataSet_Read_FilterOptions, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'FilterOptions', TDataSet_Write_FilterOptions, 0, [varEmpty]);
    AddGet(TDataSet, 'Active', TDataSet_Read_Active, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'Active', TDataSet_Write_Active, 0, [varEmpty]);
    AddGet(TDataSet, 'AutoCalcFields', TDataSet_Read_AutoCalcFields, 0, [varEmpty], varEmpty);
    AddSet(TDataSet, 'AutoCalcFields', TDataSet_Write_AutoCalcFields, 0, [varEmpty]);

    AddHandler(cDb, 'TFieldNotifyEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.FieldNotifyEvent);
    AddHandler(cDb, 'TFieldGetTextEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.FieldGetTextEvent);
    AddHandler(cDb, 'TFieldSetTextEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.FieldSetTextEvent);
    AddHandler(cDb, 'TDataChangeEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.DataChangeEvent);
    AddHandler(cDb, 'TDataSetNotifyEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.DataSetNotifyEvent);
    AddHandler(cDb, 'TDataSetErrorEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.DataSetErrorEvent);
    AddHandler(cDb, 'TFilterRecordEvent', TJvInterpreterDbEvent, @TJvInterpreterDbEvent.FilterRecordEvent);
  end;
  RegisterClasses([TStringField, TNumericField, TIntegerField, TSmallintField,
    TWordField, TAutoIncField, TFloatField, TCurrencyField, TBooleanField,
      TDateTimeField, TDateField, TTimeField, TBinaryField, TBytesField,
      TVarBytesField, TBCDField, TBlobField, TMemoField, TGraphicField,
      TDataSource, TCheckConstraint, TCheckConstraints]);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
