{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxDBReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvxDBReg;

interface

uses Classes, RTLConsts, DesignIntf, DesignEditors, VCLEditors, SysUtils, DB;

{ Register data aware custom controls and components }

procedure Register;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.D16}
{$ENDIF}

uses TypInfo, JvxLConst, JvxDBCtrl, JvxLookup, JvxLogin, JvxDBComb, JvxVCLUtils,
  {$IFNDEF Delphi3_Up} DBTables, {$ENDIF} {$IFDEF DCS} JvxSelDSFrm, {$ENDIF} 
  {$IFDEF Delphi3_Up} JvxMemDS, {$ENDIF} {$IFDEF WIN32} JvxDBRichEd, {$ENDIF} 
  Consts, LibHelp, JvxDsgn;

{ TJvxFieldProperty }
{ For TJvxDBLookupList, TJvxDBLookupCombo components }

type
  TJvxFieldProperty = class(TJvxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
    function GetDataSourcePropName: string; virtual;
  end;

function TJvxFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

procedure TJvxFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

{$IFDEF DCS}
{$IFDEF Delphi3_Up}

{ TJvxMemoryDataEditor }

type
  TJvxMemoryDataEditor = class(TJvxMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

function TJvxMemoryDataEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvxMemoryData;
  if Result then
    TJvxMemoryData(Dest).CopyStructure(Source);
end;

{$ENDIF Delphi3_Up}
{$ENDIF DCS}

{ Designer registration }

procedure Register;
begin

{$IFDEF Delphi4_Up}
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then Exit;
{$ENDIF}

{ Data aware components and controls }
  RegisterComponents(LoadStr(srRXDBAware), [
    {$IFDEF Delphi3_Up} TJvxMemoryData, {$ENDIF}
    TJvxDBGrid, TJvxDBLookupList, TJvxDBLookupCombo, TJvxLookupEdit, TJvxDBDateEdit, 
    TJvxDBCalcEdit, TJvxDBComboEdit, {$IFDEF WIN32} TJvxDBRichEdit, {$ENDIF}
    TJvxDBStatusLabel, TJvxDBComboBox]);
  RegisterComponents(LoadStr(srRXTools), [TJvxLoginDialog]);
{$IFDEF Delphi3_Up}
  RegisterNonActiveX([TJvxMemoryData, TJvxDBGrid, TJvxDBDateEdit, 
    TJvxDBStatusLabel, TJvxDBComboBox, TJvxDBLookupList,
    TJvxDBLookupCombo, TJvxLookupEdit, TJvxDBComboEdit, TJvxDBCalcEdit, 
    TJvxDBRichEdit, TJvxCustomDBComboBox, TJvxLookupControl, TJvxLoginDialog], 
    axrComponentOnly);
{$ENDIF Delphi3_Up}
{ Property and component editors for data aware components }
  RegisterPropertyEditor(TypeInfo(string), TJvxLookupControl, 'LookupField',
    TJvxFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxLookupEdit, 'LookupField',
    TJvxFieldProperty);
{$IFDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(Integer), TJvxDBGrid, 'RowsHeight', nil);
{$IFDEF DCS}
  RegisterComponentEditor(TJvxMemoryData, TJvxMemoryDataEditor);
{$ENDIF DCS}
{$ENDIF Delphi3_Up}

end;

end.
