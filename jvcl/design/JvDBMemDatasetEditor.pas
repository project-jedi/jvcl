{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBMemDatasetEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDBMemDatasetEditor;

interface
uses
  Classes, DB, SysUtils,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors;
  {$ELSE}
  LibIntf, DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  TJvAbstractMemDataSetEditor = class(TComponentEditor)
  private
    function UniqueName(Field: TField): string;
    procedure BorrowStructure;
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvMemDataSetEditor = class(TJvAbstractMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;


implementation

uses
  DsnDBCst, DSDesign, DBReg, Dialogs,
  JvJVCLUtils, JvMemoryDataSet, JvSelectDataSetForm, JvConsts, JvDsgnConsts;

//=== { TJvAbstractMemDataSetEditor } ========================================

procedure TJvAbstractMemDataSetEditor.BorrowStructure;
const
  {$IFDEF BCB}
  cCaptionFmt = '%s->%s';
  {$ELSE}
   cCaptionFmt = '%s.%s';
  {$ENDIF BCB}
var
  DataSet: TDataSet;
  I: Integer;
  Caption: string;
begin
  Caption := Component.Name;
  if (Component.Owner <> nil) and (Component.Owner.Name <> '') then
    Caption := Format(cCaptionFmt, [Component.Owner.Name, Caption]);
  DataSet := SelectDataSet(Designer, Caption, TDataSet(Component));
  if DataSet <> nil then
  begin
    StartWait;
    try
      if not CopyStructure(DataSet, Component as TDataSet) then
        Exit;
      with TDataSet(Component) do
      begin
        for I := 0 to FieldCount - 1 do
          if Fields[I].Name = '' then
            Fields[I].Name := UniqueName(Fields[I]);
      end;
    finally
      StopWait;
    end;
    Designer.Modified;
  end;
end;

function TJvAbstractMemDataSetEditor.UniqueName(Field: TField): string;
var
  Temp: string;
  Comp: TComponent;
  I: Integer;
begin
  Result := '';
  if (Field <> nil) then
  begin
    Temp := Field.FieldName;
    for I := Length(Temp) downto 1 do
      if not (Temp[I] in IdentifierSymbols) then
        System.Delete(Temp, I, 1);
    if (Temp = '') or not IsValidIdent(Temp) then
    begin
      Temp := Field.ClassName;
      if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
        System.Delete(Temp, 1, 1);
    end;
  end
  else
    Exit;
  Temp := Component.Name + Temp;
  {$IFDEF COMPILER6_UP}
  Comp := Designer.GetComponent(Temp);
  if (Comp = nil) or (Comp = Field) then
    Result := Temp
  else
    Result := Designer.UniqueName(Temp);
  {$ELSE}
  I := 0;
  repeat
    Result := Temp;
    if I > 0 then
      Result := Result + IntToStr(I);
    Comp := Designer.Form.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Field);
  {$ENDIF COMPILER6_UP}
  ShowMessage(Result);
end;

procedure TJvAbstractMemDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
    1:
      BorrowStructure;
  end;
end;

function TJvAbstractMemDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := SDatasetDesigner;
    1:
      Result := RsBorrowStructure;
  end;
end;

function TJvAbstractMemDataSetEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

//=== { TJvMemDataSetEditor } ================================================

function TJvMemDataSetEditor.CopyStructure(Source,
  Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvMemoryData;
  if Result then
    TJvMemoryData(Dest).CopyStructure(Source);
end;

end.
