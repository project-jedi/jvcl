{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgMultiResourceEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgMultiResourceEditorForm;

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgMultiResources, JvComponent;

type
  TJvgResourcesProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TJvgMultipleResourceEdit = class(TJvForm)
    sg: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure sgSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ControlsList: TList;
    procedure LoadDefaults;
  public
    Component: TJvgMultipleResources;
    function GetSub(SrcStr: string; No: Integer; var ResStr: string): Boolean;
  end;

var
  glMresEdit: TJvgMultipleResourceEdit;

implementation

uses
  JvDsgnConsts;

{$R *.dfm}

function TJvgResourcesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvgResourcesProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

procedure TJvgResourcesProperty.Edit;
var
  Dialog: TJvgMultipleResourceEdit;
begin
  TJvgMultipleResources(GetComponent(0)).Update;
  // (rom) is this correct?
  Dialog := glMresEdit.Create(Application);
  Dialog.Component := TJvgMultipleResources(GetComponent(0));
  Dialog.ShowModal;
  Dialog.Free;
  //  GetComponent(0).Owner.Name
end;

procedure TJvgMultipleResourceEdit.LoadDefaults;
var
  ARow: Integer;
begin
  sg.RowCount := Component.Comps.Count + 2;
  for ARow := 1 to Component.Comps.Count do
    sg.Cells[1, ARow] := Component.Comps[ARow - 1];
end;

procedure TJvgMultipleResourceEdit.FormShow(Sender: TObject);
var
  Str, ResStr: string;
  ACol, ARow, SubStrNo: Integer;
begin
  sg.ColCount := 3;
  sg.RowCount := Component.Resources.Count + 2;
  sg.Cells[0, 0] := RsCellControlCaption;
  sg.Cells[1, 0] := RsCellDefaultCaption;
  LoadDefaults;
  with Component, sg do
    for ARow := 1 to Resources.Count do
    begin
      Str := Resources[ARow - 1];
//      uPos1 := 1;
//      uPos2 := uPos1 + 1;
      ACol := 0;
      SubStrNo := 1;
      while GetSub(Str, SubStrNo, ResStr) do
      begin
        Inc(ACol);
        Inc(SubStrNo);
        if sg.ColCount < ACol + 1 then
          sg.ColCount := ACol + 1;
        Cells[ACol, ARow] := ResStr;
      end;
      Cells[0, ARow] := Component.Comps[ARow - 1];
      {    repeat
            if Str[uPos2] = '#' then
            begin
       Cells[ACol, ARow] := Copy(Str, uPos1, uPos2-uPos1);
       Inc(uPos2);
       uPos1 := uPos2;
       Inc(ACol);
            end;
            Inc(uPos2);
          until uPos2 >= Length(Str);}

      //    Cells[ACol, ARow] := Copy(Str, uPos1, uPos2-uPos1+1);

    end;
  sg.FixedCols := 1;
  sg.FixedRows := 1;
end;

function TJvgMultipleResourceEdit.GetSub(SrcStr: string; No: Integer;
  var ResStr: string): Boolean;
var
  Counter, uPos1, uPos2, uPrevPos2: Integer;
begin
  uPos1 := 1;
  uPos2 := 1;
  uPrevPos2 := 1;
  Counter := 0;
  ResStr := '';
  Result := False;
  if SrcStr = '' then
    Exit;
  repeat
    if SrcStr[uPos2] = '#' then
    begin
      Inc(Counter);
      uPos1 := uPrevPos2;
      uPrevPos2 := uPos2;
    end;
    Inc(uPos2);
  until (Counter = No) or (uPos2 = Length(SrcStr));
  Result := True;
  if Counter = No then
    ResStr := Copy(SrcStr, uPos1 - 1, uPos2 - uPos1 - 1)
  else
  if Counter + 1 = No then
    ResStr := Copy(SrcStr, uPrevPos2, uPos2 - uPrevPos2 + 1)
  else
    Result := False;
end;

procedure TJvgMultipleResourceEdit.sgSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if ACol <> 1 then
    Exit;
  //if Tlabel(ControlsList[ARow-1]).Caption <> Value then
//    Tlabel(ControlsList[ARow-1]).Caption := Value;
end;

procedure TJvgMultipleResourceEdit.FormCreate(Sender: TObject);
begin
  ControlsList := TList.Create;
end;

procedure TJvgMultipleResourceEdit.FormDestroy(Sender: TObject);
begin
  ControlsList.Free;
end;

end.
