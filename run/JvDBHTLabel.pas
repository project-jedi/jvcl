{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBHTLabel.PAS, released on 2004-02-01.

The Initial Developers of the Original Code are: Maciej Kaczkowski
Copyright (c) 2003 Maciej Kaczkowski
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's
JVCL home page, located at http://jvcl.sourceforge.net

Known Issues:
- To display data from a datasource, use the <FIELD="fieldname"> tag in Mask.
- You can have more than one FIELD tag in a label, i.e:
  <b>Name:</b><i><FIELD="contact"></i>, <b>Company:</b><i><FIELD="Company"></i>
- The fieldname *must* be double-quoted!
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDBHTLabel;

interface

uses
  SysUtils, Classes, DB,
  {$IFDEF VCL}
  DBCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QDBCtrls,
  {$ENDIF VisualCLX}
  JvHTControls;

type
  TJvDBHTLabel = class(TJvCustomHTLabel)
  private
    FDataLink: TFieldDataLink;
    FMask: string;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure SetMask(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateCaption;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Mask: string read FMask write SetMask;

    property Align;
    property AutoSize;
    property Constraints;
    property Color;
    property Layout;
    {$IFDEF VCL}
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property Canvas;
    {$ENDIF VisualCLX}
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnHyperLinkClick;
  end;

implementation

function ReplaceFieldNameTag(Str: string; DataSet: TDataSet): string;
var
  A, FieldName, Text: string;
  I, J: Integer;
  F: TField;
const
  FieldStr: string = '<FIELD='; // non-standard html
  function ExtractPropertyValue(Tag, PropName: string): string;
  begin
    Result := '';
    PropName := UpperCase(PropName);
    if Pos(PropName, UpperCase(Tag)) > 0 then
    begin
      Result := Copy(Tag, Pos(PropName, UpperCase(Tag))+Length(PropName), Length(Tag));
      Result := Copy(Result, Pos('"', Result)+1, Length(Result));
      Result := Copy(Result, 1, Pos('"', Result)-1);
    end;
  end;
begin
  Result := '';
  I := Pos(FieldStr, Str);
  while I > 0 do
  begin
    Result := Result + Copy(Str, 1, I - 1);
    A := Copy(Str, I, Length(Str));
    J := Pos('>', A);
    if J > 0 then
      Delete(Str, 1, I + J - 1)
    else
      Str := '';
    FieldName := ExtractPropertyValue(A, 'FIELD');
    if Assigned(DataSet) and Dataset.Active then
    begin
      F := DataSet.FindField(FieldName);
      if F <> nil then
        Text := F.AsString
      else
        Text := Format('(%s)',[FieldName]);
    end
    else
      Text := Format('(%s)',[FieldName]);
    Result := Result + Text;
    I := Pos(FieldStr, Str);
  end;
  Result := Result + Str;
end;

//=== TJvDBHTLabel ===========================================================

constructor TJvDBHTLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  with FDataLink do
  begin
    Control := Self;
    OnDataChange := DataChange;
    OnEditingChange := DataChange;
    OnUpdateData := DataChange;
    OnActiveChange := DataChange;
  end;
end;

destructor TJvDBHTLabel.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TJvDBHTLabel.UpdateCaption;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.Dataset) then
    Caption := ReplaceFieldNameTag(FMask, FDataLink.DataSet)
  else
    Caption := ReplaceFieldNameTag(Mask, nil); 
end;

function TJvDBHTLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBHTLabel.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  UpdateCaption;
end;

procedure TJvDBHTLabel.DataChange(Sender: TObject);
begin
  UpdateCaption;
end;

procedure TJvDBHTLabel.SetMask(const Value: string);
begin
  if FMask <> Value then
  begin
    FMask := Value;
    UpdateCaption;
  end;
end;

end.

