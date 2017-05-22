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
JVCL home page, located at http://jvcl.delphi-jedi.org

Known Issues:
- To display data from a datasource, use the <FIELD="fieldname"> tag in Mask.
- You can have more than one FIELD tag in a label, i.e:
  <b>Name:</b><i><FIELD="contact"></i>, <b>Company:</b><i><FIELD="Company"></i>
- The fieldname *must* be double-quoted!
-----------------------------------------------------------------------------}
// $Id$

unit JvDBHTLabel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, DB, DBCtrls, Controls,
  VDBConsts,
  JvHtControls;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBHTLabel = class(TJvCustomHTLabel)
  private
    FDataLink: TFieldDataLink;
    FMask: string;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure SetMask(const Value: string);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function GetLabelText: string; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
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
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
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
  SysUtils;

function ReplaceFieldNameTag(Str: string; DataSet: TDataSet): string;
var
  F: TField;
const
  FieldName = 'FIELD'; // non-standard html
  FieldStr = '<' + FieldName + '=';
  FieldLabelName = 'FIELDLABEL';
  FieldLabelStr = '<' + FieldLabelName + '=';

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

  function ExtractProperty(AStr: string; const PropName: string): string;
  var
    J: Integer;
    I: Integer;
    A, FieldName, Text: string;
    PropStr: string;
  begin
    Result := '';
    PropStr := '<'+PropName+'=';
    I := Pos(PropStr, AStr);
    while I > 0 do
    begin
      Result := Result + Copy(AStr, 1, I - 1);
      A := Copy(AStr, I, Length(AStr));
      J := Pos('>', A);
      if J > 0 then
        Delete(AStr, 1, I + J - 1)
      else
        AStr := '';
      FieldName := ExtractPropertyValue(A, PropStr);
      if Assigned(DataSet) and DataSet.Active then
      begin
        F := DataSet.FindField(FieldName);
        if F <> nil then
        begin
          if PropName = FieldLabelName then
            Text := F.DisplayLabel
          else
            Text := F.DisplayText;
        end
        else
          Text := Format('(%s)', [FieldName]);
      end
      else
        Text := Format('(%s)', [FieldName]);
      Result := Result + Text;
      I := Pos(PropStr, AStr);
    end;
    Result := Result + AStr;
  end;

begin
  Result := ExtractProperty(Str, FieldLabelName);
  Result := ExtractProperty(Result, FieldName);
end;

//=== { TJvDBHTLabel } =======================================================

procedure TJvDBHTLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

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
  if Assigned(FDataLink) and Assigned(FDataLink.DataSet) then
    Caption := ReplaceFieldNameTag(FMask, FDataLink.DataSet)
  else
    Caption := ReplaceFieldNameTag(Mask, nil);
end;

function TJvDBHTLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJvDBHTLabel.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
  begin
    if (Assigned(FDataLink) and Assigned(FDataLink.DataSet)) then
      Result := ReplaceFieldNameTag(FMask, FDataLink.DataSet)
    else
      Result := ReplaceFieldNameTag(Mask, nil);
  end
  else
    Result := Caption;
end;

procedure TJvDBHTLabel.Loaded;
begin
  inherited;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TJvDBHTLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBHTLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then DatabaseError(SDataSourceFixed);
    inherited;
  end;
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

