{$I jvcl.inc}
unit JvDBFindEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, DB, DBCtrls,
{$IFDEF USEJVCL}
  CheckLst,
  JVCLVer;
{$ELSE}
  CheckLst;
{$ENDIF USEJVCL}

type
  TJvEditFindStyle = (fsNavigate, fsFilter);
  TJvEditFindMode = (fmFirstPos, fmAnyPos);

  TJvDBFindEdit = class(TCustomEdit)
  private
{$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
{$ENDIF USEJVCL}
    FTimer: TTimer;
    FOldFiltered: Boolean;
    FOldFilterRecord: TFilterRecordEvent;
    FDataLink: TFieldDataLink;
    FIgnoreCase: Boolean;
    FFindMode: TJvEditFindMode;
    FFindStyle: TJvEditFindStyle;
    FSearchText: string;
    procedure ActiveChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetFindMode(const Value: TJvEditFindMode);
    procedure SetFindStyle(const Value: TJvEditFindStyle);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure FTimerTimer(Sender: TObject);
    procedure AFilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Find(AText: string);

  published
{$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
{$ENDIF USEJVCL}
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FindStyle: TJvEditFindStyle read FFindStyle write SetFindStyle default fsNavigate;
    property FindMode: TJvEditFindMode read FFindMode write SetFindMode default fmFirstPos;
    property IgnoreCase: Boolean read FIgnoreCase write SetIgnoreCase default true;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation
{ TJvDBFindEdit }

constructor TJvDBFindEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFindStyle := fsNavigate;
  FFindMode := fmFirstPos;
  FIgnoreCase := true;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.Interval := 400;
  FTimer.OnTimer := FTimerTimer;
  FSearchText := '';
  FOldFiltered := false;
  FOldFilterRecord := nil;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TJvDBFindEdit.Destroy;
begin
  if FDataLink.Active and (FFindStyle = fsFilter) then
  begin
    FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
    FDataLink.DataSet.Filtered := FOldFiltered;
  end;
  FDataLink.Control := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBFindEdit.Change;
begin
  FTimer.Enabled := false;
  if Text = '' then
    FTimer.Interval := 400;
  FTimer.Enabled := true;
  FSearchText := Text;
  inherited;
end;

procedure TJvDBFindEdit.FTimerTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  ActiveChange(Self);
  if FSearchText = '' then
    if FFindStyle = fsFilter then
    begin
      FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
      FDataLink.DataSet.Filtered := FOldFiltered;
    end
    else
  else
  begin
    if not FDataLink.Active or (FDataLink.Field = nil) then Exit;
    if FFindStyle = fsNavigate then
      if IgnoreCase then
        FDataLink.DataSet.Locate(DataField, FSearchText, [loCaseInsensitive, loPartialKey])
      else
        FDataLink.DataSet.Locate(DataField, FSearchText, [loPartialKey])
    else
      FDataLink.DataSet.Filtered := true;
  end;
  FTimer.Interval := 100;
end;

procedure TJvDBFindEdit.Find(AText: string);
begin
  FSearchText := AText;
  FTimerTimer(FTimer);
end;

procedure TJvDBFindEdit.AFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := true;
  if FOldFiltered and Assigned(FOldFilterRecord) then
    FOldFilterRecord(DataSet, Accept);
  if not Accept then Exit;
  if FFindMode = fmFirstPos then
    if IgnoreCase then
      Accept := Pos(AnsiUpperCase(FSearchText),
        AnsiUpperCase(DataSet.FieldByName(DataField).AsString)) = 1
    else
      Accept := Pos(FSearchText, DataSet.FieldByName(DataField).AsString) = 1
  else if IgnoreCase then
    Accept := Pos(AnsiUpperCase(FSearchText),
      AnsiUpperCase(DataSet.FieldByName(DataField).AsString)) > 0
  else
    Accept := Pos(FSearchText, DataSet.FieldByName(DataField).AsString) > 0
end;

procedure TJvDBFindEdit.ActiveChange(Sender: TObject);
var
  Func1, Func2: TFilterRecordEvent;
begin
  if (FFindStyle = fsNavigate) or (FDataLink.DataSet = nil) then Exit;
  Func1 := FDataLink.DataSet.OnFilterRecord;
  Func2 := AFilterRecord;
  if FDataLink.Active and (@Func1 <> @Func2) and (FSearchText > '') then
  begin
    FOldFilterRecord := FDataLink.DataSet.OnFilterRecord;
    FOldFiltered := FDataLink.DataSet.Filtered;
    FDataLink.DataSet.OnFilterRecord := AFilterRecord;
  end;
end;

function TJvDBFindEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBFindEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TJvDBFindEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBFindEdit.SetDataField(const Value: string);
begin
  if Value > '' then
    FDataLink.FieldName := Value;
end;

procedure TJvDBFindEdit.SetFindMode(const Value: TJvEditFindMode);
begin
  if FFindStyle = fsNavigate then
    FFindMode := fmFirstPos
  else
    FFindMode := Value;
end;

procedure TJvDBFindEdit.SetFindStyle(const Value: TJvEditFindStyle);
begin
  FFindStyle := Value;
  if FFindStyle = fsNavigate then FFindMode := fmFirstPos;
  ActiveChange(Self);
end;

procedure TJvDBFindEdit.SetIgnoreCase(const Value: Boolean);
begin
  FIgnoreCase := Value;
end;

procedure TJvDBFindEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
    begin
      DataSource := nil;
    end;
  end;
end;

end.

