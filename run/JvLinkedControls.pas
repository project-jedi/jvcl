unit JvLinkedControls;

interface
uses
  SysUtils, Classes, Controls;

type
  TJvLinkedControlsOption = (loLinkChecked, loLinkEnabled);
  TJvLinkedControlsOptions = set of TJvLinkedControlsOption;
  TJvLinkedControl = class(TCollectionItem)
  private
    FOwnerControl, FControl:TControl;
    FOptions: TJvLinkedControlsOptions;
    procedure SetControl(const Value: TControl);
    procedure SetOptions(const Value: TJvLinkedControlsOptions);
  public
    procedure Assign(Source:TPersistent);override;
    constructor Create(Collection: TCollection); override;
  published
    property Control:TControl read FControl write SetControl;
    property Options:TJvLinkedControlsOptions read FOptions write SetOptions default [loLinkChecked, loLinkEnabled];
  end;

  TJvLinkedControls = class(TOwnedCollection)
  private
    FControl:TControl;
    FOnChange: TNotifyEvent;
    function GetItems(Index: integer): TJvLinkedControl;
    procedure SetItems(Index: integer; const Value: TJvLinkedControl);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AControl:TControl);
    function Add:TJvLinkedControl;
    procedure Assign(Source:TPersistent);override;
    property Items[Index:integer]:TJvLinkedControl read GetItems write SetItems;default;
    property OnChange:TNotifyEvent read FOnChange write FOnCHange;
  end;

implementation

{ TJvLinkedControl }

procedure TJvLinkedControl.Assign(Source: TPersistent);
begin
  if (Source <> Self) and (Source is TJvLinkedControl) then
  begin
    Control := TJvLinkedControl(Source).Control;
    Options := TJvLinkedControl(Source).Options;
    Changed(false);
    Exit;
  end;
  inherited;
end;

constructor TJvLinkedControl.Create(Collection: TCollection);
begin
  inherited;
  FOptions := [loLInkChecked, loLinkEnabled]; 
end;

procedure TJvLinkedControl.SetControl(const Value: TControl);
begin
  if (FControl <> Value) then
  begin
    if (FControl = FOwnerControl) and (FOwnerControl <> nil) then
      raise Exception.Create('Cannot link to owner control');
    if Assigned(FControl) and Assigned(FOwnerControl) then
      FControl.RemoveFreeNotification(FOwnerControl);
    FControl := Value;
    if Assigned(FControl) and Assigned(FOwnerControl) then
      FControl.FreeNotification(FOwnerControl);
    Changed(false);
  end;
end;

procedure TJvLinkedControl.SetOptions(const Value: TJvLinkedControlsOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Changed(false);
  end;
end;

{ TJvLinkedControls }

function TJvLinkedControls.Add: TJvLinkedControl;
begin
  Result := TJvLinkedControl(inherited Add);
  Result.FOwnerControl := FControl;
end;

procedure TJvLinkedControls.Assign(Source: TPersistent);
begin
  if (Source <> Self) and (Source is TJvLinkedControls) then
  begin
    Clear;
    Exit;
  end;
  inherited;
end;

constructor TJvLinkedControls.Create(AControl: TControl);
begin
  inherited Create(AControl, TJvLinkedControl);
  FControl := AControl;
end;

function TJvLinkedControls.GetItems(Index: integer): TJvLinkedControl;
begin
  Result := TJvLinkedControl(inherited Items[Index]);
end;

procedure TJvLinkedControls.SetItems(Index: integer;
  const Value: TJvLinkedControl);
begin
  inherited Items[Index] := Value;
end;

procedure TJvLinkedControls.Update(Item: TCollectionItem);
begin
  inherited;
  if Item <> nil then
    TJvLinkedControl(Item).FOwnerControl := FControl;
  if Assigned(FOnChange) then FOnChange(Self);
end;

end.

