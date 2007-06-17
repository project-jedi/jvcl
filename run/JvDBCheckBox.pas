unit JvDBCheckBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Controls, DB, DBCtrls;

type
  TJvDBCheckBoxChangingEvent = procedure(Sender: TObject; var Allow: Boolean) of object;

  TJvDBCheckBox = class(TDBCheckBox)
  private
    FDirectEdit: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TJvDBCheckBoxChangingEvent;
    FOrgDataChange: TNotifyEvent;
    FDataChanging: Integer;
    FToggling: Integer;
    FOldValue: Variant;

    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetDataLink: TFieldDataLink;
  protected
    procedure DataChange(Sender: TObject);

    procedure Toggle; override;
    function DoChanging: Boolean; virtual;
    procedure DoChange; virtual;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DirectEdit: Boolean read FDirectEdit write FDirectEdit default True;
    property OnChanging: TJvDBCheckBoxChangingEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TJvDBCheckBox }

constructor TJvDBCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirectEdit := True;
  FOrgDataChange := GetDataLink.OnDataChange;
  GetDataLink.OnDataChange := DataChange;
end;

procedure TJvDBCheckBox.CMExit(var Message: TCMExit);
begin
  if DirectEdit then
  begin
    if GetDataLink.Active and (Field <> nil) then
      FOldValue := Field.Value;
    DoExit // TWinControl behaviour
  end
  else
    inherited;
end;

procedure TJvDBCheckBox.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDBCheckBox.Toggle;
begin
  if FDataChanging = 0 then
  begin
    if not DoChanging then
      Exit;
  end;

  Inc(FToggling);
  try
    inherited Toggle;
    if DirectEdit and (FDataChanging = 0) and GetDataLink.Active and GetDataLink.Editing then
      GetDataLink.UpdateRecord;
  finally
    Dec(FToggling);
  end;

  if FDataChanging = 0 then
    DoChange;
end;

function TJvDBCheckBox.DoChanging: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);
end;

function TJvDBCheckBox.GetDataLink: TFieldDataLink;
begin
  Result := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
end;

procedure TJvDBCheckBox.KeyPress(var Key: Char);
begin
  case Key of
    #27:
      if (Field <> nil) and (DataSource <> nil) and (DataSource.State in [dsEdit, dsInsert]) then
        Field.Value := FOldValue;
  end;
  inherited KeyPress(Key);
end;

procedure TJvDBCheckBox.DataChange(Sender: TObject);
begin
  if (FToggling = 0) and DirectEdit and (Field <> nil) then
    FOldValue := Field.Value;

  Inc(FDataChanging);
  try
    FOrgDataChange(Sender);
  finally
    Dec(FDataChanging);
  end;
end;

end.

