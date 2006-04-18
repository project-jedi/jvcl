unit D5CheckLst;

{$I ..\Common\installer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, CheckLst;

{$IFDEF COMPILER5}
type
  TCheckListBoxD5 = class(TCheckListBox)
  private
    FHeaders: TList;
    FHeaderColor: TColor;
    FHeaderBackgroundColor: TColor;
    function GetHeader(Index: Integer): Boolean;
    procedure SetHeader(Index: Integer; const Value: Boolean);
    procedure SetHeaderColor(Value: TColor);
    procedure SetHeaderBackgroundColor(Value: TColor);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Header[Index: Integer]: Boolean read GetHeader write SetHeader;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clInfoText;
    property HeaderBackgroundColor: TColor read FHeaderBackgroundColor write SetHeaderBackgroundColor default clInfoBk;
  end;

  TCheckListBox = TCheckListBoxD5;
{$ENDIF COMPILER5}

implementation

uses Compiler5MissingPropertyFix;

{$IFDEF COMPILER5}

function NewInstanceHook(AClass: TClass): TObject;
begin
  Result := TCheckListBoxD5.NewInstance;
end;

{ TCheckListBoxD5 }

constructor TCheckListBoxD5.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaders := TList.Create;
  FHeaderColor := clInfoText;
  FHeaderBackgroundColor := clInfoBk;
end;

destructor TCheckListBoxD5.Destroy;
begin
  FHeaders.Free;
  inherited Destroy;
end;

procedure TCheckListBoxD5.SetHeaderColor(Value: TColor);
begin
  if Value <> FHeaderColor then
  begin
    FHeaderColor := Value;
    Invalidate;
  end;
end;

procedure TCheckListBoxD5.SetHeaderBackgroundColor(Value: TColor);
begin
  if Value <> FHeaderBackgroundColor then
  begin
    FHeaderBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TCheckListBoxD5.SetHeader(Index: Integer; const Value: Boolean);
var
  Idx: Integer;
begin
  Idx := FHeaders.IndexOf(Pointer(Index));
  if Idx < 0 then
  begin
    if Value then
      FHeaders.Add(Pointer(Index));
  end
  else
    if not Value then
      FHeaders.Delete(Idx);
end;

function TCheckListBoxD5.GetHeader(Index: Integer): Boolean;
begin
  Result := FHeaders.IndexOf(Pointer(Index)) >= 0;
end;

procedure TCheckListBoxD5.CNDrawItem(var Message: TWMDrawItem);
begin
  if Items.Count = 0 then
    Exit;
  with Message.DrawItemStruct^ do
    if Header[itemID] then
      if not UseRightToLeftAlignment then
        rcItem.Left := rcItem.Left - GetCheckWidth
      else
        rcItem.Right := rcItem.Right + GetCheckWidth;
  inherited;
end;

procedure TCheckListBoxD5.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Header[Index] then
  begin
    Canvas.Font.Color := HeaderColor;
    Canvas.Brush.Color := HeaderBackgroundColor;
  end;
  inherited DrawItem(Index, Rect, State);
end;

initialization
  RegisterClass(TCheckListBoxD5);
  ReplaceVmtField(PVmt(CheckLst.TCheckListBox), vmtNewInstance, @NewInstanceHook);
{$ENDIF COMPILER5}

end.
