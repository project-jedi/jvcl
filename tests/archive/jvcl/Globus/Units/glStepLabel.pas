unit glStepLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TglStepLabel = class(TGraphicControl)
  private
    FStepCount: integer;
    FPassiveColor: TColor;
    FActiveColor: TColor;
    procedure SetStepCount(const Value: integer);
    { Private declarations }
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetActiveColor(const Value: TColor);
    procedure SetPassiveColor(const Value: TColor);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StepCount: integer read FStepCount write SetStepCount default 4;
    property ActiveColor: TColor default clWindowText read FActiveColor write SetActiveColor;
    property PassiveColor: TColor default clSilver read FPassiveColor write SetPassiveColor;
    property Font;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gl Controls', [TglStepLabel]);
end;

{ TglStepLabel }

constructor TglStepLabel.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TglStepLabel.Destroy;
begin
  inherited;
  FStepCount := 4;
  FActiveColor := clWindowText;
  FPassiveColor := clSilver;
end;

procedure TglStepLabel.SetActiveColor(const Value: TColor);
begin
  FActiveColor := Value;
end;

procedure TglStepLabel.SetPassiveColor(const Value: TColor);
begin
  FPassiveColor := Value;
end;

procedure TglStepLabel.SetStepCount(const Value: integer);
begin
  if Value < 1 then exit;
  FStepCount := Value;
end;

procedure TglStepLabel.WMPaint(var Message: TWMPaint);
var
  Caption: string;
  i: integer;
begin
  StepCount
end;

end.
 