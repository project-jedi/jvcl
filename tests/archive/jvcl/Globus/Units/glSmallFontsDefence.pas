{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	 Copyright (c) 1998-2001 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glSmallFontsDefence; Unit 01.2001    component TglSmallFontsDefence;
 Component prevents your apps from BIG fonts.
 ===================================================================
}
unit glSmallFontsDefence;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, grids;

type
  TglSmallFontsDefenceOptions_ = (fdoExcludeGrids);
  TglSmallFontsDefenceOptions = set of TglSmallFontsDefenceOptions_;

  TglSmallFontsDefence = class(TComponent)
  private
    FOptions: TglSmallFontsDefenceOptions;
    procedure UpdateFonts(Control: TWinControl);
    procedure SetOptions(const Value: TglSmallFontsDefenceOptions);
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TglSmallFontsDefenceOptions read FOptions write SetOptions;
  end;

procedure Register;

implementation
uses glUtils, glTypes;

procedure Register;
begin
  RegisterComponents('Gl Components', [TglSmallFontsDefence]);
end;

{ TglSmallFontsDefence }

constructor TglSmallFontsDefence.Create(AOwner: TComponent);
begin
  inherited;
  if (Owner is TForm) then (Owner as TForm).Scaled := false;
end;

procedure TglSmallFontsDefence.Loaded;
begin
  inherited;
  if (Owner is TForm) then (Owner as TForm).Scaled := false;
  if csDesigning in ComponentState then
  begin
    if not IsSmallFonts then
      ShowMessage('Проектирование приложения в режиме крупных шрифтов недопустимо!'#13#10'Компонент TglSmallFontsDefence отказывается работать в таких условиях.');
  end else
    UpdateFonts((Owner as TForm));
end;

procedure TglSmallFontsDefence.SetOptions(const Value: TglSmallFontsDefenceOptions);
begin
  FOptions := Value;
end;

procedure TglSmallFontsDefence.UpdateFonts(Control: TWinControl);
var
  i: integer;
  procedure UpdateFont(Font: TFont);
  begin
    if CompareText(Font.Name, 'MS Sans Serif') <> 0 then exit;
    Font.Name := 'Arial';
  end;
begin
  if IsSmallFonts then exit;
  if (fdoExcludeGrids in Options) and (Control is TCustomGrid) then exit;
  UpdateFont(TShowFont(Control).Font);
  with Control do
  for i:=0 to ControlCount-1 do
  begin
    UpdateFont(TShowFont(Controls[i]).Font);
    if Controls[i] is TWinControl then UpdateFonts(Controls[i] as TWinControl);
  end;

end;


end.
